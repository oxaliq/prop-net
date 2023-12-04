(ns main)


;; naive implementation: global state for scheduler
(def  alerted-propagators  (atom #{}))
(def  abort-process  (atom false))
(def  last-value-of-run  (atom :done))
;; can't remember why this is here?
;; possibly for tracking 
(def  propagators-ever-alerted  (atom #{}))

(defn clear-alerted-propagators! []
  (reset!  alerted-propagators  #{}))

(defn initialize-scheduler []
  (clear-alerted-propagators!)
  (reset!  abort-process  false)
  (reset!  last-value-of-run  :done)
  (reset!  propagators-ever-alerted  #{})
  :ok)

(defn any-propagators-alerted? []
  (not (empty? @alerted-propagators)))

(defn alert-propagators [propagators]
  (run! (fn [propagator]
          (if (not (fn? propagator))
            (throw (ex-info "Alerting a non-procedure" #{:propagator propagator})))
          (swap! propagators-ever-alerted #(conj % propagator))
          (swap! alerted-propagators #(conj % propagator)))
        propagators)
  false) ; this is just copied from the paper, but why return false???

;; single argument -> 1-tuple
(defn alert-propagator [p] (alert-propagators [p]))

(defn alert-all-propagators! []
  (apply alert-propagators propagators-ever-alerted))


;; ignoring process abortion semantics for now

;; good candidate for "tco" with recur?
(defn run-alerted []
  (let [tmp @alerted-propagators]
    (clear-alerted-propagators!)
    ;; run all propagators
    (run! #(apply % []) tmp)
    (if (any-propagators-alerted?)
      (run-alerted)
      'done)))

(defn run []
  (if (any-propagators-alerted?)
    (reset! last-value-of-run (run-alerted)))
  @last-value-of-run)


(def nothing nil)
(defn nothing? [thing]
  (= thing nothing))

(defn content [cell]
  (cell 'content))
(defn add-content [cell increment]
  ((cell 'add-content) increment))
(defn new-neighbor! [cell neighbor]
  ((cell 'new-neighbor!) neighbor))


(defn make-cell []
  (let [neighbors (atom #{})
        cntnt (atom nothing)
        add-content* (fn [increment]
                       (println "adding content")
                       (println increment)
                       (println @cntnt)
                       (cond (nothing? increment) 'ok
                             (nothing? @cntnt) (do (reset! cntnt increment)
                                                   (apply alert-propagators (vector @neighbors)))
                             :else (if (not (= @cntnt increment))
                                     (throw (ex-info "Inconsistency!"
                                                     {:content @cntnt
                                                      :increment increment})))))
        new-neighbor*! (fn [new-neighbor]
                         (if (not (contains? @neighbors new-neighbor))
                           (do
                             (swap! neighbors #(conj % new-neighbor))
                             (alert-propagator new-neighbor))))
        closure (fn [message]
                  (cond (= message 'content) @cntnt
                        (= message 'add-content) add-content*
                        (= message 'new-neighbor!) new-neighbor*!
                        :else (throw (ex-info "Unknown message" {:msg message}))))]
    closure))

(defn propagator [neighbors to-do]
  (run! #(new-neighbor! % to-do) neighbors)
  (alert-propagator to-do))

(defn function->propagator-constructor [f]
  (fn [cells]
    ;; assumes output cell is last and that cells is a vector
    (let [output (peek cells)
          inputs (pop cells)
          res (apply f inputs)]
      (propagator inputs
                  (fn []
                    (add-content output
                                 res))))))

(defn handle-nothings [f]
  (fn [& args]
    ;; only apply the function if every input has some data
    (if (empty? (filterv nothing? (map #(content %) args)))
      (apply f (map #(content %) args))
      nothing)))


(def adder (function->propagator-constructor (handle-nothings +)))
(def subtractor (function->propagator-constructor (handle-nothings -)))
(def multiplier (function->propagator-constructor (handle-nothings *)))
(def divider (function->propagator-constructor (handle-nothings /)))


(defn constant [value]
  (function->propagator-constructor (fn [] value)))

(defn sum [x y total]
  (adder [x y total])
  (subtractor [total x y])
  (subtractor [total y x])
  (run))

(defn product [x y total]
  (multiplier [x y total])
  (divider [total x y])
  (divider [total y x])
  (run))

;; the current issue is now laziness!

;; (defn fahrenheit-celsius [f c]
;;   (let [thirty-two (make-cell)
;;         f-32 (make-cell)
;;         five (make-cell)
;;         c*9 (make-cell)
;;         nine (make-cell)]
;;     ((constant 32) [thirty-two])
;;     ((constant 5) [five])
;;     ((constant 9) [nine])
;;     (sum thirty-two f-32 f)
;;     (product f-32 five c*9)
;;     (product c nine c*9)))

;; working globally to more easily inspect
(def f (make-cell))
(def c (make-cell))
(def thirty-two (make-cell))
(def f-32 (make-cell))
(def five (make-cell))
(def c*9 (make-cell))
(def nine (make-cell))
((constant 32) [thirty-two])
((constant 5) [five])
((constant 9) [nine])
;; if a value is added to f or c before creating these propagators,
;; then the system will quiesce around the solution
(sum thirty-two f-32 f)
(product f-32 five c*9)
(product c nine c*9)
;; if a value is added to f or c after creating these propagators,
;; then running the system will never propagate a value throughout
