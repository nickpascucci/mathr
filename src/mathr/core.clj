(ns mathr.core
  (:gen-class))

(def num-questions 10)

;; Help text shown at the beginning of the program execution.
(def intro-text
  (str "Welcome to Mathr! This program is designed to test your ability to do mental calculation.\n"
       "We'll present you with a series of problems. Enter the answer as quickly as you can.\n"
       "At the end, your total time, average time, and the percent of correct answers will be shown.\n"
       "When you're ready, press Enter to begin."))

(defn do-intro []
  "Display the intro text and wait for the user to press Enter."
  (println intro-text)
  (read-line))

;; Completion stats.
(def correct (atom 0))
(def incorrect (atom 0))
(def completion-times (atom []))

(defn pick-randomly! [choices]
  "Pick a random element from choices."
  (nth choices (rand-int (count choices))))

(defn op-to-sym [operation]
  "Convert a math operation to the human-friendly symbol."
  ;; TODO Make this print the readable form. There has to be a way, right?
  (case operation
    + "+"
    - "-"
    * "x"
    / "/"
    "Unknown symbol"))

(defn pick-first [operation]
  (let [val (rand-int 10)]
   (if (and (= / operation) 
            (or (= 0 val) (= 5 val) (= 7 val))) 
    (+ 1 val)
    val)))

(defn random-divisor [numerator]
  (let [candidate (atom (rand-int (+ 1 numerator)))]
    (while (or (= 0 @candidate) 
               (not (integer? (/ numerator @candidate))))
      (swap! candidate (fn [_] (rand-int (+ 1 numerator)))))
    @candidate))

(defn pick-second [operation first-val]
  (if (= '/ operation)
    (random-divisor first-val)
    (rand-int 11)))

(defn generate-question []
  "Generate a new question including values and operation."
  (let [operation (pick-randomly! ['+ '- '* '/])
        symbol (op-to-sym operation)
        first-val (pick-first operation)
        second-val (pick-second operation first-val)]
    ;; TODO Make it so that the division always has an integer answer and that we never divide by 0.
   {:operation @(resolve operation) :symbol symbol :first-val first-val :second-val second-val}))

(defn read-int []
  "Read an int from stdin and prompt the user if the input doesn't parse."
  (let [answer (atom nil)]
    (while (nil? @answer)
     (try
       (swap! answer (fn [_] (Integer/parseInt (.trim (read-line)))))
       (catch NumberFormatException e
         (println "Couldn't grok that, try again: "))))
    @answer))

(defn ask-question [question]
  "Ask a question and return whether the answer was correct."
  (let [{:keys [operation symbol first-val second-val]} question
        answer (operation first-val second-val)]
    (println (str "\t" first-val " " symbol " " second-val " = ?"))
    ;; TODO Make input more robust - empty string crashes this.
    (= answer (read-int))))

(defn run-questions! []
  "Ask num-questions questions and store the results."
  ;; Iterate through from 0 to num-questions
  ;; Generate a new question, ask it, and then store the results
  (dotimes [i num-questions]
   (let [start-time (System/currentTimeMillis)
         is-correct (ask-question (generate-question))]
     (swap! (if is-correct correct incorrect) inc)
     (swap! completion-times conj (- (System/currentTimeMillis) start-time))
     )))

(defn display-stats []
  "Print the statistics from the questioning run."
  (println "Correct: " @correct)
  (println "Incorrect: " @incorrect)
  (println "Average time:" (int (/ (apply + @completion-times) (count @completion-times))) "ms")
  )

(defn -main [& args]
  "Ask a series of questions."
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (do-intro)
  (run-questions!)
  (display-stats))
