(ns rooten.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [to-chan timeout close! sub <! >! alts! pipe pub map< filter< buffer chan timeout put!]]
            [cljs.core.match]
            [rooten.jquery :refer [$ add-class remove-class trigger]])
  (:require-macros
            [cljs.core.async.macros :as m :refer [go alt! go-loop]]
            [cljs.core.match.macros :refer [match]]))

;; rou·tine (ro͞oˈtēn)
;; noun: routine; plural noun: routines
;; 1. a sequence of actions regularly followed; a fixed program.

#_(enable-console-print!)

(def app-state (atom {:exercises ["jumping jacks"
                                  "wall sit"
                                  "push ups"
                                  "abdominal crunches"
                                  "step up onto chair"
                                  "squats"
                                  "tricep dips on chair"
                                  "plank"
                                  "high knees running in place"
                                  "lunges"
                                  "push up and rotation"
                                  "left side plank"
                                  "right side plank"]}))

(defn say
  ([message] (say message "en"))
  ([message language]
    (let [msg (js/SpeechSynthesisUtterance. message)]
      (set! (.-lang msg) language)
      (.speak js/speechSynthesis msg))))

(defn play-bell [] (-> "Glass.wav" js/Audio. .play))

(defn seconds [n] (* n 1000))
(defn now [] (-> (js/Date.) .getTime))

;; macro???
(defn create-step [step]
  (let [steps (list [(seconds 1) (delay (say step))]
                    [(seconds 4) (delay (play-bell))]
                    [(seconds 30) (delay (play-bell))]
                    [(seconds 4) (delay ())])
        commands (chan)
        events (chan)]
    (go
      (loop [started? false steps1 steps time-started (now)]
        (let [[pause action] (first steps1)]
          (alt!
            commands
              ([c]
                (match [started? c]
                  [false :start] (recur true steps1 (now))
                  [true  :stop]  (recur false (conj (next steps1) [(- pause (- (now) time-started)) action] ) nil)
                  [_        nil] (close! events)
                  :else          (recur started? steps1 time-started)))
              (if started? (timeout pause) (chan))
                ([_]
                  (do @action (if (empty? (next steps1)) (do (close! commands) (close! events)) (recur started? (next steps1) (now)))))
            :priority true))))
    [commands, events]))

(defn next-step [[set-num step-num] num-sets num-steps]
  (let [i (-> set-num (* num-steps) (+ step-num) inc)
        next-set (.floor js/Math (/ i num-steps))]
    (when (and (>= next-set 0) (< next-set num-sets)) [next-set (mod i num-steps)])))

(defn prev-step [[set-num step-num] num-sets num-steps]
  (let [i (-> set-num (* num-steps) (+ step-num) dec)
        next-set (.floor js/Math (/ i num-steps))]
    (when (and (>= next-set 0) (< next-set num-sets)) [next-set (mod i num-steps)])))

(defn routine [commands]
  (let [steps (:exercises @app-state)
        num-sets 3
        num-steps (count steps)
        events (chan)]
    (go
      (loop [started? false current [[0 0] (create-step (first steps))] timer [0 0]]
        (let [[step-index [step-commands step-events]] current
              [set-num step-num] step-index]
          (alt!
            commands
              ([c]
                (match [started? c]
                  [false :start]
                    (let [[start total] timer]
                      (>! step-commands :start)
                      (>! events [:started [set-num step-num]])
                      (recur true current [(now) total]))
                  [true :stop]
                    (let [[start total] timer]
                      (>! step-commands :stop)
                      (>! events [:stopped])
                      (recur false current [0 (+ total (- (now) start))]))
                  [_ :next]
                    (let [[next-set-num next-step-num :as next-index] (next-step step-index num-sets num-steps)]
                      (if next-index
                        ; ### fn move: can recur not be in a function???
                        (let [[next-commands next-events :as next1] (create-step (get steps next-step-num))]
                          (close! step-commands)
                          (when started? (>! next-commands :start))
                          (>! events [:changed next-index])
                          (recur started? [next-index next1] [(if started? (now) 0) 0]))
                        (recur started? current timer))) ; fn ignore
                  [_ :previous]
                    (let [[prev-set-num prev-step-num :as prev-index] (prev-step step-index num-sets num-steps)
                          [start total] timer
                          current-time (+ total (if started? (- (now) start) 0))
                          next-index (if (and prev-index (< current-time (seconds 3))) prev-index [set-num step-num])]
                      ; ### fn move
                      (let [[next-commands next-events :as next1] (create-step (get steps (second next-index)))]
                        (close! step-commands)
                        (when started? (>! next-commands :start))
                        (>! events [:changed next-index])
                        (recur started? [next-index next1] [(if started? (now) 0) 0])))
                  [_ :reset]
                    (do (close! step-commands)
                        (>! events [:stopped])
                        (>! events [:changed [0 0]])
                        (recur false [[0 0] (create-step (first steps))] [0 0]))
                  :else (recur started? current timer))) ; fn ignore
             step-events
               ([e]
                 (case
                   e nil
                   (let [next-index (next-step step-index num-sets num-steps)]
                     (if next-index
                       (do (>! commands :next) (recur started? current timer))
                       (recur false [[0 0] (create-step (first steps))] [0 0])))))
             :priority true)
           )))
    (pub events (fn [e] "1"))))


#_(say "Kevy, Tu est très, très sexy et musclé" "fr")

(def shortcuts
  {37 :previous
   39 :next
   32 :toggle})

(defn key-events [shortcuts]
  (let [key->command #(get shortcuts (.-keyCode %) :not-found)
        events (chan)]
    (set! (.-onkeydown js/window) (fn [e] (put! events [:keydown e])))
    (set! (.-onkeyup   js/window) (fn [e] (put! events [:keyup   e])))
    (->> events (map< (fn [[updown e]] [updown (key->command e)])) (filter< (fn [[updown e]] (not= e :not-found))))))

(defn control [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [keyevents (key-events shortcuts)]
        (go (loop []
          (let [[updown command] (<! keyevents)
                started? (om/get-state owner :started?)
                ownerNode (.getDOMNode owner)
                btn (case command
                      :toggle   ($ (if-not started? ".btn-start" ".btn-stop") ownerNode)
                      :previous ($ ".btn-prev" ownerNode)
                      :next     ($ ".btn-next" ownerNode))]
            (if (= updown :keydown)
              (add-class btn "active")
              (do (trigger btn "click") (remove-class btn "active"))))
            (recur)))))
    om/IRenderState
      (render-state [_ {:keys [commands started?]}]
        (dom/div #js {:className "btn-group btn-group-lg"}
          (dom/button #js {:title "Previous" :data-toggle "tooltip" :className "btn-prev btn btn-default glyphicon glyphicon-backward" :onClick (fn [e] (put! commands :previous))})
          (if-not started?
            (dom/button #js {:title "Start"  :data-toggle "tooltip" :className "btn-start btn btn-default glyphicon glyphicon-play"    :onClick (fn [e] (do (om/set-state! owner :started? true) (put! commands :start)))})
            (dom/button #js {:title "Stop"   :data-toggle "tooltip" :className "btn-stop btn btn-default glyphicon glyphicon-stop"     :onClick (fn [e] (do (om/set-state! owner :started? false) (put! commands :stop)))}))
          (dom/button #js {:title "Next"     :data-toggle "tooltip" :className "btn-next btn btn-default glyphicon glyphicon-forward"  :onClick (fn [e] (put! commands :next))})
          (dom/button #js {:title "Reset"    :data-toggle "tooltip" :className "btn-reset btn btn-default glyphicon glyphicon-repeat"  :onClick (fn [e] (put! commands :reset))})))))

(defn exercises-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [commands (chan 1)
            events-pub (routine commands)
            events-sub (chan)]
        (sub events-pub "1" events-sub)
        {:commands commands :events events-sub :started? false :current [0 0]}))
    om/IWillMount
    (will-mount [_]
      (let [commands (om/get-state owner :commands)
            events   (om/get-state owner :events)]
        (go (loop []
          (let [e (<! events)]
            (match e
              [:started step-index] (do (om/set-state! owner :started? true) (om/set-state! owner :current step-index))
              [:changed step-index] (om/set-state! owner :current step-index)
              [:stopped] (om/set-state! owner :started? false))
            (recur))))))
    om/IRenderState
    (render-state [this {:keys [commands started? current]}]
      (let [[set-num step-num] current]
        (dom/div nil
          (apply dom/table #js {:className "table table-hover"}
            (map-indexed #(dom/tr #js {:className (when (= %1 step-num) "info")} (dom/td nil %2)) (:exercises app)))
          (om/build control app {:init-state {:commands commands} :state {:started? started?}}))))))

(om/root exercises-view app-state
  {:target (. js/document (getElementById "app"))})
