(ns game.macros)

(defmacro effect [& expr]
  `(fn ~['state 'side 'eid 'card 'targets]
     ~(let [actions (map #(if (#{:challenger :contestant} (second %))
                            (concat [(first %) 'state (second %)] (drop 2 %))
                            (concat [(first %) 'state 'side] (rest %)))
                         expr)]
        `(let ~['challenger '(:challenger @state)
                'contestant '(:contestant @state)
                'contestant-reg '(get-in @state [:contestant :register])
                'contestant-reg-last '(get-in @state [:contestant :register-last-turn])
                'challenger-reg '(get-in @state [:challenger :register])
                'challenger-reg-last '(get-in @state [:challenger :register-last-turn])
                'run-server '(when (:run @state)
                               (get-in @state (concat [:contestant :servers] (:server (:run @state)))))
                'run-ices '(:ices run-server)
                'current-ice '(when-let [run-pos (:position (:run @state))]
                                (when (and (pos? run-pos) (<= run-pos (count (:ices run-server))))
                                  (nth (:ices run-server) (dec run-pos))))
                'target '(first targets)]
           ~@actions))))

(defmacro req [& expr]
  `(fn ~['state 'side 'eid 'card 'targets]
     (let ~['challenger '(:challenger @state)
            'contestant '(:contestant @state)
            'run '(:run @state)
            'run-server '(when (:run @state)
                           (get-in @state (concat [:contestant :servers] (:server (:run @state)))))
            'run-ices '(:ices run-server)
            'current-ice '(when-let [run-pos (:position (:run @state))]
                            (when (and (pos? run-pos) (<= run-pos (count (:ices run-server))))
                              (nth (:ices run-server) (dec run-pos))))
            'contestant-reg '(get-in @state [:contestant :register])
            'contestant-reg-last '(get-in @state [:contestant :register-last-turn])
            'challenger-reg '(get-in @state [:challenger :register])
            'challenger-reg-last '(get-in @state [:challenger :register-last-turn])
            'target '(first targets)
            'installed '(#{:rig :servers} (first (:zone (get-nested-host card))))
            'remotes '(get-remote-names @state)
            'servers '(zones->sorted-names (get-zones @state))
            'unprotected '(let [server (second (:zone (if (:host card)
                                                        (get-card state (:host card)) card)))]
                            (empty? (get-in @state [:contestant :servers server :ices])))
            'runnable-servers '(zones->sorted-names (get-runnable-zones @state))
            'hq-runnable '(not (:hq (get-in challenger [:register :cannot-run-on-server])))
            'rd-runnable '(not (:rd (get-in challenger [:register :cannot-run-on-server])))
            'archives-runnable '(not (:archives (get-in challenger [:register :cannot-run-on-server])))
            'tagged '(or (> (:tagged challenger) 0) (> (:tag challenger) 0))
            'has-bad-pub '(or (> (:bad-publicity contestant) 0) (> (:has-bad-pub contestant) 0))
            'this-server '(let [s (-> card :zone rest butlast)
                                r (:server run)]
                            (and (= (first r) (first s))
                                 (= (last r) (last s))))]
        ~@expr)))

(defmacro msg [& expr]
  `(fn ~['state 'side 'eid 'card 'targets]
     (let ~['challenger '(:challenger @state)
            'contestant '(:contestant @state)
            'contestant-reg '(get-in @state [:contestant :register])
            'contestant-reg-last '(get-in @state [:contestant :register-last-turn])
            'challenger-reg '(get-in @state [:challenger :register])
            'challenger-reg-last '(get-in @state [:challenger :register-last-turn])
            'run '(:run @state)
            'run-server '(when (:run @state)
                           (get-in @state (concat [:contestant :servers] (:server (:run @state)))))
            'run-ices '(:ices run-server)
            'current-ice '(when-let [run-pos (:position (:run @state))]
                            (when (and (pos? run-pos) (<= run-pos (count (:ices run-server))))
                              (nth (:ices run-server) (dec run-pos))))
            'target '(first targets)
            'tagged '(or (> (:tagged challenger) 0) (> (:tag challenger) 0))]
       (str ~@expr))))

(defmacro when-completed
  ([action expr]
   (let [reqmac `(fn [~'state1 ~'side1 ~'eid1 ~'card1 ~'target1]
                   (let [~'async-result (:result ~'eid1)]
                     ~expr))
   ;; this creates a five-argument function to be resolved later,
   ;; without overriding any local variables name state, card, etc.
         totake (if (= 'apply (first action)) 4 3)
         th (nth action totake)]
     `(let [~'use-eid (and (map? ~th) (:eid ~th))
            ~'new-eid (if ~'use-eid ~th (game.core/make-eid ~'state))]
        (~'register-effect-completed ~'state ~'side ~'new-eid ~(if (resolve 'card) ~'card nil) ~reqmac)
        (if ~'use-eid
          ~(concat (take totake action) (list 'new-eid) (drop (inc totake) action))
          ~(concat (take totake action) (list 'new-eid) (drop totake action)))))))

(defmacro final-effect [& expr]
  (macroexpand (apply list `(effect ~@expr ~(list (quote effect-completed) 'eid 'card)))))

(defmacro continue-ability
  [state side ability card targets]
  `(game.core/resolve-ability ~state ~side (assoc ~ability :eid ~'eid) ~card ~targets))
