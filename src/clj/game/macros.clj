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
                'run-locale '(when (:run @state)
                               (get-in @state (concat [:contestant :locales] (:locale (:run @state)))))
                'run-characters '(:characters run-locale)
                'current-character '(when-let [run-pos (:position (:run @state))]
                                (when (and (pos? run-pos) (<= run-pos (count (:characters run-locale))))
                                  (nth (:characters run-locale) (dec run-pos))))
                'target '(first targets)
                'played '(if (= (:side card) "Contestant") (:contestant @state) (:challenger @state))
                'versus '(if (= (:side card) "Contestant") (:challenger @state) (:contestant @state))]
           ~@actions))))

(defmacro req [& expr]
  `(fn ~['state 'side 'eid 'card 'targets]
     (let ~['challenger '(:challenger @state)
            'contestant '(:contestant @state)
            'run '(:run @state)
            'run-locale '(when (:run @state)
                           (get-in @state (concat [:contestant :locales] (:locale (:run @state)))))
            'run-characters '(:characters run-locale)
            'current-character '(when-let [run-pos (:position (:run @state))]
                            (when (and (pos? run-pos) (<= run-pos (count (:characters run-locale))))
                              (nth (:characters run-locale) (dec run-pos))))
            'contestant-reg '(get-in @state [:contestant :register])
            'contestant-reg-last '(get-in @state [:contestant :register-last-turn])
            'challenger-reg '(get-in @state [:challenger :register])
            'challenger-reg-last '(get-in @state [:challenger :register-last-turn])
            'target '(first targets)
            'placed '(#{:rig :locales} (first (:zone (get-nested-host card))))
            'parties '(get-party-names state)
            'locales '(zones->sorted-names (get-zones state))
            'unprotected '(let [locale (second (:zone (if (:host card)
                                                        (get-card state (:host card)) card)))]
                            (empty? (get-in @state [:contestant :locales locale :characters])))
            'runnable-locales '(zones->sorted-names (get-runnable-zones state))
            'hq-runnable '(not (:hq (get-in challenger [:register :cannot-run-on-locale])))
            'rd-runnable '(not (:rd (get-in challenger [:register :cannot-run-on-locale])))
            'archives-runnable '(not (:archives (get-in challenger [:register :cannot-run-on-locale])))
            'tagged '(or (pos? (:tagged challenger)) (pos? (:tag challenger)))
            'has-bad-pub '(or (pos? (:bad-publicity contestant)) (pos? (:has-bad-pub contestant)))
            'this-locale '(let [s (-> card :zone rest butlast)
                                r (:locale run)]
                            (and (= (first r) (first s))
                                 (= (last r) (last s))))
            'played '(if (= (:side card) "Contestant") (:contestant @state) (:challenger @state))
            'versus '(if (= (:side card) "Contestant") (:challenger @state) (:contestant @state))
            'served '(zones->sorted-names
                       (if (= (:side card) "Contestant") (get-zones state) (get-zones-challenger state)))]
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
            'run-locale '(when (:run @state)
                           (get-in @state (concat [:contestant :locales] (:locale (:run @state)))))
            'run-characters '(:characters run-locale)
            'current-character '(when-let [run-pos (:position (:run @state))]
                            (when (and (pos? run-pos) (<= run-pos (count (:characters run-locale))))
                              (nth (:characters run-locale) (dec run-pos))))
            'target '(first targets)
            'tagged '(or (pos? (:tagged challenger)) (pos? (:tag challenger)))
            'played '(if (= (:side card) "Contestant") (:contestant @state) (:challenger @state))
            'versus '(if (= (:side card) "Contestant") (:challenger @state) (:contestant @state))]
       (str ~@expr))))

(defmacro wait-for
  ([action & expr]
   (let [reqmac `(fn [~'state1 ~'side1 ~'eid1 ~'card1 ~'target1]
                   (let [~'async-result (:result ~'eid1)]
                     ~@expr))
   ;; this creates a five-argument function to be resolved later,
   ;; without overriding any local variables name state, card, etc.
         totake (if (= 'apply (first action)) 4 3)
         th (nth action totake)]
     `(let [~'use-eid (and (map? ~th) (:eid ~th))
            ~'new-eid (if ~'use-eid ~th (game.core/make-eid ~'state))]
        (~'register-effect-completed ~'state ~'side ~'new-eid ~(when (resolve 'card) ~'card) ~reqmac)
        (if ~'use-eid
          ~(concat (take totake action) (list 'new-eid) (drop (inc totake) action))
          ~(concat (take totake action) (list 'new-eid) (drop totake action)))))))

(defmacro continue-ability
  [state side ability card targets]
  `(game.core/resolve-ability ~state ~side (assoc ~ability :eid ~'eid) ~card ~targets))
