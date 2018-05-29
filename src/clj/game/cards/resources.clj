(in-ns 'game.core)

(declare can-host?)

(def cards-resources
  {"Open to the Summons"
   {:hosting   {:req #(and (character? %) (rezzed? %))}}

   "Thrall to the Voice"
   {:hosting   {:req #(and (character? %) (rezzed? %))}}
   })