(ns meccg.help
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [clojure.string :refer [split]]))

(def app-state (atom {}))

(def help-data
  "List of maps with FAQ about cardnum.net. Every section MUST have an :id here, so the links can work."
  (list
    {:id "general"
     :title "General"
     :sub (list
            {:id "dostuff"
             :title "How do I do I perform actions in a game?"
             :content (list
                        [:p "In general, if you want to perform an action connected to a card, try clicking that card. "
                         "Either something will happen or a menu should appear."]
                        [:p "Cards can be moved by clicking them and dragging. Clicking and dragging excessive cards from one's "
                         "hand to discard pile is normally done after one's phase/turn ends and they are over their hand size limit."]
                        [:p "A player's remaining G.I., Stage Points, or MPs in a category etc. can be manipulated by hand by using "
                         "plus/minus signs next to their numbers in the panel on the left."])}
            {:id "closemenu"
             :title "How do I close a card's menu?"
             :content [:p "Click that card again. If it isn't a menu, but a bugged prompt that shouldn't be there, "
                       "try using " [:code "/close-prompt"]]}
            {:id "commands"
             :title "How do I use commands during a game?"
             :content [:p "To use a command, type it in chatbox and press Enter. Some of the commands will bring up a prompt "
                       "requiring you to select something. List of available commands:"
                       [:ul
                        [:li [:code "/card-info"] " - display debug info about a card (player's own cards only)"]
                        [:li [:code "/clear-win"] " - requests game to clear the current win state.  Requires both players to request it"]
                        [:li [:code "/click 100"] " - Backdoor for solo play and get a draw menu"]
                        [:li [:code "/click 80"] " - Backdoor for solo play to get end turn menu, to go backwards thru menus"]
                        [:li [:code "/close-prompt"] " - close an active prompt and show the next waiting prompt, or the core actions"]
                        [:li [:code "/counter n"] " - set counters on a card to n (player's own cards only). Attempts to infer the type of counter to place. If the inference fails, you must use the next command to specify the counter type."]
                        [:li [:code "/counter type n"] " - set the specified counter type on a card to n (player's own cards only). Type must be " [:code "agenda"] ", " [:code "advance"] ", " [:code "credit"] ", " [:code "power"] ", or " [:code "virus"] ". Can be abbreviated as " [:code "ag"] ", "  [:code "ad"] ", "  [:code "c"] ", "  [:code "p"] ", or " [:code "v"] " respectively."]
                        [:li [:code "/deck #n"] " - Put card number n from your hand on top of your deck"]
                        [:li [:code "/discard"] " - Discard card hard to get to"]
                        [:li [:code "/discard-n #n"] " - Discard card number n from your hand"]
                        [:li [:code "/discard-random"] " - Discard a random card from your hand"]
                        [:li [:code "/draw n"] " - Draw n cards"]
                        [:li [:code "/facedown"] " - Select a card to place facedown"]
                        [:li [:code "/handsize n"] " - Set your handsize to n"]
                        [:li [:code "/hide"] " - Hide a card"]
                        [:li [:code "/hide-hand"] " - Hide your hand after revealing"]
                        [:li [:code "/host"] " - Host a card on another"]
                        [:li [:code "/move-bottom"] " - Pick a card in your hand to put on the bottom of your deck"]
                        [:li [:code "/move-deck"] " - Pick a card from your play-area to put on top of your deck"]
                        [:li [:code "/move-hand"] " - Pick a card from your play-area to put into your hand"]
                        [:li [:code "/move-site"] " - Pick a card from your play-area to put into your location deck"]
                        [:li [:code "/move-sb"] " - Pick a card from your play-area to put into your sideboard"]
                        [:li [:code "/move-fw-sb"] " - Pick a card from your play-area to put into your fw-sideboard"]
                        [:li [:code "/o"] " - Toggle the option key, for region access from the location deck"]
                        [:li [:code "/p"] " - Select a site for starter movement site path, quicker access"]
                        [:li [:code "/re-deck"] " - Use this to move all cards from your discard back to your deck"]
                        [:li [:code "/reveal"] " - Select a card to reveal"]
                        [:li [:code "/reveal-hand"] " - Reveals your hand to opponent, don't forget to hide"]
                        [:li [:code "/rfg"] " - Select a card to remove from the game"]
                        [:li [:code "/rfgh"] " - Select a card to remove from the game hidden"]
                        [:li [:code "/r"] " - Roll 2d6"]
                        [:li [:code "/roll n"] " - Roll an n-sided die"]
                        [:li [:code "/score"] " - Use this to score a card"]
                        [:li [:code "/z"] " - Double the zoom"]
                        ]]}
            {:id "documentation"
             :title "Is there more documentation on how to use Cardnum.net?"
             :content [:p "Read the "
                       [:a {:href "https://github.com/rezwits/meccg/wiki/Cardnum.net-Guide" :target "_blank"}
                        "Cardnum.net Guide"] " on the GitHub wiki."]}
            )}
    {:id "beginners"
     :title "Beginners"
     :sub (list
            {:id "learnrules"
             :title "Where can I find the game's rules explanation?"
             :content (list [:p "The first step is reading " [:a {:href "http://councilofelrond.org/wp-content/uploads/2017/03/The-Wizards-Rulebook.pdf" :target "_blank"} "the official rulebook"]]
                            [:p "Once familiar with the basics, the finer points of rules/card interactions can be found in "
                             "the other official rulebook links on "
                             [:a {:href "http://councilofelrond.org/rules/"} "the CoE page"] ". "
                             "There is also " [:a {:href "http://forum.councilofelrond.org/viewtopic.php?f=16&t=1500"} "Universal Rules Document"] ", which is a collection "
                             "of rulings (also unofficial) regarding various cards and game situations."])}
            {:id "firstgame"
             :title "Can I play my first game on cardnum.net even though I'm a total beginner and never played?"
             :content [:p "Sure! Many players will be happy to play/teach a beginner if they know what they're getting into beforehand. "
                       "So just create a new game with name such as \"beginner here\" or \"standard only please\", someone "
                       "happy to play with a beginner should join after a while."]}
            {:id "finddecks"
             :title "Where can I find some good starting decks?"
             :content (list [:p [:a {:href "http://forum.councilofelrond.org/viewforum.php?f=128"} "CoE Deck tips"] " is a good resource for finding decks of all kinds. "
                       "There are a few challenge deck included when you create your account."])}
            {:id "communities"
             :title "Where can I find other MECCG players to talk to?"
             :content [:p "Apart from the chatrooms here on Cardnum.net, here are a few links to online MECCG communities:"
                       [:ul
                        [:li [:a {:href "http://forum.councilofelrond.org/"} "CoE forums"]]
                        ]]}
            )}
    {:id "site"
     :title "Website"
     :sub (list
            {:id "avatar"
             :title "How do I change my avatar?"
             :content [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
                       " and create an account with the same email as the one used to register on Cardnum.net. Please note that "
                       "it can sometimes take up to a few hours for the new avatar to be visible on the site."]}
            {:id "bestbrowser"
             :title "What is the best supported browser?"
             :content '([:p "Google Chrome or Firefox on a desktop or laptop is recommended. Safari should work fine too."]
                        [:p "There is limited support for tablet browsers. If you have too many cards to fit on the screen you might not able to see all of them."]
                        [:p "Using a phone is not recommended. The screen will most likely be too small to fit the gameboard."])}
            {:id "fullscreen"
             :title "How to use cardnum.net in fullscreen mode on a tablet?"
             :content [:p "Add cardnum.net to your homescreen as described "
                       [:a {:href "http://www.howtogeek.com/196087/how-to-add-websites-to-the-home-screen-on-any-smartphone-or-tablet/"} "here"]
                       ". If you tap on the homescreen icon, you will be in fullscreen."]}
            {:id "privatemsgs"
             :title "How do I send a private message / add someone to friendlist?"
             :content [:p "The community management issues such as private messages or friendlist are currently not implemented. "
                       "They are planned, but no specific date is set, as all of our code is written by volunteers."]}
            {:id "competitive"
             :title "What is the point of the \"Competitive\" room in lobby? How does it differ from \"Casual\"?"
             :content (list [:p "Different rooms in lobby are meant to help people with similar expectations about the game find each other. "
                             "In general, competitive room is for games with players intending to play competitively. "
                             "This may mean something different to each of them... However, since it's a non-default room, "
                             "going there and creating or joining a game usually isn't accidental and is a declaration of some kind of competitive intent."]
                            [:p "Some recommendations for playing in the competitive room:"
                             [:ul
                              [:li "a decent knowledge of the game's rules"]
                              [:li "familiarity with the site's interface"]
                              [:li "a " [:span.legal "tournament legal"] " deck"]
                              [:li "enough time reserved for a full game and no distractions"]]]
                            [:p "Games with players not able or willing to follow above recommendations are probably better suited to the Casual room. "
                             "Some examples would be: learning the game, learning the site's interface, testing a completely new and crazy deck idea, "
                             "testing future spoilers, playing on a touchscreen, playing at work and likely to have to quit on short notice, etc. "
                             "All of these circumstances may cause needless frustration of players expecting to play a game in a competitive setting."])}
            {:id "aboutstats"
             :title "What are the options for tracking Game and Deck Statistics, and what do they mean?"
             :content (list [:p "Games Started vs. Completed is always logged and displayed.  We want to discourage people dropping in games. "
                             "You can toggle between the modes listed below if you feel like being a casual player one moment then logging stats the next. "
                             "No data is lost or cleared when you toggle between modes."
                             [:ul
                              [:li "Always - statistics are kept and displayed for all games you play"]
                              [:li "Competitive lobby only - statistics are kept and displayed only for competitive games"]
                              [:li "None - statistics are neither logged or displayed"]]]
                            [:p "What do the game statistics mean?"
                             [:ul
                              [:li "Games Started - games you have entered."]
                              [:li "Games Completed - games that had a winner, or games that did not complete but opponent dropped first."]
                              [:li "Games Incomplete -  games with no winner where you dropped first, and did not concede."]
                              [:li "Games Won - games won.  The percentage is compared to those games lost."]
                              [:li "Games Lost - games lost.  The percentage is compared to those games won."]]]
                            [:p "Your game completion rate is visible in the player lobby so people can determine if they should play against you."
                             " Don't quit during games - please concede if you have to leave."])}
            )}
    {:id "cards"
     :title "Cards and Specific Interactions"
     :sub (list
            {:id "Deck Building Set"
             :title "How do I choose to use a card from a specific expansion?"
             :content [:p "When buidling a deck sometimes you may for instance want to use a hazard from the Lidless-eye"
                       " expansion.  In this particular case, you would, in your hazard section of your decklist type "
                       "\"1 Ambusher (LE)\".  If you leave the card as \"1 Ambusher\", the game usually defaults to the first print."]}
            {:id "Deck Building Alignment and Set"
             :title "How do I choose a card of a different alignment?"
             :content [:p "If for instance you are building a Fallen-wizard deck and you wish to use a Minion Palantír "
                       "or other item, you can set the alignment by typing \"1 Palantír of Elostirion [M] (LE)\"."]}
            {:id "Deck Conflicts with Alignment and/or set"
             :title "I am having a problem adding Doeth?"
             :content [:p "There are about 12 cards that break the \"Format\" of the deck building process.  To add these "
                       "card to a deck, there are two ways to alleviate your fustration.  First use the search box, and "
                       "type the first part of the card's name, or in the section, get the first part of the card's name"
                       " and click + to get the system to recognize the card.  These are normally all the cards with \"(text)\"."]}
             )}
    {:id "troubleshooting"
     :title "Troubleshooting"
     :sub (list
            {:id "weird"
             :title "The site is behaving weird."
             :content [:p "The server code may have been freshly updated and you don't have the latest Javascript code. "
                       "First step in every troubleshooting should be a forced refresh of your browser by doing a "
                       [:a {:href "http://refreshyourcache.com/en/cache/"} "force refresh"] " (" [:code "Ctrl + F5"] " on Windows). "
                       "Also read the announcements on the main page, something about server problems may be written there."]}
            {:id "touchproblems"
             :title "The website doesn't work well on my touchscreen device."
             :content [:p "Touchscreen devices are currently not supported. See answer to " [:a {:href "#bestbrowser"} "this question"]
                       " for best browsers to use with Cardnum.net."]}
            {:id "toomanycompanies"
             :title "There are too many companies to fit on my screen."
             :content [:p "Decrease the zoom level of your browser and you should be able to see everything. If you are using "
                       "Chrome, you can do it by pressing CTRL and - (minus). If you are using Firefox, you may need to place "
                       [:a {:href "https://addons.mozilla.org/pl/firefox/addon/zoom-page/"} "Zoom Page addon"] " before the zoom works correctly."]}
            {:id "zerogames"
             :title "Whenever I connect to the site, I see there are 0 games in the lobby."
             :content [:p "This is most likely a websocket issue. Check if your network filters let through traffic from ws.cardnum.net. "
                       "Whitelisting *.cardnum.net should solve the problem."]}
            )}
    {:id "alternativeformats"
     :title "Alternative Formats"
     :sub (list
            {:id "Dreamcards"
             :title "What is the Dreamcard format?"
             :content [:p "This is a format designed by players of the game with a passion to keep the game "
                       "growing thru digital expansions of the game. "
                       [:a {:href "http://forum.councilofelrond.org/viewforum.php?f=58" :target "_blank"} "CoE Dreamcards link"]]}
            )}
    {:id "getinvolved"
     :title "Getting Involved"
     :sub (list
            {:id "reportingbugs"
             :title "How can I report a bug?"
             :content [:p "The best place to report bugs is the " [:a {:href "https://github.com/rezwits/meccg/issues" :target "_blank"} "GitHub issue tracker"]
                       ". Before reporting, it is best to make a quick search to see if it's already been reported. "
                       "If the bug concerns a card, look it up in "
                       [:a {:href "https://docs.google.com/spreadsheets/d/1Ly2RVe4QZRhN6TUfV1YO9DuuYvywzMnnaCunQapzzfs/edit?usp=sharing" :target "_blank"} "Card implementation status"]
                       " - the card in question may be unimplemented yet."]}
            {:id "features"
             :title "How can I suggest a feature?"
             :content [:p "Same as bugs - feature requests should go on the " [:a {:href "https://github.com/rezwits/meccg/issues" :target "_blank"} "GitHub issue tracker"]
                       ". Again, it's best to make a quick search first to avoid duplicating existing issues."]}
            {:id "donations"
             :title "How can I make a donation?"
             :content [:p "Donation info can be found on the " [:a {:href "/about"} "About"] " page."]}
            {:id "deck-dice"
             :title "How do I have different dice per deck?"
             :content [:p "Dice per deck are enabled for " [:a {:href "/about"} "donators"] " and developers of the site. If you belong to one of the aforementioned groups and you feel like you should have them enabled, "
                       "but you don't, " [:a {:href "/about"} "contact us"] "."]}
            {:id "devs"
             :title "How can I help with the coding/webdesign?"
             :content (list [:p "Visit the project page on " [:a {:href "https://github.com/rezwits/meccg/" :target "_blank"} "GitHub"] " and fork "
                             "the repository. Implement the changes you were planning on doing and create a PR (Pull Request). If you are in "
                             "need of some ideas, check out " [:a {:href "https://github.com/rezwits/meccg/labels/easy" :target "_blank"} "issues marked 'easy' on GitHub"] "."]
                            )}
            )}))

(def help-toc
  "Generates list serving as help's table of contents. Parses help-data."
  [:nav {:role "navigation" :class "table-of-contents"}
    [:ul (for [{:keys [id title sub] :as section} help-data]
      [:li [:a (when id {:href (str "#" id)}) title]
       [:ul (for [{:keys [id title] :as question} sub]
              [:li [:a (when id {:href (str "#" id)}) title]])]])]])

(def help-contents
  "Takes help-data and translates it to HTML tags."
  (for [{:keys [id title sub] :as section} help-data]
    (list [:h2 {:id id} title]
          (for [{:keys [id title content] :as question} sub]
            (list [:h3 {:id id} title]
                  content)))))

(defn help [cursor owner]
  (om/component
    (sab/html
      [:div.help.panel.content-page.blue-shade
       [:h2 "Help Topics"]
       help-toc
       help-contents])))

(om/root help app-state {:target (. js/document (getElementById "help"))})
