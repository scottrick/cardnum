(ns meccg.artists
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [clojure.string :refer [split]]))

(def artists [
              "Alan Guitierrez"
              "Alan Lee"
              "Alan Pollack"
              "Alan Rabinowitz"
              "Alarie Tano"
              "Allen G. Douglas"
              "Ana Kusicka"
              "Anders Finer"
              "Andrea Piparo"
              "Andreas Rocha"
              "Andrew Goldhawk"
              "Angel Falto"
              "Angelo Michelucci"
              "Angelo Montanini"
              "Angus McBride"
              "Anke K. Eissmann"
              "Annika Ritz"
              "April Lee"
              "Arch Stanton"
              "Audrey Corman"
              "Barbera E. de Jong"
              "Birthe Jabs"
              "Bob Eggleton"
              "Brad Williams"
              "Bradly van Camp"
              "Brian Durfee"
              "Brian Snoddy"
              "Brian T. Fox"
              "Brom"
              "Carol Heyer"
              "Chris Achilleos"
              "Chris Cocozza"
              "Chris Trevas"
              "Christina Wald"
              "Christopher Miller"
              "Cortney Skinner"
              "Dameon Willich"
              "Daniel Frazier"
              "Daniel Gelon"
              "Daniel Horne"
              "Daniel Wikart"
              "Darrell Sweet"
              "Darryl Elliott"
              "David A. Cherry"
              "David Deitrick"
              "David Kooharian"
              "David Martin"
              "David Monette"
              "David R. Seeley"
              "David Sexton"
              "David T. Wenzel"
              "David Wyatt"
              "Debbie Hughes"
              "Dennis Gordeev"
              "Devon Cady-Lee"
              "Donato Giancola"
              "Doug Anderson"
              "Doug Kovacs"
              "Douglas Beekman"
              "Douglas Shuler"
              "Douglass Chaffee"
              "Drew Tucker"
              "Ebe Kastein"
              "Edward Beard, Jr."
              "Elena Kukanova"
              "Ellym Sirac"
              "Eric David Anderson"
              "fel_x"
              "Felicia Cano"
              "Flavio Bolla"
              "Frank Frazetta"
              "Frank Kelley Freas"
              "FreakyGaming"
              "Fredrik Högberg"
              "Friedrich A. Haas"
              "Fritz Haas"
              "Gail McIntosh"
              "Greg McPherson"
              "Grzegorz Rutkowski"
              "Gyula Pozsgay"
              "Hanna Lehmusto"
              "Hannibal King"
              "Harry Quinn"
              "Heather Hudson"
              "Henning Janssen"
              "Hope Hoover"
              "Inger Edelfeldt"
              "J. Wallace Jones"
              "Jacek Kopalski"
              "Jan Pospíšil"
              "Jason Manley"
              "Jean Pierre Reol"
              "Jeffery G. Reitz"
              "Jenny Dolfen"
              "Jo Hartwig"
              "John C. Duke"
              "John Howe"
              "John Luck"
              "John Monteleone"
              "Jon Foster"
              "Jonas Jakobsson"
              "Jonas Jensen"
              "Jorge Jacinto"
              "Jos van Zijl"
              "Jugra"
              "Juhani Jokinen"
              "Julia Alekseeva"
              "Julie Freeman"
              "Justin Sweet"
              "Kaja Foglio"
              "Kalen Chock"
              "Kate Pfeilschiefter"
              "Keith Parkinson"
              "Ken Meyer, Jr."
              "Kevin Ward"
              "Kim Demulder"
              "Kimberly80"
              "Kirk Quilaquil"
              "Kyle Anderson"
              "Larry Elmore"
              "Larry Forcella"
              "Lee Seed"
              "Leo Winstead"
              "Leonid Kozienko"
              "Lĩga Kļaviņa"
              "Lisa Hunt"
              "Lissanne Lake"
              "Liz Danforth"
              "Lori Deitrick"
              "Lubov"
              "Lucas Alcantara"
              "Luis Royo"
              "Margaret Organ-Kean"
              "Maria & Elena Gubina"
              "Mark Forrer"
              "Mark Maxwell"
              "Mark Molchan"
              "Mark Poole"
              "Matt Steward"
              "Matt Stewart"
              "Matthew Bradbury"
              "Matthew Innis"
              "Matthew Stawicki"
              "Melissa Benson"
              "Melissa Brown"
              "Mia Steingraeber"
              "Mia Tavonatti"
              "Michael Apice"
              "Michael Astrachan"
              "Michael Hague"
              "Michael Kaluta"
              "Michael Komarck"
              "Michael Kucharski"
              "Miguel Coimbra"
              "Miika Karmitsa"
              "Milivoj Ceran"
              "N. Taylor Blanchard"
              "Nathalie Hertz"
              "Nicholas Jainschigg"
              "Nick Deligaris"
              "Noah Bradley"
              "Olivier Frot"
              "Omar Rayyan"
              "Pamela Shanteau"
              "Pascal Yung"
              "Pat Morrissey"
              "Paul Gregory"
              "Paul Laleine"
              "Paul Lasaine"
              "Per Sjögren"
              "Pete Fenlon"
              "Peter X. Price"
              "Quinton Hoover"
              "R. Ward Shipman"
              "Rafal Hrynkiewicz"
              "Randy Asplund-Faith"
              "Randy Gallegos"
              "Raphaël Lacoste"
              "Rebecca Guay"
              "Reto Kaul"
              "Richard Hook"
              "Rob Alexander"
              "Robin Wood"
              "Rodney Matthews"
              "Roger Garland"
              "Romas Kukalis"
              "Ron Miller"
              "Ron Rousselle II"
              "Ron Spencer"
              "Ron Walotsky"
              "Ronald Chironna"
              "Ronald Shuey"
              "Sandrine Gestin"
              "Santiago Iborra"
              "Sarel Theron"
              "Sebastian Wagner"
              "Sedone Thongvilay"
              "Sérgio Artigas"
              "Stacey K. Kite"
              "Stefano Baldo"
              "Stephen A. Daniele"
              "Stephen F. Schwartz"
              "Stephen Graham Walsh"
              "Stephen Hickman"
              "Stephen King"
              "Stephen Schwartz"
              "Steve Argyle"
              "Steve Luke"
              "Steve Otis"
              "Steven Cavallo"
              "Storn Cook"
              "Susan Van Camp"
              "Ted Nasmith"
              "Thomas Gianni"
              "Tim Baker"
              "Tim Kirk"
              "Timo Vihola"
              "Todd Lockwood"
              "Tom Cross"
              "Tom Dow"
              "Tom Kidd"
              "Tom Simonton"
              "Toni Galuidi"
              "Turbine LotRO"
              "Turner Mohan"
              "Val Mayerik"
              "Vincens Luján"
              "Vincent Dutrait"
              "William O'Connor"
              "winterkeep"
              "Wouter Florusse"
              "Zina Saunders"
               ])