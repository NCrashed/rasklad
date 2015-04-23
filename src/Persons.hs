module Persons where

import Types

persons :: [Person]
persons = [
    Person "Левон" Beef Male
  , Person "Гуща" Beef Male
  , Person "Анатолий" Beef Male
  , Person "Ольга" Beef Female
  , Person "Кристина" Other Female
  , Person "Борис" Beef Male
  , Person "Япл Захарий Адам" Beef Male
  , Person "Хитеш" Pork Male
  , Person "Святослав" Beef Male
  ]

genderWeightFactor :: Gender -> Double
genderWeightFactor Male = 1.0
genderWeightFactor Female = 0.75

