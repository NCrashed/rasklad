module Products where

import Types
import Persons

import Prelude hiding (product)
import Control.Applicative
import qualified Data.Map as M

mergeProduct :: Product -> Product -> Product
mergeProduct p1 p2 = productAddGramm (productGramm p2) p1

mergeProducts :: [Product] -> [Product]
mergeProducts ps = M.elems mp
  where 
    mp :: M.Map String Product
    mp = foldr (\p -> M.insertWith mergeProduct (productName p) p) M.empty ps
        
replaceProduct :: Person -> Product -> [Product]
replaceProduct person p@(Product _ m tp _) 
  | tp == Beef && tp' /= Beef = replacement
  | tp == Pork && tp' == Other = replacement
  | otherwise = []
  where
    m2 = m `div` 2
    tp' = personFlavor person
    replacement = [additionalCheese person m2, additionalNuts person m2]

replaceProducts :: Person -> [Product] -> [Product]
replaceProducts person ps = concat $ replaceProduct person <$> ps

replaceMeals :: Person -> Meal -> Meal
replaceMeals person meal = meal { mealProducts = replaceProducts person $ mealProducts meal }

replaceDay :: Person -> RationDay -> RationDay
replaceDay person rd = rd {
    breakfast = maybe Nothing (Just . replaceMeals person) $ breakfast rd
  , dinner = maybe Nothing (Just . replaceMeals person) $ dinner rd
  , supper = maybe Nothing (Just . replaceMeals person) $ supper rd
}

replacedRation :: Person -> Ration -> Ration
replacedRation person r = replaceDay person <$> r
  
needReplacement :: Person -> Product -> Bool
needReplacement p (Product _ _ tp _)
  | tp == Replacement = False
  | tp == Beef && tp' /= Beef = True
  | tp == Pork && tp' == Other = True
  | otherwise = False
  where
    tp' = personFlavor p

isReplacement :: Person -> Product -> Bool
isReplacement person p = productType p == Replacement && maybe False (person==) (productTargetPerson p)

additionalCheese :: Person -> Int -> Product
additionalCheese p m = Product ("доп. сыр (" ++ personName p ++ ")" ) m Replacement (Just p)

additionalNuts :: Person -> Int -> Product
additionalNuts p m = Product ("доп. орехи (" ++ personName p ++ ")" ) m Replacement (Just p)

oatmeal :: Product
oatmeal = product "овсянка" 50

rice :: Product
rice = product "рис" 50

muesli :: Product
muesli = product "мюсли с сухофруктами" 65

millet :: Product
millet = product "пшено" 50

dutchWaffles :: Product
dutchWaffles = product "голландские вафли" 36

vienneseWafers :: Product
vienneseWafers = product "венские вафли" 30

dryMilk :: Product
dryMilk = product "сухое молоко (обычн.)" 15

dryMilkMuesli :: Product
dryMilkMuesli = product "сухое молоко (мюсли)" 25

raisins :: Product
raisins = product "изюм" 20

prunes :: Product
prunes = product "чернослив" 20

dryCherry :: Product
dryCherry = product "вишня (сух.)" 20

ghee :: Product
ghee = product "топленное масло" 5

rusk :: Product
rusk = product "сухарь (черн.)" 10

sugar :: Product
sugar = product "сахар (кубик)" 7

sugarKasha :: Product
sugarKasha = product "сахар (каша)" 11

teaBlack :: Product
teaBlack = product "чай черный" 4

teaBlackOrange :: Product
teaBlackOrange = product "чай черный с цедрой" 2

teaGreen :: Product
teaGreen = product "чай зеленый" 2

soup :: Product
soup = product "суп" 25

vermicelli :: Product
vermicelli = product "вермишель" 10

pemmikanSoup :: Product
pemmikanSoup = productBeef "сублимясо (суп)" 10

pemmikan :: Product
pemmikan = productBeef "сублимясо (ужин)" 30

sausage :: Product
sausage = productBeef "колбаса (сырокопченая)" 30

porkPate :: Product
porkPate = productPork "паштет (свиной)" 30

porkStew :: Product
porkStew = productPork "тушенка (свиная)" 40

zazharku :: Product
zazharku = product "зажарка" 10

cheese :: Product
cheese = product "сыр" 30

buckwheat :: Product
buckwheat = product "гречка" 70

pasta :: Product
pasta = product "макароны" 70

lentil :: Product
lentil = product "чечевица" 60

spelled :: Product
spelled = product "полба" 60

dryOnionCarrot :: Product
dryOnionCarrot = product "лук и морковь субл." 7

onion :: Product
onion = product "лук" 10

garlic :: Product
garlic = product "чеснок" 5

tomatoPaste :: Product
tomatoPaste = product "томатная паста" 20

nougat :: Product
nougat = product "нуга" 30

turkishDelight :: Product
turkishDelight = product "рахат-лукум" 30

halvaChocolate :: Product
halvaChocolate = product "халва в шоколаде" 30

tulaGingerbread :: Product
tulaGingerbread = product "тульский пряник" 30

oatmealCoockies :: Product
oatmealCoockies = product "овсяное печенье" 30

kozinaki :: Product
kozinaki = product "козинаки" 30

coockiesChocolate :: Product
coockiesChocolate = product "печенье юбилейное с шоколадной глазурью" 30