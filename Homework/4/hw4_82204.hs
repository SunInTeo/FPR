import Data.List

main :: IO()
main = do
    print $ getSunk database == [("Guadalcanal",["Kirishima"]),("North Atlantic",["Bismarck","Hood"]),("North Cape",["Schamhorst"]),("Surigao Strait",["Fuso","Yamashiro"])]
    print $ inBattleAfterDamaged database == ["California","Prince of Wales"]

    print $ grandchildrenIncreased t1 == True
    print $ grandchildrenIncreased t2 == False

type Name = String
type Date = String
type Class = String
type Result = String
type Launched = Int

data Battle = Battle Name Date deriving Show
data Ship = Ship Name Class Launched deriving Show
data Outcome = Outcome Name Name Result deriving Show

type Database = ([Outcome], [Battle], [Ship])

outcomes :: [Outcome]
outcomes = [ Outcome "Bismarck" "North Atlantic" "sunk", Outcome "California" "Surigao Strait" "ok", Outcome "Duke of York" "North Cape" "ok", Outcome "Fuso" "Surigao Strait" "sunk", Outcome "Hood" "North Atlantic" "sunk", Outcome "King George V" "North Atlantic" "ok", Outcome "Kirishima" "Guadalcanal" "sunk", Outcome "Prince of Wales" "North Atlantic" "damaged", Outcome "Rodney" "North Atlantic" "ok", Outcome "Schamhorst" "North Cape" "sunk", Outcome "South Dakota" "Guadalcanal" "damaged", Outcome "Tennessee" "Surigao Strait" "ok", Outcome "Washington" "Guadalcanal" "ok", Outcome "Prince of Wales" "Guadalcanal" "ok", Outcome "West Virginia" "Surigao Strait" "ok", Outcome "Yamashiro" "Surigao Strait" "sunk", Outcome "California" "Guadalcanal" "damaged" ]

battles :: [Battle]
battles = [ Battle "Guadalcanal" "1942-11-15", Battle "North Atlantic" "1941-05-25", Battle "North Cape" "1943-12-26", Battle "Surigao Strait" "1944-10-25" ]

ships :: [Ship]
ships = [ Ship "California" "Tennessee" 1921, Ship "Haruna" "Kongo" 1916, Ship "Hiei" "Kongo" 1914, Ship "Iowa" "Iowa" 1943, Ship "Kirishima" "Kongo" 1915, Ship "Kongo" "Kongo" 1913, Ship "Missouri" "Iowa" 1944, Ship "Musashi" "Yamato" 1942, Ship "New Jersey" "Iowa" 1943, Ship "North Carolina" "North Carolina" 1941, Ship "Ramillies" "Revenge" 1917, Ship "Renown" "Renown" 1916, Ship "Repulse" "Renown" 1916, Ship "Resolution" "Renown" 1916, Ship "Revenge" "Revenge" 1916, Ship "Royal Oak" "Revenge" 1916, Ship "Royal Sovereign" "Revenge" 1916, Ship "Tennessee" "Tennessee" 1920, Ship "Washington" "North Carolina" 1941, Ship "Wisconsin" "Iowa" 1944, Ship "Yamato" "Yamato" 1941, Ship "Yamashiro" "Yamato" 1947, Ship "South Dakota" "North Carolina" 1941, Ship "Bismarck" "North Carolina" 1911, Ship "Duke of York" "Renown" 1916, Ship "Fuso" "Iowa" 1940, Ship "Hood" "Iowa" 1942, Ship "Rodney" "Yamato" 1915, Ship "Yanashiro" "Yamato" 1918, Ship  "Schamhorst" "North Carolina" 1917, Ship "Prince of Wales" "North Carolina" 1937, Ship "King George V" "Iowa" 1942, Ship "West Virginia" "Iowa" 1942 ]

database :: Database
database = (outcomes, battles, ships)

listOfBattles :: [Outcome] -> [Name] -> [(Name, [Name])]
listOfBattles outcomesList [] = []
listOfBattles outcomesList (battle:battlesList) = [(battle, map (\(Outcome ship _ _) -> ship) (filter (\(Outcome ship name result) -> name == battle) outcomesList))] ++ listOfBattles outcomesList battlesList

getSunk :: Database -> [(Name, [Name])]
getSunk (outcomesList, _, _) = sort $ listOfBattles sunkOutcomes (nub $ map (\(Outcome _ name _) -> name) sunkOutcomes)
    where 
        sunkOutcomes = filter (\(Outcome _ _ result) -> result == "sunk") outcomesList

getAllBattlesForTheDamaged :: [Outcome] -> [Name] -> [Outcome]
getAllBattlesForTheDamaged outcomesList [] = []
getAllBattlesForTheDamaged outcomesList (shipName:shipsNamesList) = [] ++ (filter (\(Outcome shName _ _) -> shipName == shName) outcomesList) ++ getAllBattlesForTheDamaged outcomesList shipsNamesList

getOutcomesForOneShip :: Name -> [Outcome] -> [Outcome]
getOutcomesForOneShip shipName outcomesList = filter (\(Outcome shName _ _) -> shipName == shName) outcomesList

getLengthOfOutcomesPerShip :: Name -> [Outcome] -> Int
getLengthOfOutcomesPerShip shipName outcomesList = length $ getAllBattlesForTheDamaged outcomesList [shipName]

excludeShipsWithOnlyOneBattle :: [Outcome] -> [Outcome]
excludeShipsWithOnlyOneBattle outcomesList = [] ++ (filter (\(Outcome shName _ _) -> getLengthOfOutcomesPerShip shName outcomesList > 1) outcomesList)

inBattleAfterDamaged :: Database -> [Name]
inBattleAfterDamaged (outcomesList, battlesList, shipsList) = sort $ nub $ map (\(Outcome name _ _) -> name) (excludeShipsWithOnlyOneBattle (getAllBattlesForTheDamaged outcomes (map (\(Outcome ship _ _) -> ship) damagedOutcomes))) 
    where 
        damagedOutcomes = filter (\(Outcome _ _ result) -> result == "damaged") outcomesList

data BTree = Nil | Node Int BTree BTree

t1 = Node 1 (Node (-1) (Node 2 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)) (Node (-1) Nil Nil)
t2 = Node 1 (Node 2 (Node 1 Nil Nil) (Node 1 (Node 10 Nil Nil) Nil)) (Node 3 Nil Nil)

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Nil = True
grandchildrenIncreased (Node value left right) = isIncreasing left && isIncreasing right && grandchildrenIncreased left && grandchildrenIncreased right
    where
        isIncreasing :: BTree -> Bool
        isIncreasing Nil = True
        isIncreasing (Node _ Nil Nil) = True
        isIncreasing (Node _ (Node lv _ _) Nil) = (lv - value) > 0
        isIncreasing (Node _ Nil (Node rv _ _)) = (rv - value) > 0
        isIncreasing (Node _ (Node lv _ _) (Node rv _ _)) = (lv - value) > 0 && (rv - value) > 0