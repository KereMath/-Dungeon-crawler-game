module PE2 where

data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

data Chamber = Cavern |
               NarrowPassage |
               UndergroundRiver |
               SlipperyRocks deriving (Show, Eq)
data Enemy = Enemy String Integer Integer deriving (Show, Eq)
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)
type Dungeon = Tree Chamber [Encounter]

traversePath :: Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePath hp (Node _ encounters []) _ =
  foldl handleEncounter (hp, 0) encounters
traversePath hp (Leaf _ encounters) _ =
  foldl handleEncounter (hp, 0) encounters
traversePath hp (Node _ encounters children) (x:xs) =
  let (newHp, gold) = foldl handleEncounter (hp, 0) encounters
      (remainingHp, remainingGold) = traversePath newHp (children !! x) xs
  in (remainingHp, gold + remainingGold)

handleEncounter :: (Integer, Integer) -> Encounter -> (Integer, Integer)
handleEncounter (hp, gold) (Fight (Enemy _ dmg dropGold)) =
  (hp - dmg, gold + dropGold)
handleEncounter (hp, gold) (Treasure (Gold amount)) =
  (hp, gold + amount)
handleEncounter (hp, gold) (Treasure (Potion heal)) =
  (hp + heal, gold)
  
  
  
  
  
findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain hp (Leaf _ encounters)
  | newHp > 0 = gold
  | otherwise = 0
  where
    (newHp, gold) = foldl handleEncounter (hp, 0) encounters
findMaximumGain hp (Node _ encounters children)
  | newHp <= 0 = 0
  | null maxGains = gold
  | otherwise = gold + maxGain
  where
    (newHp, gold) = foldl handleEncounter (hp, 0) encounters
    viableChildren = [child | child <- children, findMaximumGain newHp child > 0]
    maxGains = map (findMaximumGain newHp) viableChildren
    maxGain = maximum maxGains


findViablePaths :: Integer -> Dungeon -> Dungeon
findViablePaths _ EmptyTree = EmptyTree
findViablePaths hp (Leaf chamber encounters) =
  let (remainingHp, _) = foldl handleEncounter (hp, 0) encounters
  in case remainingHp > 0 of
       True  -> Leaf chamber encounters
       False -> EmptyTree
findViablePaths hp (Node chamber encounters children) =
  let (remainingHp, _) = foldl handleEncounter (hp, 0) encounters
      viableChildren = filter (not . isEmpty) (map (findViablePaths remainingHp) children)
  in case remainingHp > 0 of
       True -> case viableChildren of
                 [] -> Leaf chamber encounters
                 _  -> Node chamber encounters viableChildren
       False -> EmptyTree
  where
    isEmpty EmptyTree = True
    isEmpty _         = False



mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair hp dungeon = (0,(findViablePaths hp dungeon))

mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree EmptyTree = EmptyTree
mostEfficientSubtree dungeon =
  let (_, bestSubtree) = treefinder dungeon (0, 0) (0, EmptyTree)
    in bestSubtree

treefinder :: Dungeon -> (Integer, Integer) -> (Double, Dungeon) -> (Double, Dungeon)
treefinder EmptyTree _ _ = (0, EmptyTree)
treefinder node@(Leaf chamber encounters) (totalHp, totalGold) best =
  let (hp, gold) = foldl handleEncounter (totalHp, totalGold) encounters
      efficiency = if hp <= 0 then 1 / 0 else fromIntegral gold / fromIntegral hp
      better = if efficiency > fst best then (efficiency, node) else best
  in better
treefinder node@(Node chamber encounters children) (totalHp, totalGold) best =
  let (hp, gold) = foldl handleEncounter (totalHp, totalGold) encounters
      efficiency = if hp <= 0 then 1 / 0 else fromIntegral gold / fromIntegral hp
      subtreeBest = foldl (finder (hp, gold)) best children
      better = if efficiency > fst subtreeBest then (efficiency, node) else subtreeBest
  in better

finder :: (Integer, Integer) -> (Double, Dungeon) -> Dungeon -> (Double, Dungeon)
finder (hp, gold) summed child = treefinder child (hp, gold) summed
