import Text.XHtml (base)


data Rose a = Node a [Rose a] deriving (Show)

size :: Rose a -> Int
size (Node _ children) = 1 + sum (map size children)

height :: Rose a -> Int
height (Node _ children) = 1 + maximum ( 0 : map height children)

leavesCount :: Rose a -> [a]
leavesCount (Node val []) = [val]
leavesCount (Node val children) = concatMap leavesCount children

elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node val children) = [val]
elemsOnDepth d (Node val children) = concatMap (elemsOnDepth (d-1)) children

instance Functor Rose where
  fmap f (Node val children) = Node (f val) (map (fmap f) children)

foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Node value children) = foldl (foldRose f) (f acc value) children

generateRose :: (Num d, Eq d) =>  d -> (a -> [a]) -> a -> Rose a
generateRose 0 f root = Node root []
generateRose d f root = Node root (map (generateRose (d-1) f) (f root))

roseTree :: Rose Int
roseTree = Node 1 [Node 2 [Node 4 [], Node 5 [], Node 8 [Node 10 [], Node 11 []],  Node 9 []], Node 3 [Node 6 [], Node 7 []]]

main = do return (generateRose 2 (\x->[x+1,x+2]) 2)