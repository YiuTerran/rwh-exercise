data Tree a = Tree {
    value :: a,
    left :: Maybe (Tree a),
    right:: Maybe (Tree a)
} deriving (Eq, Show)