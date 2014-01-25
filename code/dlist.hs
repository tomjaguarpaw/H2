{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty), renderTree,
                                  uniqueXLayout)
import Diagrams.Backend.SVG (renderSVG, B)
import Data.Maybe (fromJust)

treeDiagram' = fromJust . fmap treeDiagram . uniqueXLayout 2 2

treeDiagram t = pad 1.1 . lw 0.05 . centerXY $ renderTree
          (\n -> (text n
                  <> roundedRect 3 1.5 0.3 # fc gold)
          )
          (~~) t
          `atop` square 1 # scaleY 12 # translateY (-5)
          # scaleX 34
          # lw 0 # fc whitesmoke

data Eval a = Ground a | App (Eval a) (Eval a) | Comp (Eval a) (Eval a)
            deriving Show

data Tree a = Leaf a | Branch (Tree a) (Tree a)

fromList :: [a] -> Tree [a]
fromList = Leaf

toList :: Tree [a] -> [a]
toList (Leaf x) = x
toList (Branch (Leaf x) r) = x ++ toList r
toList (Branch (Branch l1 l2) r) = toList (Branch l1 (Branch l2 r))

append :: Tree [a] -> Tree [a] -> Tree [a]
append = Branch

foldlTree :: Int
foldlTree = length (toList (foldl append (Leaf []) (map (Leaf . return) [1..20000])))

foldlList :: Int
foldlList = length (foldl (++) [] (map return [1..20000]))

step :: Eval a -> Maybe (Eval a)
step (App (Comp f g) x) = Just (App f (App g x))
step _ = Nothing

bTreeOfEval :: Eval String -> BTree String
bTreeOfEval (Ground x) = BNode x   Empty Empty
bTreeOfEval (App l r)  = BNode "$" (bTreeOfEval l) (bTreeOfEval r)
bTreeOfEval (Comp l r) = BNode "." (bTreeOfEval l) (bTreeOfEval r)

g = Ground
(...) = Branch
($$) = App
h = Leaf . (++ "s")

unfoldr :: (a -> Maybe a) -> a -> [a]
unfoldr f s = s : case f s of Nothing -> []
                              Just s' -> unfoldr f s'

compsOfTree :: Tree String -> Eval String
compsOfTree (Leaf x) = Ground (x ++ "++")
compsOfTree (Branch l r) = Comp (compsOfTree l) (compsOfTree r)

appendsExprOfTree :: Tree String -> String
appendsExprOfTree (Leaf x) = x
appendsExprOfTree (Branch l r) = concat ["(", appendsExprOfTree l,
                                         " ++ ", appendsExprOfTree r, ")"]

compsExprOfTree :: Tree String -> String
compsExprOfTree (Leaf x) = "(" ++ x ++ "++)"
compsExprOfTree (Branch l r) = concat ["(", compsExprOfTree l,
                                         " . ", compsExprOfTree r, ")"]

appendsOfTree :: Tree String -> BTree String
appendsOfTree (Leaf x) = BNode x Empty Empty
appendsOfTree (Branch l r) = BNode "++" (appendsOfTree l) (appendsOfTree r)

appendExpr = (((h "a" ... h "b") ... (h "c" ... h "d"))
               ... (h "e" ... (h "f" ... h "g")))

composition = compsOfTree appendExpr $$ g "[]"

reductions = unfoldr step composition

renderTo (filename, diag) = renderSVG filename (mkSizeSpec (Just 500) Nothing)
                            (diag :: Diagram B R2)

myrender = do
  mapM_ renderTo (zip (map (\x -> "/tmp/dlist-eval" ++ show x ++ ".svg") [1..])
                      (map (treeDiagram' . bTreeOfEval) reductions))
  renderTo ("/tmp/dlist-appends.svg", (treeDiagram' . appendsOfTree) appendExpr)
