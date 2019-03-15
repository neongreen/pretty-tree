{- |
   Module      : Data.Tree.Pretty
   Description : Pretty-print trees.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

"Data.Tree" exports 'drawTree' and 'drawForest', which provide 2D
pretty-printing of rose-trees, but in a left-to-right fashion.

The functions here draw trees more \"naturally\" in a top-down fashion.

For example, consider the following tree:

> tree :: Tree String
> tree = Node "hello" [ Node "foo" []
>                     , Node "bars" [ Node "oi!" []
>                                   , Node "baz" [ Node "a" [ Node "b" []
>                                                           , Node "c" []]
>                                                , Node "d" [ Node "e" []]]]
>                     , Node "foobar" []]

Comparing 'drawTree' and 'drawVerticalTree':

>>> putStrLn $ drawTree tree
hello
|
+- foo
|
+- bars
|  |
|  +- oi!
|  |
|  `- baz
|     |
|     +- a
|     |  |
|     |  +- b
|     |  |
|     |  `- c
|     |
|     `- d
|        |
|        `- e
|
`- foobar

>>> putStrLn $ drawVerticalTree
          hello
            |
  -------------------
 /        |          \
foo      bars      foobar
          |
       ------
      /      \
     oi!    baz
             |
            ----
           /    \
           a    d
           |    |
           --   e
          /  \
          b  c

Also consider the @Diagrams.TwoD.Layout.Tree@ module from
/diagrams-contrib/ for actual image rendering of rose-trees:
<http://hackage.haskell.org/package/diagrams-contrib>

 -}
module Data.Tree.Pretty
       ( -- * Drawing trees
         drawVerticalTree
       , drawVerticalTreeWith
         -- * Drawing forests
       , drawVerticalForest
       , drawVerticalForestWith
         -- * Widths of gaps between trees.
       , Width
       , defaultGap
         -- * Helper functions
       , treeToBox
       ) where

import Data.Tree
import Data.List(intersperse)
import Text.PrettyPrint.Boxes
import Control.Monad(ap, liftM2)

-- -----------------------------------------------------------------------------

-- | Draw a tree top-down.
drawVerticalTree :: Tree Box -> String
drawVerticalTree = drawVerticalTreeWith defaultGap

-- | Draw a tree top-down using the specified gap between sub-trees.
drawVerticalTreeWith    :: Width -> Tree Box -> String
drawVerticalTreeWith gp = render . treeToBox gp

-- | Draw a forest with each tree being top-down.
drawVerticalForest :: Forest Box -> String
drawVerticalForest = drawVerticalForestWith defaultGap

-- | Draw a forest with each tree being top-down and the specified
--    horizontal gap between trees.
drawVerticalForestWith    :: Width -> Forest Box -> String
drawVerticalForestWith gp = render . hsep gp top . map (treeToBox gp)

checkGap :: Width -> Width
checkGap = max 1

-- | This is exported in case you want to do further pretty-printing
--   using "Text.PrettyPrint.Boxes".
treeToBox :: Width -> Tree Box -> Box
treeToBox = liftM2 (.) treeBox addWidthTree . checkGap

-- | The size of the gap to use.  It is recommended that you use a
--   value @>=2@ for best results (with @2@ being the default).
type Width = Int

defaultGap :: Width
defaultGap = 2

-- -----------------------------------------------------------------------------
-- Pre-processing

-- | We need to know how wide the tree is.
data WidthLabel = WL { trWidth :: Width
                     , numSub  :: Int
                     , label   :: Box
                     }
                deriving (Show)

type WidthTree = Tree WidthLabel
type WidthForest = Forest WidthLabel

addWidthTree :: Width -> Tree Box -> WidthTree
addWidthTree gp (Node str ts) = Node (WL w ns str) ts'
  where
    ts' = addWidthsForest gp ts
    ns = length ts
    w = cols str `max` forestWidth gp ts'

addWidthsForest    :: Width -> Forest Box -> WidthForest
addWidthsForest gp = map (addWidthTree gp)

-- -----------------------------------------------------------------------------
-- Drawing

treeBox :: Width -> WidthTree -> Box
treeBox gp (Node lbl ts)
  = case ts of
      []  -> lbl'
             -- Three vLines to get the gap right.
      [t] -> vcat' [lbl', vLine] // treeBox gp t
      _   -> moveRight (iniGp + 1) (vcat' [lbl', vLine, ln]) // ts'
  where
    numTs = numSub lbl

    lbl' = label lbl

    lnWidth = sum (interTreeSpacing gp ts) + numTs -- + the vertical lines

    iniGp = (`div` 2) . pred . trWidth . rootLabel $ head ts

    ln = hcat top (replicate (lnWidth - 2) hLine)

    ts' = hsep gp top $ zipWith subT [1..] ts

    subT n t = vcat' [vln, treeBox gp t]
      where
        vln | n == 1     = lBranch
            | n == numTs = rBranch
            | otherwise  = vLine

treeWidth :: WidthTree -> Width
treeWidth = trWidth . rootLabel

forestWidth    :: Width -> WidthForest -> Width
forestWidth gp = sum . intersperse gp . map treeWidth

-- | The width between the vertical lines coming into neighbouring sub-trees.
interTreeSpacing    :: Width -> WidthForest -> [Width]
interTreeSpacing gp = (zipWith go `ap` tail) . map (pred . trWidth . rootLabel)
  where
    go l r = (l `divUp` 2) + gp + (r `div` 2)

-- -----------------------------------------------------------------------------

vLine :: Box
vLine = char '|'

hLine :: Box
hLine = char '-'

lBranch :: Box
lBranch = char '/'

rBranch :: Box
rBranch = char '\\'

divUp :: Int -> Int -> Int
a `divUp` b = negate $ (-a) `div` b

vcat' :: [Box] -> Box
vcat' = vcat center1
