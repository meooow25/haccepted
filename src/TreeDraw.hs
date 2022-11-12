{-|
Draws trees, primarily for debugging.

draw
Draws a tree. Like Data.Tree.drawTree but works with any type, can show edge labels and uses
unicode box characters.
Example:
a
├── 1 ── b
├── 2 ── c
│        ├── d
│        └── e
└── 3 ── f
-}

module TreeDraw
    ( draw
    ) where

draw :: (a -> String) -> (a -> [(Maybe String, a)]) -> a -> String
draw showa edges = unlines . go where
    go a = showa a : drawCh (edges a) where
        drawCh []     = []
        drawCh [e]    = shift "└" " " (f e)
        drawCh (e:es) = shift "├" "│" (f e) ++ drawCh es
        f (e, v) = shift label padding (go v) where
            label = edge e
            padding = replicate (length label) ' '
    edge Nothing  = "── "
    edge (Just s) = "── " ++ s ++ " ── "
    shift first other = zipWith (++) (first : repeat other)
