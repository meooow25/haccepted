module TreeDebugUtil where

draw :: (a -> String) -> (a -> [(String, a)]) -> a -> String
draw showa nxt = unlines . go where
    go a = showa a : drawSubTrees (nxt a) where
        drawSubTrees []     = []
        drawSubTrees [e]    = shift "└" " " (f e)
        drawSubTrees (e:es) = shift "├" "│" (f e) ++ drawSubTrees es
        f (b, v) = shift label padding (go v) where
            label = "── " ++ b ++ " ── "
            padding = replicate (length label) ' '
    shift first other = zipWith (++) (first : repeat other)
