data Expr = Val Int | Div Expr Expr

eval :: Expr -> Maybe Int
eval (Val n)    = Just n
eval (Div x y)  = case eval x of
                    Nothing     -> Nothing
                    Just n      -> case eval y of
                                     Nothing    -> Nothing
                                     Just m     -> safeDiv n m

safeDiv :: Int -> Int -> Maybe Int
safeDiv n m = if m == 0
              then Nothing
              else Just (n `div` m)
