import Language.Haskell.TH
main = do
    e <- runQ [|1|]
    putStrLn (show e)
    putStrLn (p6 e)

p6 (LitE x) = lit6 x
p6 _ = "!!!"
lit6 (IntegerL i) = show i
lit6 _ = "!!!"
