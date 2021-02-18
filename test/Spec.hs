import Data.SBV

main :: IO ()
main = putStrLn "Test suite not yet implemented"

sumExample = do
    pc <- sInteger "pc"
    n <- sInteger "n"
    i <- sInteger "i"
    r <- sInteger "r"
    constrain $ pc .== 0
