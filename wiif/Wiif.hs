module Main where

import qualified Data.Set as Set

import System.Environment
import System.IO
-- import Text.JSON.Generic
import WiifData

-----------------------------------------------------------
------------------- Reporting options ---------------------
-----------------------------------------------------------

checkCausalRule :: Trace -> CausalRule -> IO ()
checkCausalRule (x:(y:ys)) r =
    if (start r) `elem` (readings x) then
        if not ((end r) `elem` (readings y)) then
            putStrLn $ "# RULE INVALID: " ++ (printCausalRule r) ++
                        " between timesteps " ++ (show (time x)) ++
                        " and " ++ (show (time y))
        else checkCausalRule (y:ys) r
    else checkCausalRule (y:ys) r
-- Do we want a trace of len 1 to pass a causal rule? Loop around?
-- For now, for simplicity, yes.
checkCausalRule _ r = putStrLn $ "Rule valid: " ++ (printCausalRule r)

checkCausalRules :: Trace -> [CausalRule] -> IO ()
checkCausalRules t [] = putStrLn $ ""
checkCausalRules t (x:xs) = checkCausalRule t x >>
                                    checkCausalRules t xs

checkArrowRule :: Trace -> ArrowRule -> IO ()
checkArrowRule (x:xs) r =
    if all (`elem` (readings x)) (premises r) then
        if not ((conclusion r) `elem` (readings x)) then
            putStrLn $ "# RULE INVALID: " ++ (printArrowRule r) ++
                        " at timestep " ++ (show (time x))
        else checkArrowRule xs r
    else checkArrowRule xs r
-- Do we want a trace of len 1 to pass a causal rule? Loop around?
-- For now, for simplicity, yes.
checkArrowRule _ r = putStrLn $ "Rule valid: " ++ (printArrowRule r)

checkArrowRules :: Trace -> [ArrowRule] -> IO ()
checkArrowRules t [] = putStrLn $ ""
checkArrowRules t (x:xs) = checkArrowRule t x >>
                                checkArrowRules t xs

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

xorStepValid :: [Reading] -> XOrRule -> Bool
xorStepValid xs r =
    -- FIXME: This will also fail if a sensor is given the same reading
    -- twice in the same timestamp. I don't see why this should happen
    -- anyways, but it shouldn't fail.
    let relReadings = filter (\x -> (value x) `elem` (values r)) xs in
        let sensors = [(sensor r) | r <- relReadings] in
            not (hasDuplicates sensors)

checkXorRule :: Trace -> XOrRule -> IO ()
checkXorRule [] r = putStrLn $ "Constraint valid: " ++ (printXorRule r)
checkXorRule (x:xs) r =
    if not (xorStepValid (readings x) r) then
        putStrLn $ "# CONSTRAINT INVALID: " ++ (printXorRule r) ++
                    " at timestep " ++ (show (time x))
    else checkXorRule xs r

checkXorRules :: Trace -> [XOrRule] -> IO ()
checkXorRules t [] = putStrLn $ ""
checkXorRules t (x:xs) = checkXorRule t x >>
                            checkXorRules t xs


----------------------- Main --------------------------


main :: IO ()
main = do
    args <- getArgs
    case args of
        -- verbosity argument?
        [trace, target] -> do
            -- TODO: Add file support
            -- contents <- readFile trace
            -- putStrLn $ contents
            runTests
        _ -> do
            putStrLn $ "Usage: wiif <trace-file> <target-file>"


-- Should fail (?), rule broken
test_1 :: IO ()
test_1 = do
    let trace = wiif_predict_1
    let crs = [causal_rule_predict_1_1]
    let ars = [] --[arrow_rule_predict_1_1]
    let xrs = []
    testRules trace crs ars xrs

-- Should pass
test_2 :: IO ()
test_2 = do
    let trace = wiif_predict_1
    let crs = [causal_rule_predict_1_2]
    let ars = [] --[arrow_rule_predict_1_2]
    let xrs = []
    testRules trace crs ars xrs

-- Exo rules -- bad trace
test_3 :: IO ()
test_3 = do
    let trace = wiif_exog_1_wrong
    let crs = [causal_rule_exog_1_2]
    testRules trace crs [] []

-- Exo rules -- right trace
test_4 :: IO ()
test_4 = do
    let trace = wiif_exog_1_correct
    let crs = [causal_rule_exog_1_2]
    testRules trace crs [] []

-- Exo rules -- right trace, var mismatch
test_5 :: IO ()
test_5 = do
    let trace = wiif_exog_1_correct
    let crs = [causal_rule_exog_1_1]
    testRules trace crs [] []

testRules :: Trace -> [CausalRule] -> [ArrowRule] -> [XOrRule] -> IO ()
testRules t cs as xs = do
    putStrLn $ "Running test ...\n"
    putStrLn $ "Trace:\n"
    printTrace t
    -- putStrLn $ "\n Rules:\n"
    -- mapM_ (\x -> print_causal_rule x) cs
    -- mapM_ (\x -> print_arrow_rule x) as
    -- mapM_ (\x -> print_xor_rule x) xs
    putStrLn $ "\nResults:\n"
    checkCausalRules t cs
    checkArrowRules t as
    checkXorRules t xs
    putStrLn $ "=============\n"

testCausalRules :: Trace -> [CausalRule] -> IO ()
testCausalRules t cs =
    checkCausalRules t cs

testArrowRules :: Trace -> [ArrowRule] -> IO ()
testArrowRules t as =
    checkArrowRules t as

testXorRules :: Trace -> [XOrRule] -> IO ()
testXorRules t xs =
    checkXorRules t xs

runTests :: IO ()
runTests =
    test_1 >> test_2 >> test_3 >>
    test_4 -- >> test_5 -- TODO: Update args for var tests
