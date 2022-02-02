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
    -- get all readings with xorxs. number of sensors = number of xssfd

checkXorRules :: Trace -> [XOrRule] -> IO ()
checkXorRules t [] = putStrLn $ ""
checkXorRules t (x:xs) = checkXorRule t x >>
                            checkXorRules t xs

-----------------------------------------------------------
--------------------- Boolean options ---------------------
-----------------------------------------------------------

arrowRuleValid :: Trace -> ArrowRule -> Bool
arrowRuleValid [] _ = True
arrowRuleValid (x:xs) r =
    if all (`elem` (readings x)) (premises r) then
        (conclusion r) `elem` (readings x) && arrowRuleValid xs r
    else arrowRuleValid xs r

arrowRulesValid :: Trace -> [ArrowRule] -> Bool
arrowRulesValid t [] = True
arrowRulesValid t (x:xs) = arrowRuleValid t x &&
                                arrowRulesValid t xs

causalRulesValid :: Trace -> [CausalRule] -> Bool
causalRulesValid t [] = True
causalRulesValid t (x:xs) = causalRuleValid t x &&
                                causalRulesValid t xs

causalRuleValid :: Trace -> CausalRule -> Bool
causalRuleValid (x:(y:ys)) r =
    if (start r) `elem` (readings x) then
        (end r) `elem` (readings y) && causalRuleValid (y:ys) r
    else causalRuleValid (y:ys) r


-- relevant_reading :: [Reading] -> Sensor -> Maybe Reading
-- relevant_reading [] _ = Nothing
-- -- We only take the first reading that matches. There shouldn't be
-- -- multiple different readings for one sensor at same timestamp.
-- -- Future: validate this in code.
-- relevant_reading (x:xs) v = if sensor x == v then Just x
--                             else relevant_reading xs v

-- relevant_sensor :: [Reading] -> CausalRule -> Maybe Sensor
-- relevant_sensor [] _ = Nothing
-- -- Same deal here, assuming there's only one reading for a given sensor.
-- -- Should do validation separately.
-- relevant_sensor (x:xs) r =  if value x == start r then Just (sensor x)
--                             else relevant_sensor xs r

-- causal_step_valid :: CausalRule -> Sensor -> TimeStep -> Bool
-- causal_step_valid rule v t =
--     case relevant_reading (readings t) v of
--         Just reading -> end rule == (value reading)
--         Nothing -> False

-- causal_rule_valid :: Trace -> CausalRule -> Bool
-- causal_rule_valid (x:(y:ys)) r =
--     case relevant_sensor (readings x) r of
--         Just v -> do
--             causal_step_valid r v y && causal_rule_valid (y:ys) r
--         Nothing -> causal_rule_valid (y:ys) r
-- -- Do we want a trace of len 1 to pass a causal rule? Loop around?
-- -- For now, for simplicity, yes.
-- causal_rule_valid _ _ = True

-- -- Can't get Rule type properly working
-- rules_valid :: Trace -> [Rule] -> Bool
-- rules_valid _ [] = True
-- rules_valid t (x:xs) = rule_valid t x && rules_valid t xs

-- rule_valid :: Trace -> Rule -> Bool
-- rule_valid t (CausalRule s e) = causal_rule_valid t (CR s e)
-- rule_valid _ _ = error "Not implemented"


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
