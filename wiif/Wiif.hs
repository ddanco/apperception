module Main where

import qualified Data.Set as Set

import System.Environment
import System.IO
-- import Text.JSON.Generic
import WiifData

-----------------------------------------------------------
------------------- Reporting options ---------------------
-----------------------------------------------------------

check_causal_rule :: Trace -> CausalRule -> IO ()
check_causal_rule (x:(y:ys)) r =
    if (start r) `elem` (readings x) then
        if not ((end r) `elem` (readings y)) then
            putStrLn $ "# RULE INVALID: " ++ (print_causal_rule r) ++
                        " between timesteps " ++ (show (time x)) ++
                        " and " ++ (show (time y))
        else check_causal_rule (y:ys) r
    else check_causal_rule (y:ys) r
-- Do we want a trace of len 1 to pass a causal rule? Loop around?
-- For now, for simplicity, yes.
check_causal_rule _ r = putStrLn $ "Rule valid: " ++ (print_causal_rule r)

check_causal_rules :: Trace -> [CausalRule] -> IO ()
check_causal_rules t [] = putStrLn $ ""
check_causal_rules t (x:xs) = check_causal_rule t x >>
                                    check_causal_rules t xs

check_arrow_rule :: Trace -> ArrowRule -> IO ()
check_arrow_rule (x:xs) r =
    if all (`elem` (readings x)) (premises r) then
        if not ((conclusion r) `elem` (readings x)) then
            putStrLn $ "# RULE INVALID: " ++ (print_arrow_rule r) ++
                        " at timestep " ++ (show (time x))
        else check_arrow_rule xs r
    else check_arrow_rule xs r
-- Do we want a trace of len 1 to pass a causal rule? Loop around?
-- For now, for simplicity, yes.
check_arrow_rule _ r = putStrLn $ "Rule valid: " ++ (print_arrow_rule r)

check_arrow_rules :: Trace -> [ArrowRule] -> IO ()
check_arrow_rules t [] = putStrLn $ ""
check_arrow_rules t (x:xs) = check_arrow_rule t x >>
                                check_arrow_rules t xs

has_duplicates :: (Ord a) => [a] -> Bool
has_duplicates list = length list /= length set
  where set = Set.fromList list

xor_step_valid :: [Reading] -> XOrRule -> Bool
xor_step_valid xs r =
    let rel_readings = filter (\x -> (value x) `elem` (values r)) xs in
        let sensors = [(sensor r) | r <- rel_readings] in
            not (has_duplicates sensors)

check_xor_rule :: Trace -> XOrRule -> IO ()
check_xor_rule [] r = putStrLn $ "Constraint valid: " ++ (print_xor_rule r)
check_xor_rule (x:xs) r =
    if not (xor_step_valid (readings x) r) then
        putStrLn $ "# CONSTRAINT INVALID: " ++ (print_xor_rule r) ++
                    " at timestep " ++ (show (time x))
    else check_xor_rule xs r
    -- get all readings with xorxs. number of sensors = number of xssfd

check_xor_rules :: Trace -> [XOrRule] -> IO ()
check_xor_rules t [] = putStrLn $ ""
check_xor_rules t (x:xs) = check_xor_rule t x >>
                            check_xor_rules t xs

-----------------------------------------------------------
--------------------- Boolean options ---------------------
-----------------------------------------------------------

arrow_rule_valid :: Trace -> ArrowRule -> Bool
arrow_rule_valid [] _ = True
arrow_rule_valid (x:xs) r =
    if all (`elem` (readings x)) (premises r) then
        (conclusion r) `elem` (readings x) && arrow_rule_valid xs r
    else arrow_rule_valid xs r

arrow_rules_valid :: Trace -> [ArrowRule] -> Bool
arrow_rules_valid t [] = True
arrow_rules_valid t (x:xs) = arrow_rule_valid t x &&
                                arrow_rules_valid t xs

causal_rules_valid :: Trace -> [CausalRule] -> Bool
causal_rules_valid t [] = True
causal_rules_valid t (x:xs) = causal_rule_valid t x &&
                                causal_rules_valid t xs

causal_rule_valid :: Trace -> CausalRule -> Bool
causal_rule_valid (x:(y:ys)) r =
    if (start r) `elem` (readings x) then
        (end r) `elem` (readings y) && causal_rule_valid (y:ys) r
    else causal_rule_valid (y:ys) r


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
            run_tests
        _ -> do
            putStrLn $ "Usage: wiif <trace-file> <target-file>"


-- Should fail (?), rule broken
test_1 :: IO ()
test_1 = do
    let trace = wiif_predict_1
    let crs = [causal_rule_predict_1_1]
    let ars = [] --[arrow_rule_predict_1_1]
    let xrs = []
    test_rules trace crs ars xrs

-- Should pass
test_2 :: IO ()
test_2 = do
    let trace = wiif_predict_1
    let crs = [causal_rule_predict_1_2]
    let ars = [] --[arrow_rule_predict_1_2]
    let xrs = []
    test_rules trace crs ars xrs

-- Exo rules -- bad trace
test_3 :: IO ()
test_3 = do
    let trace = wiif_exog_1_wrong
    let crs = [causal_rule_exog_1_2]
    test_rules trace crs [] []

-- Exo rules -- right trace
test_4 :: IO ()
test_4 = do
    let trace = wiif_exog_1_correct
    let crs = [causal_rule_exog_1_2]
    test_rules trace crs [] []

-- Exo rules -- right trace, var mismatch
test_5 :: IO ()
test_5 = do
    let trace = wiif_exog_1_correct
    let crs = [causal_rule_exog_1_1]
    test_rules trace crs [] []

test_rules :: Trace -> [CausalRule] -> [ArrowRule] -> [XOrRule] -> IO ()
test_rules t cs as xs = do
    putStrLn $ "Running test ...\n"
    check_causal_rules t cs
    check_arrow_rules t as
    check_xor_rules t xs
    putStrLn $ "=============\n"

test_causal_rules :: Trace -> [CausalRule] -> IO ()
test_causal_rules t cs =
    check_causal_rules t cs

test_arrow_rules :: Trace -> [ArrowRule] -> IO ()
test_arrow_rules t as =
    check_arrow_rules t as

test_xor_rules :: Trace -> [XOrRule] -> IO ()
test_xor_rules t xs =
    check_xor_rules t xs

run_tests :: IO ()
run_tests =
    test_1 >> test_2 >> test_3 >>
    test_4 >> test_5
