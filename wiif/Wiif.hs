module Main where

import System.Environment
import System.IO
-- import Text.JSON.Generic

type Trace = [TimeStep]

data TimeStep = TimeStep {
    time :: Int,
    readings :: [Reading]
} -- deriving (Show, Data, Typeable)

data Reading =  Reading {
    sensor :: String,
    value :: String
} deriving (Eq, Show)-- deriving (Show, Data, Typeable)

data CausalRule = CausalRule {
    -- Conditions e.g. "on" "off",
    -- Maybe make datatype more sophisticated
    start :: String,
    end :: String
}

data ArrowRule = ArrowRule {
    premises :: [Reading],
    conclusion :: Reading
}

-- ========================= Data ======================

wiif_predict_1 :: Trace
wiif_predict_1 = [
    TimeStep {
        time = 1,
        readings = [Reading {sensor="light", value="on"},
                    Reading {sensor="intensity", value="bright"}]
    },
    TimeStep {
        time = 2,
        readings = [Reading {sensor="light", value="off"}]
    },
    TimeStep {
        time = 3,
        readings = [Reading {sensor="light", value="on"},
                    Reading {sensor="intensity", value="bright"}]
    },
    TimeStep {
        time = 4,
        readings = [Reading {sensor="light", value="off"}]
    },
    TimeStep {
        time = 5,
        readings = [Reading {sensor="light", value="on"},
                    Reading {sensor="intensity", value="bright"}]
    },
    TimeStep {
        time = 6,
        readings = [Reading {sensor="light", value="off"}]
    }]

causal_rule_predict_1_1 :: CausalRule
causal_rule_predict_1_1 = CausalRule {
    start = "on",
    end = "on"
}

causal_rule_predict_1_2 :: CausalRule
causal_rule_predict_1_2 = CausalRule {
    start = "on",
    end = "off"
}

arrow_rule_predict_1_1 :: ArrowRule
arrow_rule_predict_1_1 = ArrowRule {
    premises = [Reading {sensor="light", value="off"}],
    conclusion = Reading {sensor="intensity", value="bright"}
}

arrow_rule_predict_1_2 :: ArrowRule
arrow_rule_predict_1_2 = ArrowRule {
    premises = [Reading {sensor="light", value="on"}],
    conclusion = Reading {sensor="intensity", value="bright"}
}

-- =====================================================

-- data Rule = CausalRule | ArrowRule
-- data Rule = CausalRule String String |
--             ArrowRule [String] String deriving (Eq, Show)

type Sensor = String -- Maybe overkill.

relevant_reading :: [Reading] -> Sensor -> Maybe Reading
relevant_reading [] _ = Nothing
-- We only take the first reading that matches. There shouldn't be
-- multiple different readings for one sensor at same timestamp.
-- Future: validate this in code.
relevant_reading (x:xs) v = if sensor x == v then Just x
                            else relevant_reading xs v

relevant_sensor :: [Reading] -> CausalRule -> Maybe Sensor
relevant_sensor [] _ = Nothing
-- Same deal here, assuming there's only one reading for a given sensor.
-- Should do validation separately.
relevant_sensor (x:xs) r =  if value x == start r then Just (sensor x)
                            else relevant_sensor xs r

causal_step_valid :: CausalRule -> Sensor -> TimeStep -> Bool
causal_step_valid rule v t =
    case relevant_reading (readings t) v of
        Just reading -> end rule == (value reading)
        Nothing -> False

causal_rule_valid :: Trace -> CausalRule -> Bool
causal_rule_valid (x:(y:ys)) r =
    case relevant_sensor (readings x) r of
        Just v -> do
            causal_step_valid r v y && causal_rule_valid (y:ys) r
        Nothing -> causal_rule_valid (y:ys) r
-- Do we want a trace of len 1 to pass a causal rule? Loop around?
-- For now, for simplicity, yes.
causal_rule_valid _ _ = True

causal_rules_valid :: Trace -> [CausalRule] -> Bool
causal_rules_valid t [] = True
causal_rules_valid t (x:xs) = causal_rule_valid t x &&
                                causal_rules_valid t xs

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

-----------------------------------------------
--------- Can't get Rule type properly working
-----------------------------------------------

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
        [trace, target] -> do
            -- contents <- readFile trace
            -- putStrLn $ contents
            let should_pass = False
            let trace = wiif_predict_1
            if should_pass then do
                let causal_rules = [causal_rule_predict_1_2]
                let arrow_rules = [arrow_rule_predict_1_2]
                test_rules trace causal_rules arrow_rules
            else do
                let causal_rules = [causal_rule_predict_1_1]
                let arrow_rules = [arrow_rule_predict_1_1]
                test_rules trace causal_rules arrow_rules
        _ -> do
            putStrLn $ "Usage: wiif <trace-file> <target-file>"


test_causal_rules :: Trace -> [CausalRule] -> IO ()
test_causal_rules t cs =
    if causal_rules_valid t cs then
        putStrLn $ "Causal rules valid"
    else putStrLn $ "Causal rules invalid"

test_arrow_rules :: Trace -> [ArrowRule] -> IO ()
test_arrow_rules t as =
    if arrow_rules_valid t as then
        putStrLn $ "Arrow rules valid"
    else putStrLn $ "Arrow rules invalid"

test_rules :: Trace -> [CausalRule] -> [ArrowRule] -> IO ()
test_rules t cs as =
    test_causal_rules t cs >>
    test_arrow_rules t as
