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
} -- deriving (Show, Data, Typeable)

data CausalRule = CausalRule {
    -- Conditions e.g. "on" "off",
    -- Maybe make datatype more sophisticated
    start :: String,
    end :: String
}

data ArrowRule = ArrowRule {
    premise :: [String],
    conclusion :: String
}

-- ========================= Data ======================

wiif_1 :: Trace
wiif_1 = [
    TimeStep {
        time = 1,
        readings = [Reading {sensor="light", value="on"}]
    },
    TimeStep {
        time = 2,
        readings = [Reading {sensor="light", value="on"}]
    },
    TimeStep {
        time = 3,
        readings = [Reading {sensor="light", value="on"}]
    },
    TimeStep {
        time = 4,
        readings = [Reading {sensor="light", value="on"}]
    },
    TimeStep {
        time = 5,
        readings = [Reading {sensor="light", value="on"}]
    },
    TimeStep {
        time = 6,
        readings = [Reading {sensor="light", value="on"}]
    }]

causal_rule_1 :: CausalRule
causal_rule_1 = CausalRule {
    start = "on",
    end = "off"
}

causal_rule_2 :: CausalRule
causal_rule_2 = CausalRule {
    start = "on",
    end = "on"
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

causal_rule_valid :: Trace -> CausalRule -> Bool
causal_rule_valid (x:(y:ys)) r =
    case relevant_sensor (readings x) r of
        Just v -> do
            causal_step_valid r v y && causal_rule_valid (y:ys) r
        Nothing -> causal_rule_valid (y:ys) r
-- Do we want a trace of len 1 to pass a causal rule? Loop around?
-- For now, for simplicity, yes.
causal_rule_valid _ _ = True

causal_step_valid :: CausalRule -> Sensor -> TimeStep -> Bool
causal_step_valid rule v t =
    case relevant_reading (readings t) v of
        Just reading -> end rule == (value reading)
        Nothing -> False

causal_rules_valid :: Trace -> [CausalRule] -> Bool
causal_rules_valid t [] = True
causal_rules_valid t (x:xs) = causal_rule_valid t x &&
                                causal_rules_valid t xs

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
            let trace = wiif_1
            let causal_rules = [causal_rule_2]
            if causal_rules_valid trace causal_rules then
                putStrLn $ "Causal rules valid"
            else putStrLn $ "Causal rules invalid"
            
        _ -> do
            putStrLn $ "Usage: wiif <trace-file> <target-file>"
