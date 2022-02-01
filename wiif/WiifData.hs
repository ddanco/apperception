module WiifData where


type Trace = [TimeStep]

data TimeStep = TimeStep {
    time :: Int,
    readings :: [Reading]
} deriving (Eq, Show)

data Reading =  Reading {
    sensor :: String,
    value :: String
} deriving (Eq, Show)

-- FIXME: Determine if this is correct datatype...handle vars correctly.
data CausalRule = CausalRule {
    start :: Reading,
    end :: Reading
} deriving (Eq, Show)

print_causal_rule :: CausalRule -> String
print_causal_rule r = "s(" ++ (show (value (start r))) ++ ",var x) >> " ++
                        "s(" ++ (show (value (end r))) ++ ",var x)"

data ArrowRule = ArrowRule {
    premises :: [Reading],
    conclusion :: Reading
}

print_arrow_rule :: ArrowRule -> String
-- FIXME: Make pretty
print_arrow_rule r = (show (premises r)) ++ " -> " ++ (show (conclusion r))

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

wiif_exog_1_wrong :: Trace
wiif_exog_1_wrong = [
    TimeStep {
        time = 1,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 2,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 3,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 4,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 5,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 6,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_bong"}]
    },
    TimeStep {
        time = 7,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 8,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 9,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 10,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 11,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_bong"}]
    },
    TimeStep {
        time = 12,
        readings = [Reading {sensor="obj_1", value="c_on"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 13,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_bong"}]
    },
    TimeStep {
        time = 14,
        readings = [Reading {sensor="obj_1", value="c_on"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 15,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 16,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    }
    ]

wiif_exog_1_correct :: Trace
wiif_exog_1_correct = [
    TimeStep {
        time = 1,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 2,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 3,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 4,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 5,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 6,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_bong"}]
    },
    TimeStep {
        time = 7,
        readings = [Reading {sensor="obj_1", value="c_on"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 8,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 9,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 10,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 11,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_bong"}]
    },
    TimeStep {
        time = 12,
        readings = [Reading {sensor="obj_1", value="c_on"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 13,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_bong"}]
    },
    TimeStep {
        time = 14,
        readings = [Reading {sensor="obj_1", value="c_on"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 15,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    },
    TimeStep {
        time = 16,
        readings = [Reading {sensor="obj_1", value="c_off"},
                    Reading {sensor="exog", value="c_noop"}]
    }
    ]

causal_rule_predict_1_1 :: CausalRule
causal_rule_predict_1_1 = CausalRule {
    start = Reading {sensor="light", value="on"},
    end = Reading {sensor="light", value="on"}
}

causal_rule_predict_1_2 :: CausalRule
causal_rule_predict_1_2 = CausalRule {
    start = Reading {sensor="light", value="on"},
    end = Reading {sensor="light", value="off"}
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

-- Fails: variable name mismatch
causal_rule_exog_1_1 :: CausalRule
causal_rule_exog_1_1 = CausalRule {
    start = Reading {sensor="exog", value="c_bong"},
    end = Reading {sensor="obj_a", value="c_on"}
}

-- Passes with exog_1
causal_rule_exog_1_2 :: CausalRule
causal_rule_exog_1_2 = CausalRule {
    start = Reading {sensor="exog", value="c_bong"},
    end = Reading {sensor="obj_1", value="c_on"}
}

-- =====================================================

-- data Rule = CausalRule | ArrowRule
-- data Rule = CausalRule String String |
--             ArrowRule [String] String deriving (Eq, Show)

type Sensor = String -- Maybe overkill.

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
