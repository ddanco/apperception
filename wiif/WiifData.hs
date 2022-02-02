module WiifData where

import qualified Data.List as List

-----------------------------------------------------------
-------------------------- Types --------------------------
-----------------------------------------------------------

type Trace = [TimeStep]

printTrace :: Trace -> IO ()
printTrace t =
    mapM_ (\x -> printTimeStep x) t

type Sensor = String -- Maybe overkill.

data TimeStep = TimeStep {
    time :: Int,
    readings :: [Reading]
} deriving (Eq, Show)

printTimeStep :: TimeStep -> IO ()
printTimeStep t = do
    putStrLn $ "Time : " ++ (show (time t)) ++
                "      Readings : " ++
                (List.intercalate "  " (printReadings (readings t)))

data Reading =  Reading {
    sensor :: String,
    value :: String
} deriving (Eq, Show)

printReadings :: [Reading] -> [String]
printReadings [] = []
printReadings (x:xs) = ((printReading x):(printReadings xs))

printReading :: Reading -> String
printReading r = (show (sensor r)) ++ "(" ++ (show (value r)) ++ ")"

-- FIXME: Determine if this is correct datatype...handle vars correctly.
data CausalRule = CausalRule {
    start :: Reading,
    end :: Reading
} deriving (Eq, Show)

printCausalRule :: CausalRule -> String
printCausalRule r = "s(" ++ (show (value (start r))) ++ ",var x) >> " ++
                        "s(" ++ (show (value (end r))) ++ ",var x)"

data ArrowRule = ArrowRule {
    premises :: [Reading],
    conclusion :: Reading
}

printArrowRule :: ArrowRule -> String
-- FIXME: Make pretty
printArrowRule r = (show (premises r)) ++ " -> " ++ (show (conclusion r))

-- data Rule = CausalRule | ArrowRule
-- data Rule = CausalRule String String |
--             ArrowRule [String] String deriving (Eq, Show)

data XOrRule = XOrRule {
    -- More elaborate type?
    values :: [String]
}

printXorRule :: XOrRule -> String
printXorRule r = List.intercalate "+" (values r)

-----------------------------------------------------------
-------------------------- Data ---------------------------
-----------------------------------------------------------

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
