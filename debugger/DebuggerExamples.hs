module DebuggerExamples where

import DebuggerData

-----------------------------------------------------------
------------------------ Examples -------------------------
-----------------------------------------------------------

debug_predict_1 :: Trace
debug_predict_1 = [
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

debug_exog_1_wrong :: Trace
debug_exog_1_wrong = [
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

debug_exog_1_correct :: Trace
debug_exog_1_correct = [
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

-----------------------------------------------------------
---------------------- Rhythm -----------------------------
-----------------------------------------------------------

debug_rhythm_correct :: Trace
debug_rhythm_correct = [
    TimeStep {
        time = 1,
        readings = [Reading {sensor="bass", value="3"},
                    Reading {sensor="snare", value="0"},
                    Reading {sensor="hihat", value="3"}]
    },
    TimeStep {
        time = 2,
        readings = [Reading {sensor="bass", value="2"},
                    Reading {sensor="snare", value="0"},
                    Reading {sensor="hihat", value="2"}]
    },
    TimeStep {
        time = 3,
        readings = [Reading {sensor="bass", value="1"},
                    Reading {sensor="snare", value="0"},
                    Reading {sensor="hihat", value="3"}]
    },
    TimeStep {
        time = 4,
        readings = [Reading {sensor="bass", value="0"},
                    Reading {sensor="snare", value="0"},
                    Reading {sensor="hihat", value="2"}]
    },
    TimeStep {
        time = 5,
        readings = [Reading {sensor="bass", value="3"},
                    Reading {sensor="snare", value="3"},
                    Reading {sensor="hihat", value="3"}]
    }
    ]

debug_rhythm_incorrect :: Trace
debug_rhythm_incorrect = [
    TimeStep {
        time = 1,
        readings = [Reading {sensor="bass", value="3"},
                    Reading {sensor="snare", value="0"},
                    Reading {sensor="hihat", value="3"}]
    },
    TimeStep {
        time = 2,
        readings = [Reading {sensor="bass", value="2"},
                    Reading {sensor="snare", value="0"},
                    Reading {sensor="hihat", value="2"}]
    },
    TimeStep {
        time = 3,
        readings = [Reading {sensor="bass", value="1"},
                    Reading {sensor="snare", value="0"},
                    Reading {sensor="hihat", value="2"}]
    },
    TimeStep {
        time = 4,
        readings = [Reading {sensor="bass", value="0"},
                    Reading {sensor="snare", value="0"},
                    Reading {sensor="hihat", value="3"}]
    },
    TimeStep {
        time = 5,
        readings = [Reading {sensor="bass", value="3"},
                    Reading {sensor="snare", value="3"},
                    Reading {sensor="hihat", value="2"}]
    }
    ]

causal_rule_rhythm_1_1 :: CausalRule2
causal_rule_rhythm_1_1 = CausalRule2 {
    start_cond = \x -> (sensor x) == "hihat" && (isntLoudest (value x)),
    end_cond = \x -> (sensor x) == "hihat" && (isLoudest (value x))
}

-- Rule generated for predict_EighthNodeDrumBeat
------------------------------------------------
-- % s_hihat ^ succ(l, l2) ^ s_l_loud >> s_l2_loud
-- % if hihat isnt the loudest, then next step it's the loudest
-- r11 : isa(p_is_hi_hat,var_s) /\ isa2(p_succ,var_l,var_l2) /\ s2(c_loudness,var_s,var_l) >> s2(c_loudness, var_s, var_l2)

isLoudest :: String -> Bool
isLoudest s = (s == "3")

isntLoudest :: String -> Bool
isntLoudest s = not (isLoudest s)
