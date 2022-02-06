module DebuggerData where

import qualified Data.List as List

-----------------------------------------------------------
-------------------------- Types --------------------------
-----------------------------------------------------------

-- Rule types are more built out than Interpretation.hs.
-- There, the atoms are just strings, and we need easier-to-handle
-- datatypes to be able to judge validity of the rules.

type Trace = [TimeStep]

printTrace :: Trace -> IO ()
printTrace t =
    mapM_ (\x -> printTimeStep x) t

data TimeStep = TimeStep {
    time :: Int,
    readings :: [Reading]
} deriving (Eq, Show)

printTimeStep :: TimeStep -> IO ()
printTimeStep t = do
    putStrLn $ "Time : " ++ (show (time t)) ++
                "      Readings : " ++
                (List.intercalate "  " (readingsString (readings t)))

data Reading =  Reading {
    sensor :: String,
    value :: String
} deriving (Eq, Show)

readingsString :: [Reading] -> [String]
readingsString [] = []
readingsString (x:xs) = ((readingString x):(readingsString xs))

readingString :: Reading -> String
readingString r = (show (sensor r)) ++ "(" ++ (show (value r)) ++ ")"

-- FIXME: Determine if this is correct datatype...handle vars correctly.
data CausalRule = CausalRule {
    start :: Reading,
    end :: Reading
} deriving (Eq, Show)

causalRuleString :: CausalRule -> String
causalRuleString r = "s(" ++ (show (value (start r))) ++ ",var x) >> " ++
                        "s(" ++ (show (value (end r))) ++ ",var x)"

data ArrowRule = ArrowRule {
    premises :: [Reading],
    conclusion :: Reading
}

arrowRuleString :: ArrowRule -> String
-- FIXME: Make pretty
arrowRuleString r = (show (premises r)) ++ " -> " ++ (show (conclusion r))

data XOrConstraint = XOrConstraint {
    -- More elaborate type?
    values :: [String]
}

xorString :: XOrConstraint -> String
xorString r = List.intercalate "+" (values r)

-----------------------------------------------------------
------- Demonstation of 'complex' rules -------------------
-------- (unrealistic for this setup) ---------------------
-----------------------------------------------------------

data CausalRule2 = CausalRule2 {
    start_cond :: Reading -> Bool,
    end_cond :: Reading -> Bool
} -- deriving (Eq, Show)
