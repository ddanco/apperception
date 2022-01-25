module Main where

import System.Environment

type Trace = [TimeStep]

data TimeStep = TS {
    id :: Int,
    readings :: [Reading]
}

data Reading = {
    sensor :: String,
    value :: String,
}

data Rule = CausalRule | ArrowRule

data CausalRule = CR {
    -- conditions eg. "on" "off",
    -- maybe make datatype more sophisticated
    start :: String,
    end :: String
}

type Var = String

valid_causal_rule :: Trace -> Rule -> Bool
valid_causal_rule x:(y:ys) r =
    case relevant_var of
        Just v -> do
            causal_step_valid r v y && valid_causal_rule (y:ys) r
        Nothing -> valid_causal_rule (y:ys) r
valid_causal_rule _ = True

causal_step_valid :: CausalRule -> Var -> TimeStep -> Bool
causal_step_valid r v t =
    case relevant_step of
        Just s -> do
            ...
        Nothing -> False


main :: IO ()
main = do
    args <- getArgs
    case args of
        [trace, target] -> do
            putStrLn $ "Yay"
        _ -> do
            putStrLn $ "Usage: wiif <trace-file> <target-file>"