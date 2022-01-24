module Main where

import qualified Data.List as List
import qualified System.Environment as Environment

import AttentionData

--------------------- Main ---------------------

main :: IO()
main = do
    args <- Environment.getArgs
    putStrLn $ "Generating attention task for " ++ concat (List.intersperse " " args)
    case args of
        [w, t] -> do
            let f = "data_misc/input_attn_" ++ w ++ ".lp"
            putStrLn $ "Generating file " ++ f
            gen_attn f w (read t)
        _ -> error "Not implemented"

gen_attn :: String -> String -> Int -> IO ()
gen_attn f w_s t = error "Not implemented"
-- gen_attn f w_s t = case lookup w_s attn_worlds of
--     Just w -> write_attn_task f w t
--     Nothing -> error "World not found"
