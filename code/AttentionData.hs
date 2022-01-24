module AttentionData where

import Interpretation


-------------------- Templates ------------------------

attn_templates :: [(String, (String, Template, String))]
attn_templates = [
    ("attn_1.lp", ("data/attention", template_attn_1, "attn_1.lp"))
    ]

frame_attn_1 :: Frame
frame_attn_1 = Frame {
    types = [T "object"],
    type_hierarchy = [],
    objects = [
        (O "sensor_a", T "object")
        ],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [
        (C "on", [T "object"]),
        (C "off", [T "object"])
        -- something about position?
        ],
    input_concepts = [C "on", C "off"],
    static_concepts = [],
    vars = [
        (V "x", T "object")
        ],
    var_groups = [
        [V "x"]
        ],
    aux_files = []
}

template_attn_1 :: Template
template_attn_1 = Template {
    dir = "attn",
    frame = frame_attn_1,
    min_body_atoms = 1,
    max_body_atoms = 1, -- ?
    num_arrow_rules = 0,
    num_causes_rules = 4,
    num_visual_predicates = Nothing,
    use_noise = False
    }


----------- live in AttentionData.hs ... --------------

-- data Dir = DirLeft | DirRight deriving (Eq, Ord, Show)

-- data ObjState = OS {
--     index :: Int,
--     x :: Int,
--     y :: Int,
--     attn_dir :: Dir, -- ?
--     state :: Int,
--     max_state :: Int
-- } deriving (Eq, Ord, Show)

-- data World = W {
--     attn_objects :: [ObjState],
--     max_x :: Int,
--     max_y :: Int
-- } deriving (Eq, Ord, Show)

-- obj :: Int -> Int -> Int -> Dir -> Int -> ObjState
-- obj i x y d s = OS {
--     index = i,
--     x = x,
--     y = y,
--     attn_dir = d,
--     state = 1,
--     max_state = s
-- }

-- attn_worlds :: [(String, World)]
-- attn_worlds = [
--     ("w1", w1),
--     ("w1", w2),
--     ("w3", w3),
--     ("w4", w4)
--     ]

-- w1 = W {
--     attn_objects = [
--         obj 1 0 0 DirRight 3,
--         obj 2 1 1 DirLeft 2,
--         obj 3 0 2 DirRight 1
--         ],        
--     max_x = 3,
--     max_y = 3
-- }

-- w2 = W {
--     attn_objects = [
--         obj 1 0 0 DirRight 3,
--         obj 2 3 1 DirLeft 2,
--         obj 3 0 2 DirRight 1
--         ],        
--     max_x = 4,
--     max_y = 3
-- }

-- w3 = W {
--     attn_objects = [
--         obj 1 0 0 DirRight 3,
--         obj 2 4 1 DirLeft 2,
--         obj 3 0 2 DirRight 1
--         ],        
--     max_x = 5,
--     max_y = 3
-- }

-- w4 = W {
--     attn_objects = [
--         obj 1 0 0 DirLeft 3,
--         obj 2 0 1 DirRight 2,
--         obj 3 0 2 DirLeft 1
--         ],        
--     max_x = 5,
--     max_y = 3
-- }

-------------------------------------------------------

-- frame_attn_1 :: Frame
-- frame_attn_1 = Frame {
--     types = [T "sensor_1", T "cell", T "grid", T "object"],
--     type_hierarchy = [
--         (T "object", [T "cell", T "grid"]),
--         (T "cell", [T "sensor_1"])
--     ],
--     Interpretation.objects = [
--         (O "sensor_a", T "sensor_1"),
--         (O "grid", T "grid")
--     ],
--     exogeneous_objects = [],
--     permanent_concepts = [],
--     fluid_concepts = [
--         (C "on", [T "sensor_1"]),
--         (C "off", [T "sensor_1"])
--     ],
--     input_concepts = [C "on", C "off"],
--     static_concepts = [],
--     vars = [
--         (V "s1", T "sensor_1"),
--         (V "c", T "cell"),
--         (V "g", T "grid"),
--         (V "x", T "object")
--     ],
--     var_groups = [
--         [V "x"],
--         [V "s1"],
--         [V "c"],
--         [V "c", V "g"]
--     ],
--     aux_files = []
-- }

-- template_attn_1 :: Template
-- template_attn_1 = Template {
--     dir = "attn",
--     frame = frame_attn_1,
--     min_body_atoms = 1,
--     max_body_atoms = 1, -- ??
--     num_arrow_rules = 0,
--     num_causes_rules = 2,
--     num_visual_predicates = Nothing,
--     use_noise = False
-- }

-- write_attn_task :: String -> World -> Int -> IO ()
-- write_attn_task file w t =
--     writeFile file (unlines (attn_task w t))

-- attn_task :: World -> Int -> [String]
-- -- occlusion_task w t = comments w t ++ trajectory_text w t ++ elements_text w t ++ exclusions_text ++ space_text w
-- attn_task = comments w t ++ "Test"

-- comments :: World -> Int -> [String]
-- comments w t = h ++ concat (map f ps) ++ [header, ""] where
--     ps = zip [1..] traj
--     traj = take t (trajectory w)
--     h = [header, "% Auto-generated from Attention.hs", "%"]
--     f (i, x) = ("% Time " ++ show i ++ ":") : map g (display_world x) ++ ["%"]
--     g l = "% " ++ l
--     header = "%---------------------------------------------------"

-- display_world :: World -> [String]
-- display_world w = List.foldl' update_lines (empty_world w) (attn_objects w)

-- empty_world :: World -> [String]
-- empty_world w = replicate (max_y w) (replicate (max_x w) '.')

-- update_lines :: [String] -> ObjState -> [String]
-- update_lines = ["Test"]
-- -- update_lines w obj = ls1 ++ [l] ++ ls2 where
-- --     ls1 = take (y obj) w
-- --     l = update_line (w !! y obj) obj
-- --     ls2 = drop (y obj + 1) w

