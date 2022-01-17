module Main where

frame_attn_1 :: Frame
frame_attn_1 = Frame {
    types = [T "sensor_1", T "cell", T "grid", T "object"],
    type_hierarchy = [
        (T "object", [T "cell", T "grid"]),
        (T "cell", [T "sensor_1"])
    ],
    objects = [
        (O "sensor_a", T "sensor_1"),
        (O "grid", T "grid")
    ],
    exogeneous_objects = [],
    permanent_concepts = [],
    fluid_concepts = [
        (C "on", [T "sensor_1"]),
        (C "off", [T "sensor_1"])
    ],
    input_concepts = [C "on", C "off"],
    static_concepts = [],
    vars = [
        (V "s1", T "sensor_1"),
        (V "c", T "cell"),
        (V "g", T "grid"),
        (V "x", T "object")
    ],
    var_groups = [
        [V "x"],
        [V "s1"],
        [V "c"],
        [V "c", V "g"]
    ],
    aux_files = []
}

template_attn_1 :: Template
template_attn_1 = Template {
    dir = "attn",
    frame = frame_attn_1,
    min_body_atoms = 1,
    max_body_atoms = 1, -- ??
    num_arrow_rules = 0,
    num_causes_rules = 2,
    num_visual_predicates = Nothing,
    use_noise = False,
}