% Tune generated from EighthNoteDrumBeat

% Sensor readings
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_3), 1).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 1).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_3), 1).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_2), 2).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 2).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_2), 2).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_1), 3).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 3).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_3), 3).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 4).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_2), 4).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_3), 5).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_3), 5).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_3), 5).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_2), 6).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_2), 6).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_2), 6).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_1), 7).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_1), 7).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_3), 7).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 8).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_2), 8).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_3), 9).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 9).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_3), 9).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_2), 10).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 10).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_2), 10).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_1), 11).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 11).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_3), 11).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 12).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 12).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_2), 12).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_3), 13).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_3), 13).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_3), 13).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_2), 14).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_2), 14).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_2), 14).
senses(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_1), 15).
senses(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_1), 15).
senses(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_3), 15).
hidden(s2(c_loudness, obj_sensor_bass_drum, obj_loudness_0), 16).
hidden(s2(c_loudness, obj_sensor_snare_drum, obj_loudness_0), 16).
hidden(s2(c_loudness, obj_sensor_hi_hat, obj_loudness_2), 16).

% Elements
is_object(obj_sensor_bass_drum).
is_object(obj_sensor_snare_drum).
is_object(obj_sensor_hi_hat).

is_time(1..16).

% ∃! clause for c_loudness : at most one
:-
	holds(s2(c_loudness, X, Y), T),
	holds(s2(c_loudness, X, Y2), T),
	Y != Y2.

% ∃! clause for c_loudness : at least one
:-
	permanent(isa(t_sensor, X)),
	is_time(T),
	not aux_c_loudness(X, T).

aux_c_loudness(X, T) :-
	holds(s2(c_loudness, X, _), T).

% Incompossibility for p_r
incompossible(s2(c_loudness, X, Y), s2(c_loudness, X, Y2)) :-
	permanent(isa(t_sensor, X)),
	permanent(isa(t_loudness, Y)),
	permanent(isa(t_loudness, Y2)),
	Y != Y2.


% c_val / p_is_0 rules (useless??)
r10 : isa(p_is_0,var_n) /\ isa(p_is_hi_hat,var_s) /\ s2(c_val,var_s,var_n2) >> s2(c_val, var_s, var_n)
r8 : isa2(p_num_succ,var_n,var_n2) /\ s2(c_val,var_s,var_n2) >> s2(c_val, var_s, var_n)
r5 : isa(p_is_0,var_n) /\ s(c_q2,var_s) /\ s2(c_val,var_s,var_n) >> s(c_q1, var_s)
r7 : isa(p_is_0,var_n) /\ s(c_q1,var_s) >> s(c_q2, var_s)

% Figured out:
% max
% l_max_loud ^ l2_max_loud ^ s_q1 ^ s_q4 -> s_l2 (which is max)
% 14: max loud
r1 : isa(p_max_loudness,var_l) /\ isa(p_max_loudness,var_l2) /\ s(c_q1,var_s) /\ s(c_q4,var_s) -> s2(c_loudness, var_s, var_l2)

% s_hihat ^ l_max_loud ^ succ(l_2, l) ^ s_l2_loud -> s_1
% if the hihat is one after max loudness, it reads 1
r3 : isa(p_is_hi_hat,var_s) /\ isa(p_max_loudness,var_l) /\ isa2(p_succ,var_l2,var_l) /\ s2(c_loudness,var_s,var_l2) -> s(c_q1, var_s)

% succ(l, l2) ^ s_1 ^ s_l_loud -> s_3
% if s is 1 and it isn't loudest, then it's also 3
r2 : isa2(p_succ,var_l,var_l2) /\ s(c_q1,var_s) /\ s2(c_loudness,var_s,var_l) -> s(c_q3, var_s)

% l_min_loud ^ s_2 -> s_4
% 2 is always paired with 4 (vacuous min_loud...)
r4 : isa(p_min_loudness,var_l) /\ s(c_q2,var_s) -> s(c_q4, var_s)

% causal

% s_bassdrum ^ l_max_loud ^ l2_min_loud ^ s_3 >> s_l2_loud (min)
% if bassdrum is 3, then next step it will be min loudness
r12 : isa(p_is_bass_drum,var_s) /\ isa(p_max_loudness,var_l) /\ isa(p_min_loudness,var_l2) /\ s(c_q3,var_s) >> s2(c_loudness, var_s, var_l2)
r6 : isa(p_is_snare_drum,var_s) /\ isa(p_max_loudness,var_l) /\ isa(p_min_loudness,var_l2) /\ s(c_q3,var_s) >> s2(c_loudness, var_s, var_l2)

% succ(l, l2) ^ s_4 ^ s_l2_loud >> s_l_loud
% if s is 4 and isn't quietest, s gets quieter next step
r9 : isa2(p_succ,var_l,var_l2) /\ s(c_q4,var_s) /\ s2(c_loudness,var_s,var_l2) >> s2(c_loudness, var_s, var_l)

% s_hihat ^ succ(l, l2) ^ s_l_loud >> s_l2_loud
% if hihat isnt the loudest, then next step it's the loudest
r11 : isa(p_is_hi_hat,var_s) /\ isa2(p_succ,var_l,var_l2) /\ s2(c_loudness,var_s,var_l) >> s2(c_loudness, var_s, var_l2)
