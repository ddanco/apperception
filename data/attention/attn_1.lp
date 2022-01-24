
%------------------------------------------------------------------------------
% The sensory given.
%------------------------------------------------------------------------------

%-----------------------------------------
%   Time            Sensor a    //Sensor b
%                            
%   1               on          a         
%   2               off         a         
%   3               on          a         
%   4               off         a         
%-----------------------------------------

% The given sequence
senses(s(c_on, obj_sensor_a), 1).
senses(s(c_off, obj_sensor_a), 2).
senses(s(c_on, obj_sensor_a), 3).
senses(s(c_off, obj_sensor_a), 4).

% Elements
is_object(obj_sensor_a).
is_concept(c_on).
is_concept(c_off).
is_time(1..4).

% Input exclusions
% Every sensor (of type 1) is either on or off
% X : sensor_1 → on(X) ⊕ off(X)
% At most one
:-
    holds(s(c_on, X), T),
    holds(s(c_off, X), T).

% At least one
:-
    permanent(isa(t_object, X)),
    is_time(T),
    not holds(s(c_on, X), T),
    not holds(s(c_off, X), T).

% Incompossibility
incompossible(s(c_on, X), s(c_off, X)) :-
    permanent(isa(t_object, X)).

exclusion_output("c_on+c_off").
