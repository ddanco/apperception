%----------------------------
%   Time            Sensor a 
%                            
%   1               on       
%   2               off      
%   3               on       
%   4               off      
%   5               on       
%   6               off      
%----------------------------

% The given sequence
senses(s(c_on, obj_sensor_a), 1).
senses(s(c_off, obj_sensor_a), 2).
senses(s(c_on, obj_sensor_a), 3).
senses(s(c_off, obj_sensor_a), 4).
senses(s(c_on, obj_sensor_a), 5).
hidden(s(c_off, obj_sensor_a), 6).