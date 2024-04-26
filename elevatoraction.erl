-module(elevatoraction).
-export([start/2]).

% This is the main function to set everything up
% Pass in the # of floors and the # of people
start(Floors, NumberOfPeople) ->
    People = person:create_people(Floors, NumberOfPeople),
    Elevator = elevator:start(People),
    {People, Elevator}.
