-module(elevatoraction).
-export([start/0]).

start() ->
    People = person:create_people(10, 5),
    Elevator = elevator:start(People),
    {People, Elevator}.
