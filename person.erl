-module(person).
-export([init/2, create_people/2]).

init(CurrentFloor, DesiredFloor) ->
    spawn(fun() -> initializing(CurrentFloor, DesiredFloor) end).

initializing(CurrentFloor, DesiredFloor) ->
    io:format("[~p->~p] waiting on floor ~p~n", [CurrentFloor, DesiredFloor, CurrentFloor]),
    receive
        {Elevator, over_here} ->
            Elevator ! {self(), stop_at, CurrentFloor},
            timer:sleep(50),
            waiting_for_elevator(Elevator, CurrentFloor, DesiredFloor)
    end.

waiting_for_elevator(Elevator, CurrentFloor, DesiredFloor) ->
    receive
        {Elevator, on_floor, CurrentFloor}  ->
            Elevator ! {self(), ack, CurrentFloor},
            io:format("[~p->~p] pressing button ~p~n", [CurrentFloor, DesiredFloor, DesiredFloor]),
            Elevator ! {self(), stop_at, DesiredFloor},
            waiting_for_floor(Elevator, CurrentFloor, DesiredFloor);
        {Elevator, on_floor, Floor} ->
            Elevator ! {self(), ack, Floor},
            waiting_for_elevator(Elevator, CurrentFloor, DesiredFloor)
        after 1000 -> %getting impatient
            Elevator ! {self(), stop_at, CurrentFloor},
            waiting_for_elevator(Elevator, CurrentFloor, DesiredFloor)
    end.

waiting_for_floor(_Elevator, CurrentFloor, DesiredFloor) ->
    receive
        {From, on_floor, DesiredFloor} ->
            From ! {self(), ack, DesiredFloor},
            done(CurrentFloor, DesiredFloor);
        {From, on_floor, Floor} ->
            From ! {self(), ack, Floor},
            waiting_for_floor(_Elevator, CurrentFloor, DesiredFloor)
    end.

done(CurrentFloor, DesiredFloor) ->
    io:format("[~p->~p] getting off at floor ~p~n", [CurrentFloor, DesiredFloor, DesiredFloor]),
    process_end().

process_end() ->
    receive
        {From, on_floor, Floor} ->
            From ! {self(), ack, Floor},
            process_end()
    end.

valid_random(Number, NotThis, Total) ->
    case Number of 
        NotThis ->
            NewNumber = (round(rand:uniform() * Total + 1) rem Total)+1,
            valid_random(NewNumber, NotThis, Total);
        _ -> 
            Number
    end. 

create_people(MaxFloor, Count) ->
    [
        begin
            Current = valid_random(-1, -1, MaxFloor),
            Desired = valid_random(Current, Current, MaxFloor),
            person:init(Current, Desired)
        end 
        || _ <- lists:seq(1, Count)
    ].