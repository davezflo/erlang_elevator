-module(elevator).
-export([start/1]).

% will spawn the elevator process
% takes a list of People
start(People) ->
    spawn(fun()->initialize(People, 1, []) end).

% send a message to the people to provide them the elevator process
% initialize to the stopped state
initialize(People, CurrentFloor, FloorQueue) ->
    lists:map(fun(Person) -> Person ! {self(), over_here} end, People),
    stopped(People, CurrentFloor, FloorQueue).

% ack routine ensures that the elevator doesn't get ahead of the people without this,
% the elevator will continue sending messages about reaching floors and old messages won't have been processed.
% As a result, when a person is ready to process their queue, they will read old messages first and get off on 
% a floor when the elevator isn't there. example - person on Floor 7, wanting to got to Floor 1. Elevator signals
% to all people which floor it is on. It will start with floor 1. Since the person isn't ready to process floors yet,
% this message will queue. When the person finally is ready, they will think they are on Floor 1 and immediately get off.
% This ack routine prevents it:
% Elevator -> fun(1..N Person) -> OnFloor(X) ... wait for N acks before proceeding 
ack_all(People, CurrentFloor, FloorQueue, Direction, Count) ->
    receive
        {_From, ack, CurrentFloor} ->
            if 
                Count +1 < length(People) ->
                    ack_all(People, CurrentFloor, FloorQueue, Direction, Count+1);
                Count + 1 == length(People) ->
                    complete_service(CurrentFloor, FloorQueue, Direction)
            end
    end.

% to watch sim, we are sleeping. Elevator signals all people which floor it is on and then waits for their ack.
service_floor(People, CurrentFloor, FloorQueue, Direction) ->
    timer:sleep(200),
    io:format("DING: @floor[~p] ~p!~n", [Direction, CurrentFloor]),
    lists:map(fun(Person) -> Person ! {self(), on_floor, CurrentFloor} end, People),
    ack_all(People, CurrentFloor, FloorQueue, Direction, 0).

% Routine ensures all instances of a number has been removed from the list
remove_from_queue(Queue, Number) ->
    NQ = Queue -- [Number],
    NewQueue = case lists:member(Number, NQ) of
        true ->
             remove_from_queue(NQ, Number);
        false ->
            NQ
    end,
    NewQueue.

% Provides new values for the FloorQueue and the Direction of the elevator
% all elements are > CurrentFloor -> up
% up && any element > CurrentFloor -> up
% all elements < CurrentFloor -> down
% down && any element < CurrentFloor -> down
complete_service(CurrentFloor, FloorQueue, Direction) ->


    NewQueue = remove_from_queue(FloorQueue, CurrentFloor),
   
    AllGreater = lists:all(fun(X) -> X > CurrentFloor end, NewQueue),
    AllLess = lists:all(fun(X) -> X < CurrentFloor end, NewQueue),
   
    NewDirection = 
        case {length(NewQueue), lists:any(fun(X) -> X > CurrentFloor end, NewQueue), Direction} of
        {0, _, _} -> stopped;
        {_, true, up} -> up;
        {_, true, down} -> down;
        {_, _, up} when  AllGreater -> up;
        {_, _, down} when AllLess -> down;
        _ -> stopped % Default case if none of the above conditions match
    end,

    {NewQueue, NewDirection}.
    
% Elevator is in the up state
% Services the floor and potentially changes direction
% if it remains in up, it waits for messages
up(People, CurrentFloor, FloorQueue) ->
    {NewQueue, NewDirection} = service_floor(People, CurrentFloor, FloorQueue, up),
    case NewDirection of 
        stopped -> 
            stopped(People, CurrentFloor, NewQueue);
        down ->
            down(People, CurrentFloor, NewQueue);
        up ->
            receive
                {_From, stop_at, Floor} ->
                    up(People, CurrentFloor+1, NewQueue ++ [Floor])
            after 200 ->
                up(People, CurrentFloor+1, NewQueue)
            end
    end.

% Elevator is in the down state
% Services the floor and potentially changes direction
% if it remains in down, it waits for messages
down(People, CurrentFloor, FloorQueue) ->
    {NewQueue, NewDirection} = service_floor(People, CurrentFloor, FloorQueue, down),
    case NewDirection of 
        stopped -> 
            stopped(People, CurrentFloor, NewQueue);
        up ->
            up(People, CurrentFloor, NewQueue);
        down ->
            receive
                {_From, stop_at, Floor} ->
                    down(People, CurrentFloor-1, NewQueue ++ [Floor])
            after 200 ->
                down(People, CurrentFloor-1, NewQueue)
            end
    end.

% Elevator is in the stop state
% Services the floor 
% Waits forever for a call from a person
stopped(People, CurrentFloor, FloorQueue) ->
    {NewQueue, _} = service_floor(People, CurrentFloor, FloorQueue, stopped),
    receive 
        {_From, stop_at, Floor} ->
            {NewFloor, Direction} = if 
                    Floor > CurrentFloor ->
                        {CurrentFloor + 1, up};
                    Floor < CurrentFloor ->
                        {CurrentFloor - 1, down};
                    Floor == CurrentFloor ->
                        {CurrentFloor, stopped}
                end,
            case Direction of
                up -> 
                    up(People, NewFloor, NewQueue ++ [Floor]);
                down ->
                    down(People, NewFloor, NewQueue ++ [Floor]);
                stopped ->
                    stopped(People, NewFloor, NewQueue ++ [Floor])
            end
    end.

