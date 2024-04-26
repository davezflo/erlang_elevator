-module(elevator).
-export([start/1]).

start(People) ->
    spawn(fun()->initialize(People, 1, []) end).

initialize(People, CurrentFloor, FloorQueue) ->
    lists:map(fun(Person) -> Person ! {self(), over_here} end, People),
    stopped(People, CurrentFloor, FloorQueue).

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

service_floor(People, CurrentFloor, FloorQueue, Direction) ->
    timer:sleep(200),
    io:format("DING: @floor[~p] ~p!~n", [Direction, CurrentFloor]),
    lists:map(fun(Person) -> Person ! {self(), on_floor, CurrentFloor} end, People),
    ack_all(People, CurrentFloor, FloorQueue, Direction, 0).

remove_from_queue(Queue, Number) ->
    NQ = Queue -- [Number],
    NewQueue = case lists:member(Number, NQ) of
        true ->
             remove_from_queue(NQ, Number);
        false ->
            NQ
    end,
    NewQueue.

complete_service(CurrentFloor, FloorQueue, Direction) ->

    % all elements are > CurrentFloor -> up
    % up && any element > CurrentFloor -> up
    % all elements < CurrentFloor -> down
    % down && any element < CurrentFloor -> down

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

