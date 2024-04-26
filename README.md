# Elevator Action Simulation

I wrote this code to learn erlang and in particular the process system. I thought it would be pretty straight forward, but I learned a lot. In the end:
1. I like the challenge of implementing a *seemingly* simple elevator
2. I think there are much better languages to achieve this in.

This repository contains an Erlang simulation of an elevator system. It consists of three modules:

1. `elevatoraction.erl`: This module serves as the entry point for setting up the elevator simulation. It provides a `start/2` function to initialize the simulation with a specified number of floors and people.

2. `elevator.erl`: The `elevator` module implements the behavior of the elevator. It manages the elevator's state (stopped, going up, going down), services floors, and handles messages from people.

3. `person.erl`: This module defines the behavior of individual people in the elevator simulation. It includes functions for initializing a person process, waiting for the elevator, getting on the elevator, and getting off at the desired floor.

## Usage

To start the elevator simulation, use the `elevatoraction:start/2` function in the `elevatoraction.erl` module. Pass in the number of floors and the number of people as arguments.

Example:

```erlang
elevatoraction:start(10, 5).
