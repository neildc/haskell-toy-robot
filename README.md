# rea-toy-robot

## Requisites
* Stack (Haskell build tool )
https://docs.haskellstack.org/en/stable/install_and_upgrade/


## Building
```
stack build
```

## Testing

#### Unit tests
```
stack test
```

#### End to end tests
There are also e2e tests in the `test/e2e` folder. 

These can be ran with the test runner script `e2e.sh`

#### e2e test file format
```
EXPECTED OUTPUT
<BLANK LINE>
Command 1
Command 2
Command 3
...
Command N
```


## Running
```
stack run
```

or

```
cat $FILE | stack exec rea-toy-robot-exe
```


## SPEC
- [x] Misc
  - [X] The application must be a command line application.
  - [X] Input can be from a file, or from standard input, as the developer chooses.
     - going for stdin, can always `cat $FILE | thisProgram` anyway
  -  [x] The application is a simulation of a toy robot moving on a square tabletop,
  of dimensions 5 units x 5 units.
     - Hard coding the 5 x 5 units in `Lib.boardHeight/Width` constants for now.

- [x] User Input
  - [x] The first valid command to the robot is a PLACE command, 
  - [x] after that, any sequence of commands may be issued, in any order, including another PLACE command. 
  - [x] The appl

    - [x] A robot that is not on the table can choose the ignore the MOVE, LEFT, RIGHT

- [x] Update
   - [x] PLACE X,Y,F
     - [x] PLACE will put the toy robot on the table in position X,Y and facing NORTH,
        SOUTH, EAST or WEST.
     - [x] The toy robot must not fall off the table during movement. This also includes the initial placement of the toy robot.
       this is done during parsing

   - [x] MOVE
     - [x] MOVE will move the toy robot one unit forward in the direction it is
     - [x] Any move that would cause the robot to fall must be ignored.

   - [x] LEFT/RIGHT
     - LEFT and RIGHT will rotate the robot 90 degrees in the specified direction
        without changing the position of the robot.

   - [x] REPORT
     - REPORT will announce the X,Y and F of the robot. This can be in any form,
        but standard output is sufficient.
   - [x] PLACE_OBJECT


    - [x] When moving  should be prevented from bumping into previously placed objects on the tabletop surface. 
    - [x] PLACE_OBJECT should place an object in front of the current location
For example if the toy robot is at location (0,0) and facing EAST, an object should be placed in location (1,0). 

0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
> 0 0 0 0 

0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
> $ 0 0 0 

Placing out of bounds?
0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
V 0 0 0 0 

0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
V 0 0 0 0 

$ -> Obstacle
0 -> table positions
[<, >, v, ^] -> Robot

0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 
0 0 0 0 0 


Extension 1: Obstacles
Extend the Toy Robot with a PLACE_OBJECT command, to place an object on the tabletop surface.

When moving the toy robot it should be prevented from bumping into previously placed objects on the tabletop surface. PLACE_OBJECT should place an object in front of the current location of the toy robot.

For example if the toy robot is at location (0,0) and facing EAST, an object should be placed in location (1,0). 









































