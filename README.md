# rea-toy-robot

SPEC
===============
- [ ] Misc
  - [X] The application must be a command line application.
  - [X] Input can be from a file, or from standard input, as the developer chooses.
     - going for stdin, can always `cat $FILE | thisProgram` anyway
  -  [x] The application is a simulation of a toy robot moving on a square tabletop,
  of dimensions 5 units x 5 units.
     - Hard coding the 5 x 5 units in `Lib.boardHeight/Width` constants for now.

- [ ] User Input
  - [ ] The first valid command to the robot is a PLACE command, 
  - [ ] after that, any sequence of commands may be issued, in any order, including another PLACE command. 
  - [ ] The application should discard all commands in the sequence until a valid PLACE command has been executed.
    - [ ] A robot that is not on the table can choose the ignore the MOVE, LEFT, RIGHT

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

