# Chess

Please adhere to [Cornell University's Academic Integrity Policy](http://cuinfo.cornell.edu/aic.cfm) and [The CS Department's Academic Integrity Policy](http://www.cs.cornell.edu/undergrad/CSMajor#ai).

## Overview

This is a chess project that I completed with [three other partners (John, Max, Eli)](https://github.com/saaqebs/Chess/blob/master/authors.mli) for my undergraduate Functional Programming class. 

The game has four features: Human v. Human, Human v. Easy Computer, Human v. Advanced Computer, and spectating the Easy Computer v. Advanced Computer.

## Design

The design is based off Vectors, Boards, and Player modules. These modules are built upon one another, creating a complex layering of code to efficiently create the system. The game is entirely made in OCaml, but does not use the GUI library; instead we print to the terminal window. 

## Running

To run the game, download this repository. Change your terminal's directory to the folder containing this directory. afterwards, type `make play`. Given that you have OCaml installed on your computer, it should immediately run the chess program and you should be prompted with the proper questions.
