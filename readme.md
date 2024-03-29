# Description
This is an interpreter for the BASIC language written in Haskell; it takes source files to perfrom lexical analysis and
then execute the behavior directly. A description of the grammar used is also included for reference.

# Example Output


![inter1](https://user-images.githubusercontent.com/68394183/191052989-536c39c9-7c79-48f6-b40d-7b0de8ec7045.PNG)




## Requirements
Ensure that the System.Random package [Hackage] (https://hackage.haskell.org/package/random-1.1) is installed on the system.
Also, the code was developed using GHC 8.10.1

## Usage
A single exe is generated after compiling, where it upon running, prompts the user for a filename, and the program is executed.

## Issues
 * For loops as of now can be the only statement on a line
 * Colons (':') will only be treated as delimiters for statements
 * Loop steps may overshoot, exceeding the final variable value, and continuing going



