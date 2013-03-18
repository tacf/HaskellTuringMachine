HaskellTuringMachine
====================

A simple Turing Machine in Haskell

Introduction
===========

A Turing Machine is a 6-Tuple M = (Q,A,G,delta,i,f) where,
 - Q is the set of possible states
 - A is the input alphabet
 - G (A in G) is the tape alphabet
 - i,f are states from Q called, respectively, initial and final
 - delta is a partial function of type 
   QxG -> QxGx{Left,Equal,Right}

A configuration is a triple C = (v,s,u)
 - v is the word before cursor position on tape
 - s is the current state of the machine
 - u is the from the cursor position to "infinity"

Example:
Let's assume cursor positioned at 'c' location,
Tape = "abbbcdddddBBBBBBB...",
then
C = ("abbb",<some_state>, "dddddBBBBBBB...")

Assumptions
==========
For simplicity reasons is assumed that the TM tape is infinite and  is filled with blank characters(B) after the users input 

(ex: "abcBBBBBBBB...", where "abc" is the user input)