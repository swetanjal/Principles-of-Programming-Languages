# Principles-of-Programming-Languages

This repository contains all the assignments done as a part of 'Principles of Programming Languages(PoPL)' Course instructed by Dr. Venkatesh Chopella during the Spring 2020 Semester at IIIT-H. The aim of this course was to get an in-depth understanding of how Programming Languages are designed by building a number of Interpreters, each of which incorporates a new design concept.

Here is a description of each of the Assignments done:
- Assignment 1: Exercise 1.4 of the book 'Essentials of Programming Languages' which involved writing recursive programs using Racket. Being comfortable with recursion is very important to understand Programming Language Design as you have to continuously evaluate Abstract Syntax Trees which are recursive structures.
- Assignment 2: Functional Programming on Trees excercise, another component which is crucial to undertsand Programming Language Design. In fact ASTs are nothing but special types of trees.
- Assignment 3: Interpreter to evaluate programs written in the Arithmetic Language. The Arithmetic Language is a very simple Language that supports Arithmetic operations on integers. This interpreter is nothing but a calculator which incorporates the idea of evaluation of expressions using ASTs. Run interpreter as follows:
    ```bash
    cd Assignment\ 3/
    ./repl
    ```
    Refer Chapter 2 of the book "Essentials of Programming Languages" for a detailed specification of the Arithmetic Language.
- Assignment 4: Interpreter that supports lexical scoping in the Arithmetic Language. Refer to Chapter 3.2 of the book "Essentials of Programming Languages" for detailed specification of the language. Run interpreter as follows:
    ```bash
    cd Assignment\ 4/
    ./repl
    ```
    Refer to lexical.org for detailed specification and syntax of this defined language.

- Assignment 5: Interpreter that supports procedures and recursion in the Arithmetic Langauge. Refer to Chapter 3.3 and 3.4 of the book "Essentials of Programming Languages" for detailed specs of the language. Run the interpreter as follows:
    ```bash
    cd Assignment\ 5/
    ghci
    Prelude> :l ASTParser.hs
    [1 of 1] Compiling ASTParser        ( ASTParser.hs, interpreted )
    Ok, modules loaded: ASTParser.
    *ASTParser> :l letrec.hs 
    [1 of 2] Compiling ASTParser        ( ASTParser.hs, interpreted )
    [2 of 2] Compiling Main             ( letrec.hs, interpreted )
    Ok, modules loaded: ASTParser, Main.
    ```
    Refer to functional-recursive.org for details about syntax of the Parse Tree for the interpreter.

- Assignment 6: Interpreter that supports stores and imperative programming in the Arithmetic Language. Refer to Chapter 4.2 of the book "Essentials of Programming Languages" for detailed specifications of the language. Run the interpreter as follows:
    ```bash
    cd Assignment\ 5/
    ghci
    Prelude> :l ASTParser.hs
    [1 of 1] Compiling ASTParser        ( ASTParser.hs, interpreted )
    Ok, modules loaded: ASTParser.
    *ASTParser> :l stores.hs 
    [1 of 2] Compiling ASTParser        ( ASTParser.hs, interpreted )
    [2 of 2] Compiling Main             ( stores.hs, interpreted )
    Ok, modules loaded: ASTParser, Main.
    ```
    Refer to stores.org for details about syntax of the Parse Tree for the interpreter.

For any queries related to the content, feel free to reach out to me at: swetanjal.dutta@research.iiit.ac.in