# Changelog for `psnodig`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 03-09-2023

- Written a Syntax.hs file for an early draft of Psnodig's syntax
- Written a parser for a subset of Go, called Gourmet
- Written a parser for a subset of Python, called Petite

## 0.1.0.1 - 04-09-2023

- Written a transpiler, PetiteTranspiler, which takes an AST and returns code in Petite syntax
- Written a transpiler, GourmetTranspiler, which takes an AST and returns code in Gourmet syntax
- Added "If" statement to Petite and Gourmet
- Thought: Are functions relevant? If this tool is used for algorithms: should not each algorithm be their own file? And would this in turn be something that solidified Psnodig's status as a DSL rather than a GPL?
- Added QuickCheck as dependency and started setting up test suite

## 0.1.0.2 - 08-09-2023

- Renamed GourmetTranspiler to GourmetWriter
- Changed how expressions are parsed in Petite and Gourmet, now using a table
- Added indentations after conditionals and loops in Gourmet writer
- Created a LaTeX folder and a LatexWriter file
- Extended the Program datatype to include a function name and arguments

## 0.1.0.3 - 16-09-2023

- Added GreaterThanEqual and LessThanEqual
- Added booleans
- Added function calls (expressions, like in Haskell)
- Added return statement
- Refactored the Program type
  - Used to be "Program String [String] [Statement]", so that each program was just a function
  - Now it's "Program [Function] FunctionCall", so that you can have multiple functions, and choose which one you want to call
  - This makes more sense for algorithms that rely on auxiliary functions like merge sort.
  - Also, this is kinda how I write algorithms in Python :)
- Places that take multiple arguments were written like this:
  - identifier `sepBy` string ", "
  - I have removed the whitespace strictness:
    - identifier `sepBy` (whiteSpace >> char ',' >> whiteSpace)
- Added arrays
- Added array indexing
- Successfully managed to transpile a (hacky) version of binary search from Gourmet to Gourmet
- Successfully managed to transpile a (hacky) version of binary search from Gourmet to LaTeX

## TODO:

- Add "else <cond> {}" and "else {}" to the language
- if <c0> {} else if <c1> {} .. {} else if <cn> {}
- if <c0> {} else {}

k1 = if ? {}
k2 = if ? {} else {}
k3 = if ? {} else if ? {}
k4 = if ? {} else if ? {} else {}

data k1 = expr [stmt]
data k2 = expr [stmt] [stmt]
data k3 = k1 k1
data k4 = k1 k2

- Use a reader monad to traverse the AST and store relevant metadata for binary search, e.g. functions and arrays

- Try more algorithms
- Write an interpreter for Gourmet
- Get at least 1 test actually working
