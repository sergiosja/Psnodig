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

## 0.1.0.3 - 17-09-2023

- Added IfElse and Else
- Successfully managed to transpile the lecture version of binary search from Gourmet to LaTeX

## 0.1.0.4 - 18-09-2023

- Added code to create PDF from LaTeX file you get from using the LaTeX writer
- Cleaned up src/Programs folder

## 0.1.0.5 - 22-09-2023

- Added "and" and "or" to operators
- Added "not" expression

## 0.1.0.6 - 23-09-2023

- Refactored grammar, made Arrays their own "thing"
- Added foreach- and for-loops to grammar
- Wrote parser and writer for Gourmet and Latex for for-loops
- Refactored parts of the Gourmet parser to use applicative style over monadic
- Refactored assigning to include assigning on array index
- Successfully transpiled bubble and insertion sort from Gourmet to Latex
- Added "build-in functions" length, ceil, floor and contains to Latex Writer, since they can be represented with mathematical symbols
- When transpiling a file, a .tex file maching the name of the original file is created. Previously "algo.txt" was always created
- EDIT: The "pdflatex" command is only ran when the file does not already exist (because the pdf is re-rendered anway if the .tex-file is changed) [edit: reverted it as it didn't seem to work after all]
- Added possibility to call functions as statements. Divided into CallExp and CallStmt, which are basically identical

## 0.1.0.7 - 26-09-2023

- Used State monad to traverse AST and store environment of functions and arrays
- Changed Writer monad to ReaderT Environment (Writer String), to use the environment in the code
- Spent a lot of time getting the State monad functions right

## 0.1.0.8 - 27-09-2023

- Latex writer now writes fractal symbol during division expressions
- Added all statements and expressions to Env
- Simplified methods in Main.hs since I'm only focusing on Gourmet and Latex now
- Added function to print AST, to analyse the intermediate code when needed

- **Discovered pretty crucial limitation: Proc names can't have numbers (or any other symbols) in them, due to the way the \KwSetFunction-command in Latex works.**

  - On the other hand, very few (if any) "serious" algorithms need non-letter characters. The Gourmet interpreter will still be able to handle them, they just have to be rewritten when transpiled to Latex

- Implemented #-statements: Code that is interpreted, but ignored by writer. Basically a way of describing macros within a procedure
- Implemented @-statements: Code that comes with a description, so that the interpreter runs the code, while the writer ignores the code and writes the description only. Especially useful when the implementation is messy and/or unsignificant
- Refactored more of the parser to use applicative style
- Removed "pass" statements, since we're using block scoping with {}-brackets
- Added math symbol "in" for loops like "for i := array {}"

## 0.1.0.9 - 02-10-2023

- Added break and continue statements
- Refactored types
- Added structs and field access

## Gourmet Parser & Latex Writer

### FIXME:

- Replace true and false with Top- and Bottom symbols in Latex Writer
- ArrayIndex String Expression: `String` should actually be `VariableExp`
- Should be possible to do nested indexing, e.g. `array[1][2][3]`. Possible change is from `ArrayIndex String Expression` to `ArrayIndex String [Expression]`
- ` f()[exp]` should be valid in case `f -> Array`
  - Check if this has ever happened in an algorithm before implementing, intuitively a rare case
- ForEach String String [Statement]: Last `String` could be `(Either String Array)`
  - Again, when is it necessary to traverse a hardcoded array?

### TODO:

- Add unwrapping? e.g. `for (s, f) := SetOfTuples {}` or `(E, V) := Graph`
- Allow explicit parentheses
- Allow user to describe input, output and name of algorithm
- Implement Class/struct/objects?
  - Will have to refactor types if this is the case!
  - Maybe a global state of types also, will have to try out stuff
- Implement Graphs and Trees as separate things?
- Implement Maps?
- Add things like stack, queue, PQ etc?
- if we have class, we can do this ourselves

- Add annotationExpression too? or probably redundant

- For graphs and trees, can use tikZ library to visualise them!! e.g. how do the trees look at the end of the run? can just use latex writer, and add functions. and add config stuff to accomodate for it.

## Gourmet Interpreter

### FIXME:

### TODO:

- Start
- for i := a, b

  - if a < b = increment i
  - if a > b = decrement i

- Allow input from stdin
- Implement stuff like add/append etc. to lists

## Testing

- Get at least 1 property based test working
- Do user testing, try on many different people

  - Students in 1st year of CS
  - Students in 2nd year of CS
  - Students in 3rd year of CS
  - Master students
  - PhD students (Magnus, Joachim etc.)
  - People who have not taken IN2010?
  - People from other unis
    - Sophie? due to UiB
    - NTNU friends at Blank
  - Other people at Blank

- What exactly do I want to test??
- Get help from Eivind and Eirik

## Other

- Try more algorithms
- Study more algorithms in the books I've listed
- Should clear up some of the code "language". I write exp, exps, expr, exp1, exp2 etc many places, it can become confusing, I should have some sort of dictionary, and also be more consistent, example: \
  expr -> single expression \
  exprs -> list of expressions \
  expr1 -> first of N expressions in \ collection, where N is a constant \
  expr2 -> second of N expressions =||=
