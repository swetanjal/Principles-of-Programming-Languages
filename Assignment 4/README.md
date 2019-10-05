# LEXICAL Language Evaluator

To run the parser:

(1) Open `ghci`, the Haskell interactive prompt.
(2) Load the file `AST.hs`
(3) Give any expression- belonging to the LEXICAL language- as a string to the function `run`.

(note: the evaluator has not been implemented yet!)

```haskell
$ ghci
Prelude> :l AST.hs
Prelude> :l AST.hs
[1 of 3] Compiling ParseUtils       ( ParseUtils.hs, interpreted )
[2 of 3] Compiling ASTParser        ( ASTParser.hs, interpreted )
[3 of 3] Compiling Main             ( AST.hs, interpreted )
Ok, three modules loaded.
*Main> run "( + 1 2 )"
3
```
