# LL-reducer
This tool reduces your terms of elementary linear calculus and checks if they are well-formed.
### Usage
To compile the project, run `make` (or `make && make clean`). Then the program in `Main` will accept a list of statements of a form similar to:
```{haskell}
x = \y . y
z = \! z . !(x z)
```
After every such statement, it will check if the term is well-formed, substitute known constants, and output the step-by-step normalization.