Usage example:

```
> import Data.Tree.Pretty

> let g = vcat left (map text ["┌───────┐","│ o o . │","│ o x x │","│ . . . │","└───────┘"])

> let tree = (Node g [Node g [], Node g [Node g [], Node g [], Node g []]])

> putStrLn $ drawVerticalTree tree
           ┌───────┐
           │ o o . │
           │ o x x │
           │ . . . │
           └───────┘
               |
     ---------------------
    /                     \
┌───────┐             ┌───────┐
│ o o . │             │ o o . │
│ o x x │             │ o x x │
│ . . . │             │ . . . │
└───────┘             └───────┘
                          |
                ---------------------
               /          |          \
           ┌───────┐  ┌───────┐  ┌───────┐
           │ o o . │  │ o o . │  │ o o . │
           │ o x x │  │ o x x │  │ o x x │
           │ . . . │  │ . . . │  │ . . . │
           └───────┘  └───────┘  └───────┘
```
