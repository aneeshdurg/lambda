# lambda
A simple lambda calculus interpreter

### Usage

If you haven't installed stack:
```bash
curl -sSL https://get.haskellstack.org/ | sh
```
After you have stack,
```bash
stack init
```

To run either use `stack build` to generate binaries or `stack repl` and type `main`.

### Syntax
#### Function application:
* function and arugments seperated by a space.
* functions will assume that all items to the left are arguments unless they are specified with parens or it encounters an infix binary operation (+*-/><) 
```haskell
f x
g (f x) y
f x + 1 
  == (f x) + 1
```
#### Lambda functions:
* denoted by a \, arguments are seperated by spaces
* body and arguments delimited by a .
```haskell
\x y.x + y
\x.\y.y x
(\x.x x) (\x.x x)
```
#### Variable assignment:

* Variables can only be assigned as a top level expression

```haskell
y = 1
x = (\x.x+y)
y = 3
x 2 
  == 3
y 
  == 3
```

#### Numbers and Booleans

* Currently only integers are supported

```haskell
1, 2, #t, #f
(\x . 3 > x) 2
  == #f
```

#### Binary Operators

```haskell
1+2
  == 3
5/3
 == 1
1*1
  == 1
1-1
  == 0
1 > 1
  == #t
1 < 1
  == #t
```

### TODO:

* Implement logical operators
* Implement equality operator
* Various other things
