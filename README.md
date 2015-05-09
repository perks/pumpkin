<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
Pumpkin
=====

##Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
  - [Usage](#usage)
- [Features](#features)
  - [Functional](#functional)
  - [Function Piping and Composition](#function-piping-and-composition)
  - [Concise](#concise)
- [Quickstart Tutorial](#quickstart-tutorial)
    - [Variables](#variables)
    - [Functions](#functions)
    - [Comments](#comments)
    - [Piping](#piping)
    - [Function Composition](#function-composition)
    - [Type Inference](#type-inference)
    - [Control Flow](#control-flow)
    - [Printing](#printing)
    - [Running Programs](#running-programs)
- [Language Manual](#language-manual)
  - [Types and Variables](#types-and-variables)
    - [Naming](#naming)
    - [Variables](#variables-1)
    - [Native Types](#native-types)
    - [Boolean Operators](#boolean-operators)
    - [Derived Types (Tuples, Lists, & Maps)](#derived-types-tuples-lists-&-maps)
    - [Arithmetic Operators](#arithmetic-operators)
  - [Program Structure](#program-structure)
    - [Comments](#comments-1)
    - [Indentation](#indentation)
  - [Functions](#functions-1)
    - [Function Chains](#function-chains)
    - [Multi-line Piping](#multi-line-piping)
    - [Composing Functions](#composing-functions)
    - [Partially Applied Functions](#partially-applied-functions)
    - [Native Functions](#native-functions)
    - [List Functions](#list-functions)
- [Demos](#demos)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->


#Introduction
Pumpkin is patchwork functional programming language. The Pumpkin programming language is a light-functional scripting language, which allows for coding flexibility and concise syntax. Pumpkin supports many syntactic optimizations for function nesting and chaining, such as pipes and partially applied functions. This language focuses on easily modeling a complex system of function calls.

_Developed with :heart: at Columbia University_

#Installation
Build the compiler binary (pmkin) by typing ```make``` in the top level directory

##Usage
```bash
Usage: pmkn [required-option] <source file>
required-option:
  -t: Prints token stream
  -a: Pretty prints Ast as a program
  -s: Prints Sast
  -c: Compiles to JavaScript
```

#Features

##Functional
Pumpkin follows functional programming principles: Functions are first class, they can be passed, returned and partially applied. We have both typed and untyped syntax, and allow for recursive functions. Anonymous functions are also allowed to compose functions on the fly, that capture a needed one time behavior but which doesn’t need to be generalized.

##Function Piping and Composition
Pumping allows one argument function calls to be piped together, this way resolving a long system of calls on one, easy to read, line.
Composition takes this idea one step further by nesting calls. Composed functions create new functions out of nested ones, pipe arguments in: pumpkin made complicated calls easy to use and manipulate.

##Concise
With type inference, piping and function composition we allow the programmer to write minimal code that accomplishes great functionality.

#Quickstart Tutorial

Here we present the fundamental building blocks of our language necessary to begin writing simple programs.

###Variables
Declare variables with the keyword ’val’:
```scala
val y: Bool = True
```

###Functions
Declare functions with theyword 'def':
```scala
def add(a: Int, b: Int) => a + b
```

###Comments

Create comments with ```\\``` or ```\* *\```

```java
\\ This is a comment

\* This is a
    multi-line comment *\
```

###Piping
Pipe function arguments with ```|>```

```scala
val x = [1,2,3] |> (a: List[Int] => len(a) % 2)
if x is 0:
  print("Even")
else:
  print("Odd")
```
###Function Composition

Create function compositions with ```<<``` or ```>>```

```scala
val plusTwoTimesThree = (x: Int => x * 3) << (x: Int => x + 2)
plusTwoTimesThree(4) // => 18
```

###Type Inference
Our language includes type inference for variables and functions. The above declarations could be written concisely as such:

```scala
val y = True
def add(a, b) => a + b
```

###Control Flow

Control flow is handled through if...else loops:

###Printing
Printing is handled with the ’print’ keyword.

```scala
print("somestring")
print(variable)
```

###Running Programs
Programs must be saved as ’.pk’ files. We include a makefile to compile our language, which must be done with ’make’. Then, our language can be compiled to Javascript by running the pumpking executable with the ’-c’ flag. Finally, you may use the platform of your choice to execute Javascript programs. We recommened Node, which is a popular and easy to install platform. A sample workflow is provided:

```bash
$ make
$ ./pkmn -c targetProgram.pk > targetProgram.js
$ node targetProgram.js
```

#Language Manual

##Types and Variables

###Naming

Variable and function names must be a sequence of letters, digits and underscores. The first character must be a letter. By convention we will use CamelCase for names with multiple words.

###Variables

A variable is a storage location paired with an associated name and type, which contains some value. All variables are statically typed and may be reassigned with a new primative or structure of a corresponding type. Variables can not be redeclared in any context.
Variable declarations and assignments, much like in C, are treated as expression. A variable, unlike C, cannot be declared as empty.

```scala
// Follows form val name = value , or name: Type = value .
// Legal declarations
val aNumber: Int = 5 aNumber = 10
aNumber = aNumber + 5
v a l anotherNumber = aNumber = 6
// I l l e g a l
val aNumber = 10 // Error thrown on redeclaration of an already used variable
val emptyVar // Error thrown on empty variable
```

###Native Types
* Int: a signed two’s complement integer which has a minimum value of −2^31 and a maximum value of 2^31-1.
* Float: a floating point value that can represent either very small fractions or numbers of great magnitude.
* Char: a single character.
* String: an immutable type that contains a sequence of characters.
* Bool:: a boolean type whose only two possible values are True and False.
* Unit: represents an output of nothing, similar to unit in most functional languages.

###Boolean Operators
Boolean variables are either True or False. They can be manipulated with logical expressions and operators.
Relational Expressions:
Boolean variable can be compared and combined with logic symbols to form expressions. Equality tests can be written with the keyword ’is’ or with the symbol ’==.’ Other comparisons use the standard symbols used in Java:

```scala
1 < 2 // less than => True 
3 > 4 // greater than => False
1<=3 // Less or equal to => True 
2 >= 2 // Greater or equal to => True 
3 is 4 // Equality => False
5 == 5 // Equality => True
```
Logical Operators:
Pumpkin supports the three basic logic operators. Logical and can be written with the keyword ’and’ or with the symbol ’&&.’ Logical or can be written with the keyword ’or’ or with the symbol ’||.’ Negation can be written as the keyword ’not’ or with the symbol ’ !.’ Finally, equality can be expressed by either using '===' or 'is'. You can mix and match styles, it is left to the user how to do so.

Examples:

```scala
False is False // => True
True is not False // => True
not False // => True
!True // => False
not True and False // => True
True or False // => True
True || False // => True
False == True // => False
```

###Derived Types (Tuples, Lists, & Maps)
**N-tuples:** Tuples are a basic immutable data structure that holds an ordered set of elements.
Unlike Lists or Maps, the elements with a tuple can be of any type and do not need to follow a consistent type declaration.
Tuples are most useful when a function may wish to return more than a single piece of information, or more commonly used as part of function nesting with ’pipe ins’ and ’pipe outs’.
The following symbol scheme will be used to access consecutive elements of a tuple: $0, $1, $2, ... $n-1.

```scala
val t = (1, ”hello”, ”world”, 5)
t |> (x: Tuple => if ((x$0 + x$3) % 2 == 0) print(x$1 + ” ” + x$2)) // Prints ”hello world”;

//Alternatively

t |>
(x : Int, a: String, b: String, y: Int => if(( x + y ) % 2 == 0 ) print(a + ” ” + b))
// Prints ”hello world”;
```

To declare a tuple with a single value it is necessary to follow it with a comma (like Python):
```scala
val t = (1,) //Type Tuple[Int,] val i = (1) //Type Int
```

**Lists:** Lists replace arrays as the basic iterable data structure. Lists are zero-indexed and immutable, so that all operations create new copies of the list. Lists accept any type but must have a consistent type across all elements.
Pumpkin Lists also support basic head and List features, called as hd and tl respectively. Pumpkin also supports is empty and len
Pumpkin supports the typical cons operator for new list creation.

``scala
//Normal construction
val myList: List[Int] = [1, 2, 3, 4]
// ’:: ’ is an operation that creates a new list by appending the element to the head
val newList = 10 :: myList // => [10, 1, 2, 3, 4)]
val head = hd(myList) //hd = 1
val tail = tl(myList) //tl = [2, 3, 4]

**Maps:** Maps act as a basic immutable Key:Value data structure that is
staticly typed on both Key and Value. Maps are unordered and no guarantee can be made on the ordering of contents. Keys can be only primative types.

```scala
val myMap = (”x” −> ”y”, ”a” −> ”b”, ”t” −> ”s”)
val fetchedVal = myMap(”x”); // => ”y”
```

### Arithmetic Operators
Pumpkin supports the following basic arithmetic operators:
* ’+’ Used for addition or String concatonation.
* ’-’ Used for subtraction or unary negative.
* ’/’ Used for division.
* ’*’ Used for multiplication.
* ’%’ Used for modulus.

```java
1 + 2 // => 3
4/2 // =>2
4.0 / 2 // => Type error 
3 % 2 // => 1
6 − 2 // => 4 
6−−2 //=>8
```

##Program Structure

Pumpkin is a compiled functional scripting language. Thus, the entry point of the code is the first line of a file, and the return type/value is whatever the last line of the file returns.

###Comments
Single line comments are symbolized by two forward slash characters.

```java
// Slashes mark the beginning of a single line comment.
```

Multi-line comments are written C-style.

```java
/∗
The slash and asterisk mark the beginning and end of a multi−line comment.
∗/
```

###Indentation
Pumpkin does not use curly brackets to delimit scope, so correct white spacing is essential for programs written in the language. The indentation must be either a certain amount of spaces or a tab character, but it must be consistent throughout a program, as in the following example:

```python
if (x % 2 == 0):
  if (False):
    print (”unreachable nested code”)
  else :
    print (”Even”)
else :
  print (”Odd”)
```

## Functions

In Pumpkin all flow-control is done through functions. Pumpkin does not support for or while loops.

Functions can be defined with the ’def’ keyword. The types must not be specified due to type inference, but the the parameters must be written within parantheses. The exception to this rule is that recursive functions must always be declared with explicit return types.

Basic Syntax:

```scala
def funcName(parameter: type): returnType => 
 // code goes here
```

Single line functions are also allowed:

```scala
def x(y:Int):Int=> (y+1)
x(1) // => 2
```

Here are a couple of ways to declare a value that answers ”is 2 even?":

```scala
// value is determined when x is evaluated in an expression
def x: Bool => 
  even(2)

val x = even(2) // value is determined at compile time
```

Anonymous functions are also allowed for flow control. They must be declared on a single line.

```scala
( variableName : dataType => code ) : returnType

To pass a function as a parameter use the syntax **name:(params − > return type)**.

```scala
def x(y: Int, z:(Int, Int −> String)): Bool => True
```

The function x takes two parameters; y, which is an int, and z, which is a function. z takes two int parameters and returns a string. x returns a boolean. (Here the code for function x is trivial, but one could imagine more complexity involving invocation of z)

###Function Chains
The symbols ’| >’ and ’< |’ can be used to chain nested function calls. The expression on the bar side is evaluated first. All arithmetic operators are applied before evaluating left to right, and parentheses are respected as in the traditional order of operations. All expressions on the call-side of the flow must be functions.

For example:

```scala
val a: Int = 3
a |> (x: Int => x + 1): Int // Returns 4;
```
NOTE: The function calls *funcName(x), x| > funcName* and *funcName < |x* are semantically the same, but resolve differently with different precedence.

More examples of control flow:

```scala
val b: Int = 3
b |> (x: Int => x + 1): Int |> even
```
The above expression gets executed as follows:

1.  Evaluate expression b: Int => 3.
2.  Left-to-right: pipe into expr2, an anonymous function which takes one argument (an Int), adds 1 to it and returns a new Int.
3.  Pipe result into even(), which is a function that takes an Int and returns a Bool.
4.  No more pipes, left-to-right is done, return boolean value.

```scala
val b: Int = 3
even <| (b |> (x: Int => x + 1): Int)
```
The above expression gets executed as follows:
1. Evaluate expression b: Int => 3, since the parentheses give it precedence.
2. Pipe right into anonymous function that returns an Int.
3. Evaluate expression even: (x: Int => Bool) with the return of the anonymous call. 
4. After evaluating there are no more pipes or operations, return boolean.

###Multi-line Piping

Pipes will ignore whitespace. Thus if a line begins with a pipe it will push the return of the line above in. Once a line does not begin with a | > or < | sign it is outside the piped block

```scala
4
|> plusOne
|> even // return false
even (2) // outside pipe
```

###Composing Functions

The << and >> operators can be used to call a fuction with the return type of another. For example:

```scala
￼(f << g) <| x or x |> (f << g) // is same as f(g(x)) 
(f >> g) <| x or x |> (f >> g) // is same as g(f(x))
```

NOTE: If at any time the order or type of the arguments don’t match, compiler will throw errors. Furthermore only the inner-most function is allowed to take mutiple paramethers, in order to avoid unreachable paramethers.

Another example:

```scala
def timesTwo(x: Int):Int => x ∗ 2 
def plusOne(x: Int):Int => x + 1

def plusOneTimesTwo(x: Int): Int => x |> (plusOne << timesTwo) 
def timesTwoPlusOne(x: Int): Int => x |> (timesTwo << plusOne)

plusOneTimesTwo ( 9 ) // => 20 
timesTwoPlusOne ( 9 ) // => 19
```

###Partially Applied Functions


Pumpkin supports partially applied functions. When a function is called without all of the necessary arguments, it returns another function that has the previously passed arguments already set. Arguments must be passed in order:

```scala
def plus(x, y):Int =>
  x+y

val plusOne = plus (1) 
val three = plusOne (2)
```

Another example:

```scala
[1, 2, 3, 4] |> filter(even) |> map(timesTwo) |> fold(add, 0) // Returns 12
```

###Native Functions

**Print**: Printing to the standard output stream is akin to Java, and begins with the word print followed by the items to be printed in parentheses.
Escape characters include ’\t’ for tab, ’\n’ for newline, and ’\\’ for backslash

###List Functions
A few built in functions are created specifically to operate on lists: 
**hd** returns the first element of a list.
**tl** returns a new list with the first element removed.
**is empty** returns boolean indicating if the list is empty or not. 
**len** returns an integer indicating the length of the list


#Demos

Download and compile the demo to see some capabilities of Pumpkin!

```
./pmkin -c demo.pk | node
```









