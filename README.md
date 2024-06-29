# Unnamed

![workflow badge](https://github.com/musiKk/new-lang/actions/workflows/maven.yml/badge.svg)

My testbed for implementing a very simple programming language. Tokenizer and recursive descent parser handwritten. Simple interpreter.

# Features

## Whitespace, Statement Termination, etc.

Whitespace is insignificant. Expressions greedily parse as much as possible. There is no statement terminator like semicolon.

```
// this works just fine
var x = 1 data Container { content: Str } print(x) var c = Container("foo") print(c.content)
```

## Comments

There are only single-line comments. Comments are introduced with `//` and go until the end of the line

```
// prepare for a variable
var x = 1 // sets x to 1
// variable x exists now
```

## Variables

Variables are mutable. New bindings are introduced with the `var` keyword.

```
var x = 1
var y = "my x: " + x
x = 2
x = "foo" // currently not forbidden but hopefully eventually
```

## Arrays

Arrays have a fixed length. If an initializer list is passed, the length is optional. The runner will crash if accessing out of bounds.

```
var a = []int{}         // empty
var b = [5]int{}        // length 5
var c = []int{1, 2, 3}  // length 3
var d = [3]int{1, 2, 3} // redundant but fine
var e = [5]int{1, 2, 3} // error, length must match initializer list

// access and assign
c[0] = c[1] + c[2]
```

## Blocks

A block is a grouping mechanism and allowed everywhere a statement or expression is allowed. The value of the block is the value of the last statement executed.

```
var x = {
    var a = 1
    var b = 2
    print("returning a + b now")
    a + b
}
// x is 3 now
```

As the block is effectively treated like a value, it is a 100% replacement for parenthesesis used for grouping of expressions. Blocks are just more versatile as they allow for more than just a simple expression.

```
// these are equivalent
var x = (1 + 2) * 3
var y = { 1 + 2 } * 3
```

## Control Flow

There is only an if else right now.

```
var x = 1
if (x == 1) print("x is indeed 1")
else print("for some reason x is different: " + x)
```

## Custom Data Types

Custom data types are created with the `data` keyword. They automatically register a creation function with the same name as the type that can be used to instantiate the type. Fields are accessed with dot notation.

```
data Dog { name: Str }

var fido = Dog("Fido")
print(fido.name)
```

## Functions

Functions are declared with `def`. The body of a function is "assigned" with the equals sign `=`. The body can be any statement. The return value of the function is the return value of the statement.

```
def add(a, b) = a + b
def noisyAdd(a, b) = {
    print("going to add " + a + " and " + b)
    a + b
}
```

Functions are called like in any other C-like language.

```
print(add(1, 2))

var result = noisyAdd(3, 4)
print(result)
```

The commas in the signature are optional. I haven't made up my mind which way should be preferred.

```
// this also works
def add(a: Int b: Int) = a + b
```

Functions are normal variables and can be given to other functions as such. This includes creation functions.

```
def add(a, b) = a + b
def invokeFn(fn, a, b) = fn(a, b)
print(add(1, 2))
print(invokeFn(add, 1, 2))

data Dog { age: Int friends: Int }
print(invokeFn(Dog, 8, 5))
```

## Methods

Methods are just function definitions with an explicit receiver. They have access to the receiver via the `this` keyword.

```
data Dog { name: Str }
def Dog.noise() = this.name + " bark"

var dog = Dog("Fido")
print(dog.noise())
```

For declared methods only there is support for UFCS (uniform function call syntax). This is not useful right now but should allow handing over method references in the future.

```
data Dog {}
def Dog.noise() = "bark"

var dog = Dog()
print(dog.noise())
print(Dog.noise(dog))
```

## Traits

A trait defines behavior. It can be used as an input for a function.

```
data Dog { name: Str }
trait Named { def getName(): Str }
impl Dog is Named { def getName(): Str = this.name }

def describe(named: Named) =
    print("the thing with the name is named " + named.getName())

var fido = Dog("Fido")
describe(fido)
```

Technically this works by collecting all implementing functions at the callsite and registering them in the scope of the callee. That is why traits currently cannot be used as members.

# Under Construction 👷‍♂️

## Types

Types are currently mostly optional and resolved at runtime. I want to introduce a type inference step that completes all missing type declarations though. The only exception for now are the traits which require passing implementing functions to the callee.

## Function Name Resolution

Currently function resolution is hardcoded very specifically for function evaluation. However, I'd like to make this a bit more generic where they are looked up like ordinary variables and thus can be used in data structures and as function parameters.
