
---

# Types

```note.notitle
Question: How do you create a great program?
Answer:   You type it!
```

In this chapter, we will go over the exercises from the introduction
and add types to the examples.

----

In Haskell, type signatures can be provided inline, or above definitions.

For example:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
x :: Int
x = 3
~~~

or

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
x = (3 :: Int)
~~~

It is far more common to place the type-signature above the definition,
with inline types only used in situations where ambiguities need
to be resolved.

<div class="important">

```instruction
You are defining a floating point variable:
```

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myFloat = 1.1
~~~

```instruction
Give your variable a type-signature.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myFloat :: Float
myFloat = 1.1
~~~

</div>

## Type Synonyms

In Haskell, we can give type-expressions an alias (or synonym) by
using the `type` keyword. This allows you to cut down the verbosity
and chance of errors
in your code when you have type expressions that would otherwise
be repeated frequently.

An example of this is the `String` type-synonym, which is defined as
follows:

~~~{data-language=haskell .nocheck}
type String = [Char]
~~~

```instruction
 

Give your string variable from the previous chapter a type-signature.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myString :: String
myString = "Hello Haskell"
~~~

## Tuples

Tuple type signatures look the same as the tuples themselves, with
types in place of the data.

For example, if you had a tuple of a String and an Int, the type
would look as follows:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myTuple :: (String, Int)
myTuple = ("The meaning of life", 42)
~~~

```instruction
Give your previous tuple definition a type signature.
```

## Functions

The type signatures of functions in Haskell are a little different
from how they look in the more familiar C family languages,
but the syntax is very elegant, and will allow a higher-level of
reasoning than less consistant forms.

The syntax for a function type-signarure is of the form:

~~~{data-language=haskell .nocheck}
{functionName} :: {argument} -> {result}
~~~

The main idea is that functions in Haskell only ever take one
argument. If you wish to define a function that takes more
than one argument, then you should, infact, define a function
that takes one argument, then returns another function.

Luckily the syntax for doing this in Haskell looks identical
to defining a multi-argument function:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myMultiply x y z = x * y * z
~~~

However, the distinction becomes clear with the type-signature:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myMultiply :: Int -> (Int -> (Int -> Int))
myMultiply x y z = x * y * z
~~~

It is clear, that the function only takes one argument, then returns a function
(that only takes one argument, and returns a function
(that only takes one argument, that returns an Int.))

Fortunately, Haskell's function syntax is right-associative, allowing us to
drop the parentheses:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myMultiply :: Int -> Int -> Int -> Int
myMultiply x y z = x * y * z
~~~

```instruction
 

Define a function `myMultiply` that multiplies 4 numbers.
Give your function a type-signature
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myMultiply :: Int -> Int -> Int -> Int -> Int
myMultiply w x y z = w * x * y * z
~~~

## Lists

List type-signatures look like:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
list1 :: [Int]
list2 :: [Int]
list3 :: [String]

list1 = [1,2,3]
list2 = 1 : 2 : []
list3 = "hello" : "world" : []
~~~

More information about why lists can be used the way that they are is
contained in the [ADTs](#adts-algebraic-data-types) chapter.

```instruction
Define a variable containing a list.
```

```instruction
Give your variable a type-signature.
```

You can deconstruct a list by pattern matching the head and tail like so:

~~~{data-language=haskell .nocheck}
f (x:xs) = ...
~~~

```instruction
Define a function to get the first element of a list.
```

Note: In `Prelude` this function is called `head`.

~~~{.answer data-language=haskell data-filter=./resources/scripts/check.sh}
myHead (x:xs) = x -- This is a partial function, Beware!
~~~

```instruction
Define a variable containing the first element of your list.
```

~~~{.answer data-language=haskell .nocheck} 
myFirstElement = myHead myList
~~~

### Define Length

```instruction
Define a function that takes a list and returns the length.
```

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
-- In Haskell type-signature syntax, this is written as:
myLength :: [a] -> Int
myLength = undefined
~~~

Your solution should have the form of:

~~~{data-language=haskell .nocheck}
myLength []     = ...
myLength (x:xs) = ...
~~~

Things to consider:

* What is the length of an empty list? (the base case)
* What is the length of xs?

~~~{.answer data-language=haskell data-filter=./resources/scripts/check.sh}
mylength []     = 0
mylength (x:xs) = 1 + mylength xs
~~~

### Define `myMap`

```instruction

  
Define a function that takes a function from a to b "a -> b",
and a list of as "[a]", and returns a list of bs "[b]".
```

Things to consider:

* What is the type-signature of myMap?
* What is the base-case of myMap?

~~~{.answer data-language=haskell data-filter=./resources/scripts/check.sh}
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
~~~

## Fun List Functions

For your reading pleasure, here are some definintions of common functions:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myFilter f []     = []
myFilter f (x:xs) = if f x then x : myFilter f xs
                           else     myFilter f xs

myFold f z []     = z
myFold f z (x:xs) = f x (myFold f z xs)

myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

myElem e []     = False
myElem e (x:xs) = if e == x then True
                            else myElem e xs
~~~

See if you can determine the type-signatures for these functions.

```open
An open-ended question:

What is a good balance between safety and expressiveness in a
programming-language?
```
