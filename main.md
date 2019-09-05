---
marp: true
title: Real world functional programming with ReasonML
paginate: true
class: invert
size: 4k
style: |
  code {
    color: white;
  }
---

# Real world functional programming with ReasonML

---

# 0. Basics

---

Functional programming = functions!

But not only!

Here are some concepts we will discuss about, and see how they can be useful with some real world examples:

- Variants/ADT
- Pattern matching
- Opaque types
- Phantom types
- GADT
- Optionals/Results
- Category theory (a very few examples), Monads, etc...

---

## Why, and how, ReasonML?

- Translates to OCaml, and is 99% compatible with the OCaml feature set
- Compiles to JavaScript using the [BuckleScript](https://bucklescript.github.io/) toolchain
  - Easy to set up and to use
  - The whole npm ecosystem is available
- Targets the web, front, and back, and allows for straight, simple, examples
- Has a limited amount of features, making these languages easy to learn (!= Haskell or PureScript)
- 99% type safe (at least)
- Blazing fast compile time (~20ms in incremental mode on an average machine in a medium-big sized project ~2000 files)
- Crazily good type inference, that can be [proved](https://www.math.nagoya-u.ac.jp/~garrigue/papers/inria080613.pdf)
- Not necessary to explicit the type 95% of the time (edge cases need type annotation, when we need lobal abstract type for example)

---

## Notice

- `Std` refers to an OCaml/ReasonML module defined here: [https://sketch.sh/s/jr9KkndXjGNXpUcABLtkar/](https://sketch.sh/s/jr9KkndXjGNXpUcABLtkar/)
- `BuckleScript` is the toolchain, it can "understand" both `OCaml` and `ReasonML` (which are the languages), and output JavaScript code

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)

---

# 1. Syntax

---

## Basic types

```reasonml
let unit: unit = ();

let x: int = 1;

let s: string = "foo";

let xs: list(bool) = [true, false, true];

let a: array(float) = [|1.2, 3.2, 5.3|];

let o: option(int) = Some(42);

let t: (int, string, bool) = (42, "foo", true);
```

---

## Arrays vs Lists

```reasonml
let list = [1, 2, 3];

let array = [|1, 2, 3|];
```

---

## Arrays vs Lists

|               | Lists                 | Arrays              |
| ------------- | --------------------- | ------------------- |
| Size          | small–medium          | small–large         |
| Resizable     | flexible              | fixed               |
| Mutability    | immutable             | mutable             |
| Element types | homogeneous           | homogeneous         |
| Access via    | destructuring         | index               |
| Fastest       | preprend/remove first | read/write elements |

---

## Records

Records are fixed structures in ReasonML:

```reasonml
type person = {
  id: int,
  age: int,
  name: string,
};

let canDrink =
  fun
  | {name: "Chuck Norris"} => "Of course!"
  | {age} when age >= 20 => "It's probably ok"
  | {age} when age == 19 => "Too bad"
  | _ => "No way!";
```

---

## Optional values

ReasonML doesn't have any `null` value (although there is a `unit` type as seen previously).

Defined as follow by the core library:

```reasonml
type option('a) = Some('a) | None;
```

Can be used this way:

```reasonml
// This function is unsafe and could fail
let parseInt = (x: string): option(int) = ...; // Computes intensively

let x = parseInt("42"); // x == Some(42)
let x' = parseInt("??"); // x == None
```

---

## Optional values

- Offers null safety
- Cannot break (!= former versions of TypeScript)
- Has to be handled explicitly

---

## Functions!

```reasonml
// Lightweight syntax, notice that there is no `return` keyword in ReasonML
let f = x => x + 1;

// Higher order functions
let applyTwice = (f, x) => f(f(x));

applyTwice(f, 1); // == 3

// With explicit type
let f = (x: int): int => x + 1;

// Labeled arguments (with default values, notice the final unit)
let parseInt = (x: int, ~radius:int=10, ()): option(int) = ...;

let x: option(int) = parseInt("42", ());

let x': option(int) = parseInt("2A", ~radius=16, ());
```

---

## Curried by default

![bg right:60%](https://myfoodstory.com/wp-content/uploads/2015/09/mutton-keema-or-spiced-lamb-mince-curry-recipe.1024x1024-2.jpg)

---

## Curried by default

```reasonml
let add = (x, y, z) => x + y + z;

let f = add(10); // has type (int, int) => int

let g = f(20); // has type int => int

let x = g(30); // has type int and value 60
```

- Works with labeled arguments

---

## Everything's an expression

```reasonml
let value = if (true) {
  12
} else {
  21
}; // value == 12

let value' = switch (true) {
  | true => 12
  | false => 12
}; // value' == 12
```

---

## A word on loops

- Hardly used
- Prefer built-in functions (map, etc...)
- Use recursivity

---

## Recursivity

- Recursive functions have to be marked as recursive

```reasonml
let rec sum = fun
| [] => 0
| [head, ...tail] => head + sum(tail);

let x = sum([1, 2, 3]); // x == 6
```

---

## Modules

A module is basically a set of functions, types, and even submodules!

```reasonml
module Math = {
  let add = (x, y) => x + y;

  let sub = (x, y) => x - y;
};

// Use the dot notation to access members
let x = Math.add(1, 2); // x == 3
```

---

## Module types

We can also define the types of our modules

```reasonml
module type MATH = {
  let add: (int, int) => int;

  let sub: (int, int) => int;
};

// And later:

module Math: MATH = {
  // ...
};
```

---

## Opening/Aliasing a module

We can access the elements defined in a module using the dot notation, or with an `open` statement

```reasonml
let x = Math.add(1, Math.add(2, 3));

// or

open Math;

let x' = add(1, add(2, 3));

// or even, using local opens

let x'' = Math.(add(1, add(2, 3)));

// We can also "alias" a module

module Whatever = Math;
```

---

## Functors

:warning: **Not to be mistaken with the functor related to the category theory!** :warning:

Behave as higher order modules, since they will take a module and use it in order to build an other module

```reasonml
module ImproveMath = (M: MATH) => {
  let addTwice = (x, y) => M.add(M.add(x, y), y);
};

module ImprovedMath = ImproveMath(Math);

let x' = ImprovedMath.addTwice(2, 3); // x == 8
```

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)

---

# 2. ADT/Variants

---

## ADT = Algebraic Data Type

Simple usage

```reasonml
type color = Red | Blue | Green;
//    ^^ name       ^^ constructors

let color: color = Red;

switch (color) {
  | Red => "red"
  | Blue => "blue"
  | Green => "green"
};
```

---

## How does it differ from a simple switch/case with some strings?

---

## How does it differ from a simple switch/case with some strings?

1. Shorter

---

## How does it differ from a simple switch/case with some strings?

1. Shorter
2. Can carry some payload...

---

## How does it differ from a simple switch/case with some strings?

1. Shorter
2. Can carry some payload...
3. ... and extract it!

```reasonml
type color = Hex(string) | RGB(int, int, int);

let color = Hex("#332244");

switch (color) {
  | Hex(hex) => ...
  | RGB(red, green, blue) => ...
}
```

---

## How does it differ from a simple switch/case with some strings?

1. Shorter
2. Can carry some payload...
3. ... and extract it!
4. Allows for `guards`...

```reasonml
type color = Hex(string) | RGB(int, int, int);

let color = RGB(256, 10, 32);

switch (color) {
  | Hex(_) => ...
  | RGB(red, _, _) when red <= 255 && red >= 0 => ...
  | ...
}
```

---

## How does it differ from a simple switch/case with some strings?

1. Shorter
2. Can carry some payload...
3. ... and extract it!
4. Allows for `guards`...
5. ... and for catch all pattern

```reasonml
type color = Red | Green | Blue;

let isRed = color =>
  switch (color) {
    | Red => true
    | _ => false
  };
```

---

## How does it differ from a simple switch/case with some strings?

1. Shorter
2. Can carry some payload...
3. ... and extract it!
4. Allows for `guards`...
5. ... and for catch all pattern
6. Values are _Limited_...

```reasonml
type color = Red | Blue;

let color = Red;

switch (color) {
  | Yellow => ... // Doesn't compile
  ...
}
```

---

## How does it differ from a simple switch/case with some strings?

1. Shorter
2. Can carry some payload...
3. ... and extract it!
4. Allows for `guards`...
5. ... and for catch all pattern
6. Values are _Limited_...
7. ...and _predictable_

```reasonml
type color = Red | Blue;

let color = Red;

switch (color) {
  | Red => ...
} // Warns us that Blue is not handled here
```

---

## How does it differ from a simple switch/case with some strings?

1. Shorter
2. Can carry some payload...
3. ... and extract it!
4. Allows for `guards`...
5. ... and for catch all pattern
6. Values are _Limited_...
7. ...and _predictable_
8. => **Enforces proper data representation**

---

## Data representation

:warning: The following is in TypeScript :warning:

```ts
interface Request<T> {
  isLoading: boolean;
  error: Error | null;
  data: T | null;
}
```

Not _necessarily_ invariant (what if we couldn't have an error _and_ some data at the same time?)

---

## Data representation

- `exn` is the exception type, more on this later
- `'a` stands for a polymorphic type

```reasonml
type request('a) = Loading | Failed(exn) | Success('a);
```

- Easy to read/write/edit
- Invariant
- Extensible (we can add a `NoData` constructor, it will be "caught" by the `switch` expressions)

---

## Data representation - Redux

An example using [Redux](https://redux.js.org/)

```reasonml
type actions =
  | Increment
  | Decrement
  | Set(int)
  | Add(int)
  | Reset;

type state = int;

let reducer = (state, action) =>
  switch (action) {
  | Increment => state + 1
  | Decrement => state - 1
  | Set(newState) => newState
  | Add(value) => state + value
  | Reset => 0
  };

let newState = reducer(2, Add(3));
```

---

## Data representation - Redux

We can also use this fancy feature to declare our `reducer`:

```reasonml
/* We can even use this pattern in function definitions */

let reducer' = state =>
  fun
  | Increment => state + 1
  | Decrement => state - 1
  | Set(newState) => newState
  | Add(value) => state + value
  | Reset => 0;

let newState' = reducer(23, Increment);
```

---

## Data representation - Redux

We could even go for something more functional by returning functions manipulating the state!

- Where `const` is a common function ignoring its second argument, and returning its first one.

```reasonml
let reducer'' = fun
  | Increment => (+)(1)
  | Decrement => (-)(1)
  | Set(newState) => const(newState)
  | Add(value) => (+)(value)
  | Reset => const(0);

let newState'' = reducer''(Add(3), 2);
```

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)

---

# 3. Opaque types

---

## Opaque types

It can be sometimes important to disambiguate types that carry the same kind of values:

```reasonml
// Let's say we have a function that change the age of a user in the db.
// In order to achieve that, the function needs to know the user's id, and the new age.
let setAge = (id: int, age: int) => ...; // Perform computation
```

---

## Opaque types

```reasonml
// Which one is the id, which one is the age??
setAge(20, 24);

setAge(24, 20);
```

---

## Opaque types

Here come some opaque types!

```reasonml
type id = Id(int);

type age = Age(int);

let setAge' = (id: id, age: age) => ...; // Perform computation

// Now calling the above function is clear, and explicit

setAge'(Id(20), Age(24));

// Notice that we can also use labeled arguments here:
let setAge'' = (~id: int, ~age: int) => ();

setAge''(~id=20, ~age=24);
```

---

## Opaque types

We can also use modules and module types to achieve this. The code is longer, but there is no extra memory allocation.

```reasonml
module Id: { type t; let wrap: int => t; let unwrap: t => int } = {
  type t = int;

  let wrap = x => x;

  let unwrap = x => x;
};

let f = (id: Id.t) => "Got id #" ++ string_of_int(Id.unwrap(id));

f(Id.wrap(42)); // == "Got id #42"
```

Notice that the above code could be refactored and simplified to allow for smaller definitions.

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)

---

# 4. Phantom types

---

## Phantom types

Let's say we have a set of functions for validating and sending some data to a server.
But we don't want one to send a dataset that has not been validated yet...

...at _compile time_.

1. Some types

```reasonml
// These types are abstract, meaning they don't have any constructor
type validated;
type unvalidated;

// This will be the record we want to "protect"
type person = {
  age: int,
  name: string,
};
```

---

## Phantom types

2. Some module _type_ definition

We need to define the type first, so we can hide the inner representation of our phantom type

```reasonml
module type PERSON_FORM_DATA = {
  // Our phantom type! (It will be more explicit later)
  type t(_);

  // We need to expose some creator for our type
  // Notice that the returned value is "unvalidated"
  let create: (~age: int, ~name: string) => t(unvalidated);

  // This functions will turn an "unvalidated" dataset to a "validated" dataset, if said dataset is valid
  let validate: t(unvalidated) => option(t(validated));

  // As specified above, we can only send validated data
  let send: t(validated) => unit;
};
```

---

## Phantom types

3. The implementation

```reasonml
module PersonFormData: PERSON_FORM_DATA = {
  // The phantom type is now clearly identifiable: it takes a type argument but doesn't use it!
  type t(_) = person;

  // Performs the validation
  let validate = ({age, name} as person: person): option(t(_)) =>
    age >= 0 && Std.String.length(name) >= 1 ? Some(person) : None;

  let create = (~age, ~name): person => {age, name};

  let send = person => ...; // Send to the server
};

let personData = PersonFormData.create(~age=20, ~name="foobar");

// Will only send valid data.
switch (PersonFormData.validate(personData)) {
  | None => () // Don't do anything
  | Some(data) => PersonFormData.send(data) // Send to server!
}

PersonFormData.send(personData); // Doesn't compile!
```

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)

---

# 5.

![bg fit right:80%](https://i.imgflip.com/39q7ey.jpg)

---

### **They are everywhere: lists and arrays, promises, functions, strings, optional types, etc...**

---

## Functors (!= OCaml's Functor)

Probably the most common one!
A functor can be mapped over, like a list, or a promise in JavaScript, and offers a `map` function.

Let's say we want to add 1 to each entries of an array.

Using the imperative paradigm:

```reasonml
let xs: array(int) = [|1, 2, 3|];


// Creates an array of length similar to xs with values set to 0 by default
let xs' = Std.Array.make(Std.Array.length(xs), 0);

// Iteration!
for (index in 0 to Std.Array.length(xs) - 1) {
  Std.Array.set(xs', index, xs[index] + 1);
};

xs'; // xs' == [|2, 3, 4|]
```

---

## Functors

Functional way

```reasonml
let xs'' = Std.Array.map(x => x + 1, xs); // xs'' == [|2, 3, 4|]
```

---

### Option is also a functor!

```reasonml
// Both values have the same type
let optionalValue: option(int) = Some(42);
let optionalEmptyValue: option(int) = None;

/// First approach: pattern matching
switch (optionalValue) {
| Some(value) => Some(value + 1)
| None => None
}; // == Some(43)

switch (optionalEmptyValue) {
| Some(value) => Some(value + 1)
| None => None
}; // == None

// Second approach: using functors' map!
Std.Option.map(x => x + 1, optionalValue); // == Some(43)

Std.Option.map(x => x + 1, optionalEmptyValue); // == None

// Notice that we can also use currying here
Std.Option.map((+)(1), optionalValue);
```

---

## An other useful category: Applicative

An applicative is a superset of a Functor, it contains both a `map` and an `andMap` functions (sometimes called differently).

In practice: we receive some strings from different (form) inputs, and we want to sum them all!

```reasonml
// The received inputs, parsed
let x = Std.Int.fromString("1"); // x == Some(1)
let y = Std.Int.fromString("2"); // y == Some(2)
let z = Std.Int.fromString("3"); // z == Some(3)

// The "sum" function
let sumInputs = (x, y, z) => x + y + z;
```

---

## Applicative

```reasonml
// First, let's apply our function to the first optional value
let f = Std.Option.map(sumInputs, x);
// f has type option((int, int) => int)
```

Nice we now have an option containing a function... Could it get even worse?

---

## Applicative: andMap

`andMap` will take 2 applicatives, extract their values, and apply the first value to the second one!

```reasonml
// We provide the two optionals
// andMap will apply the function contained in f to the value contained in y...
let g = Std.Option.andMap(f, y);

// ... and return a value of type option(int => int)

// One more time
let h = Std.Option.andMap(g, z); // h has type option(int) and equals Some(6)
```

---

## Applicative

In real world, we would probably chain the function calls

```reasonml
open Std.Option;

let f' = andMap(andMap(map(sumInputs, x), y), z);

// Or, using the -> operator offered by BucklesScript and ReasonML

sumInputs->map(x)->andMap(y)->andMap(z);

// We can even use the operator aliases!
// Where <$> == map and <*> == andMap

sumInputs <$> x <*> y <*> z;
```

---

## Monad

Monad is a superset of Applicative, it therefore contains `map`, and `andMap`, but also the `flatMap` functions.
Options, lists, arrays, etc... are all monads already!

- Often aliased as `>>=`

---

## Monad

With the use case as above, let's add some complexity.
The inputs have to be parsed, like before, but they are also optional!

```reasonml
// The received inputs
let x = Some("1");
let y = None;
let z = Some("3");

// The "sum" function, again
let sumInputs = (x, y, z) => x + y + z;
```

---

## Monad

It will be tedious to work with the parsed values

```reasonml
// Let's use the good old `map` function...
let x' = Std.Option.map(Std.Int.fromString, x);
// x' will have type option(option(int)) and value Some(Some(1))
let y' = Std.Option.map(Std.Int.fromString, y);
// y' will have type option(option(int)) and value None
let z' = Std.Option.map(Std.Int.fromString, z);
// z' will have type option(option(int)) and value Some(Some(3))
```

---

Woah...

![bg right 150%](https://thumbs.gfycat.com/ThisSomberAmericanwigeon-max-1mb.gif)

---

## Monad: flatMap

:warning: Not actual ReasonML code :warning:

`map` has type `(a -> b) -> f a -> f b`
`flatMap` has type `(a -> f b) -> f a -> f b`

```reasonml
let x'' = Std.Option.flatMap(Std.Int.fromString, x);
// x'' has type option(int) and value Some(1)
```

---

## Monad

```reasonml
// >>= is a common alias for flatMap

// First, let's open, and alias some modules
module Int = Std.Int;
open Std.Option;

// And let's make it work!
let result =
  sumInputs
    <$> (x >>= Int.fromString)
    <*> (y >>= Int.fromString)
    <*> (z >>= Int.fromString); // result == None

let result' =
  sumInputs
    <$> (Some("1") >>= Int.fromString)
    <*> (Some("2") >>= Int.fromString)
    <*> (Some("3") >>= Int.fromString); // result == Some(6)
```

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)

---

# 6. Handling errors, the monadic way

---

## Exceptions

- Exist in OCaml/ReasonML
- Have all the same type `exn` (they use the type extension capability of OCaml)
- Behave and work the same way as any other types/constructors...
- ... until they are "raised"
- The `raise` function returns `'a`, meaning it can be used anywhere

```reasonml
// This exception has type exn, and can be "built" with the MyExn constructor
exception MyExn(string);

let myExn = MyExn("foo");

// Simple try/catch mechanic
try (raise(myExn)) {
| MyExn(what) => Js.log(what)
};
```

---

## Exceptions - Switch

We can even catch inside switch expressions!

```reasonml
let weirdFunctionThatRaises = (): option(int) => raise(MyExn("foo"));

switch (weirdFunctionThatRaises()) {
| Some(x) => "Got an int!"
| None => "Got nothing"
| exception (MyExn(value)) => value
| exception _ => "What happened?"
}; // ???
```

---

## Exceptions - Switch

We can even catch inside switch expressions!

```reasonml
let weirdFunctionThatRaises = (): option(int) => raise(MyExn("foo"));

switch (weirdFunctionThatRaises()) {
| Some(x) => "Got an int!"
| None => "Got nothing"
| exception (MyExn(value)) => value
| exception _ => "What happened?"
}; // == "foo"
```

---

## Exceptions, pros/cons

Pros:

- Don't "taint" functions
- Useful in some fields where we actually want to terminate our application on critical error
- Can be caught

Cons:

- Terminates the program, which is not what we want in web most of the time
- Don't "taint" functions, meaning it can be hard to know which function "raises"

Best practices:

- Suffix raising functions with `Exn`, e.g.: `parseInt -> parseIntExn`

---

## Result

Most of the time, we will prefer the `result` type. It's also a monad (and therefore an applicative functor)!

```reasonml
// They can be defined this way:
type result('ok, 'error) =
  | Ok('ok)
  | Error('error);
```

---

## Result

Examples

```reasonml
// Could be an exception as well, again they work almost the same as normal types
type parseIntError =
  | ParseIntError;

let parseInt = (input: string): Std.Result.t(int, parseIntError) =>
  // Let's pretend there is some computation here
  // An error could also be returned
  Ok(42);

switch (parseInt("42")) {
| Ok(value) => value
| Error(_) => 0
}; // == 42

// Of course, we can use the super fancy monadic operators!

open Std.Result;

sumInputs <$> parseInt("42") <*> parseInt("42") <*> parseInt("42");
```

---

## Result - Common practices

- Result are also _bifunctor_, so we can map the ok value, and the error value...
- ... It's extremely useful when dealing with _heterogeneous result_ values...
- ... Since `result(int, parseError)` is _not_ compatible with `result(int, argumentError)` for example
- Result can be turned into an `option`, discarding the error message...
- ... And can be built from an `option`

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)

---

# 7. GADT

---

## Problem: implementing a Promise-like type

- Has a "pending" status
- Handles `resolve` (everything was ok)...
- ... And `reject` (errors)
- Can be mapped over (a Promise is a Functor), only when the promise is not fulfilled
- Can be ran only once (for performance reasons)
- Provides some status utility functions (`isPending`, etc...)

---

## The naive way

The main module with its type

```reasonml
module Promise = {
  // We're using variants, as already saw earlier
  type t('resolve, 'reject) =
    | Pending('resolve)
    | Resolved('resolve)
    | Rejected('reject);

  // | ResolvedNoData; // A new constructor could be useful one day

  // Since we will often ignore some value when pattern matching
  // adding a new constructor here would break
  // many of the following functions

  // ...
```

---

## The naive way

Implementation, utility functions

```reasonml
  // ...

  // Utilities: creation, status, etc...
  let create = (value: 'resolve): t('resolve, 'reject) => Pending(value);

  // The following functions are verbose, and dangerous as stated above
  let isPending = (promise: t('resolve, 'reject)): bool =>
    switch (promise) {
    | Pending(_) => true
    | _ => false
    };

  let isFulfilled = (promise: t('resolve, 'reject)): bool =>
    switch (promise) {
    | Resolved () => true
    | _ => false
    };

  // ...
```

---

## The naive way

End of the implementation, mapping, and running our promises

```reasonml
  // ...

  let map =
      (
        f: 'resolveIn => 'resolveOut,
        promise: t('resolveIn, 'reject)
      ): t('resolveOut, 'reject) =>
    switch (promise) {
    | Pending(v) => create(f(v))
    | _ => assert(false)
    // Fails at runtime
    // Could be solved with an "empty" promise of type t(unit, 'reject)
    // but then it could leads to undesired/misleading behaviors
    };

  let run =
      (
        f: 'resolveIn => 'resolveOut,
        promise: t('resolveIn, 'reject)
      ) : t('resolveOut, 'reject) =>
    switch (promise) {
    | Pending(v) => Resolved(f(v))
    | _ => assert(false)
    };
}; // End of the Promise module
```

---

## The naive way

Usage

```reasonml
let promise = Promise.create(42);

// The following works perfectly!
promise
  |> Promise.map(_ => "foo")
  |> Promise.run(Js.log);


// But the following would fail at runtime
// Or, depending on the implementation do nothing (which can be misleading)
promise
  |> Promise.run(Js.log)
  |> Promise.run(Js.log);

// This one would also fail at runtime
promise
  |> Promise.run(_ => "foo")
  |> Promise.map(\_ => 42);
```

---

## The naive way - Conclusion

The naive implementation works, but:

- It can be dangerous or at best misleading in some common use cases
- The implementation can be also verbose, because of the utility functions
- It doesn't scale well

---

## GADT

Generalized Algebraic Data Type. They are basically an "extension", or an "improvement", of the traditional ADT/Variants pattern.

```reasonml
// ADT
type myOption('a) =
  | MySome('a)
  | MyNone;

// GADT
type myOption(_) =
  | MySome('a): myOption('a)
  | MyNone: myOption(_);
```

---

## GADT

It's also more flexible, as it can be opaque

```reasonml
type myOption =
  | MySome('a): myOption
  | MyNone: myOption;
```

There is now no way to retrieve the inner value `'a`!!

---

## GADT

It can act as a phantom type too

```reasonml
type myOption(_, _) =
  | MySome('a): myOption('a, _)
  | MyNone: myOption(_, _);

// It can even go super crazy!
type myOption(_, _, _) =
  | MySome('a): myOption('a, int, float)
  | MyNone: myOption(_, string, list(option(int)));
```

---

## How is it useful? Promise, the GADT way

```reasonml
module Promise = {
  type pending;
  type fulfilled;

  type t(_, _, _) =
    | Pending('resolve): t('resolve, _, pending)
    | Resolved('resolve): t('resolve, _, fulfilled)
    | Rejected('reject): t(_, 'reject, fulfilled);

  // | ResolvedNoData: t(_, _, fullfilled);

  // New values would be "caught" by the switch expression
  // at compile time since our patterns will now be exhaustive

  // ...
```

---

## The GADT way

Implementation, utility functions

```reasonml
  // ...

  // Utilities: creation, status, etc...
  let create = (value: 'resolve): t('resolve, 'reject, pending) =>
    Pending(value);

  // No need for the `is*` functions here
  // since the type already carries this information
  // Here is a dumb implementation if needed:
  let isPending = (_: t('resolve, 'reject, pending)): bool => true;

  let isFulfilled = (_: t('resolve, 'reject, fulfilled)): bool => true;

  // ...
```

---

## The GADT way

End of the implementation, mapping, and running our promises

```reasonml
  // ...

  let map =
      (
        type resolveIn, // A local abstract type is need here
        f: resolveIn => 'resolveOut,
        promise: t(resolveIn, 'reject, pending),
      ): t('resolveOut, 'reject, pending) =>
    switch (promise) {
    | Pending(v) => create(f(v))
    }; // Notice how we don't even need to handle the other cases!

  let run =
      (
        f: 'resolveIn => 'resolveOut,
        promise: t('resolveIn, 'reject, pending),
      ): t('resolveOut, 'reject, fulfilled) =>
    switch (promise) {
    | Pending(v) => Resolved(f(v))
    }; // Same here!
}; // End of the Promise module
```

---

## The GADT way

Usage

```reasonml
let promise: Promise.t(int, exn, Promise.pending) =
  Promise.create(42);

// The following works fine
promise
  |> Promise.map(_ => "foo")
  |> Promise.run(Js.log);

// Doesn't compile!
promise
  |> Promise.run(Js.log)
  |> Promise.run(Js.log);

// Doesn't compile!
promise
  |> Promise.run(_ => "foo")
  |> Promise.map(_ => 42);
```

---

## An other, fancier, implementation using GADT

```reasonml
module Promise = {
  type pending;
  type fulfilled;

  type t(_, _, _) =
    | Pending('resolve): t('resolve, _, pending)
    | Resolved('resolve): t('resolve, _, fulfilled)
    | Rejected('reject): t(_, 'reject, fulfilled);

  let create = value => Pending(value);

  // Still useless
  let isPending =
    fun
    | Pending(_) => true;

  let isFulfilled =
    fun
    | Resolved(_)
    | Rejected(_) => true;

  let map = (type resolveIn, f) =>
    fun
    | Pending(v) => create(f(v));

  let run = f =>
    fun
    | Pending(v) => Resolved(f(v));
};

// Same usage
```

---

## The GADT way - Conclusion

It works, it's less verbose, more robust, without any runtime issues, nor undesired behaviors

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)

---

# 8. And more, way more...

---

## There's more

We also can use modules, functors (the OCaml's ones), private types, extensible types (like `exn`), etc... So many ways to improve our code safety, and readability, by keeping it as short as possible.

---

# 9. Links

---

## Links

The `Std` library used in this presentation can be found here: https://sketch.sh/s/jr9KkndXjGNXpUcABLtkar/_

Also, the above examples, can be found here: https://sketch.sh/s/eWFZQaqrqqWaT9PRq3V6PR/_

---

![bg 15%](https://i.pinimg.com/originals/7c/e7/6e/7ce76e2ad69c01b9e3949a466dd4a2aa.png)
