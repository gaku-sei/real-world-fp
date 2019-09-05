_Link 1: https://sketch.sh/s/jr9KkndXjGNXpUcABLtkar/_

```reasonml
module Array = {
  let flatMap = (f, xs) =>
    Array.to_list(xs) |> List.map(x => f(x)) |> Array.concat;

  let length = Array.length;

  let map = Array.map;

  let (<$>) = map;

  let make = Array.make;

  let set = Array.set;
};

module Option = {
  let map = (f, o) =>
    switch (o) {
    | Some(x) => Some(f(x))
    | None => None
    };

  let (<$>) = map;

  let flatMap = (f, o) =>
    switch (o) {
    | Some(x) => f(x)
    | None => None
    };

  let (>>=) = (o, f) => flatMap(f, o);

  let andMap = (f, o) =>
    switch (f, o) {
    | (Some(f), Some(o)) => Some(f(o))
    | _ => None
    };

  let (<*>) = andMap;
};

module Result = {
  type t('a, 'error) =
    | Ok('a)
    | Error('error);

  let map = (f, r) =>
    switch (r) {
    | Ok(x) => Ok(f(x))
    | Error(error) => Error(error)
    };

  let (<$>) = map;

  let flatMap = (f, r) =>
    switch (r) {
    | Ok(x) => f(x)
    | Error(error) => Error(error)
    };

  let (>>=) = (r, f) => flatMap(f, r);

  let andMap = (f, r) =>
    switch (f, r) {
    | (Ok(f), Ok(r)) => Ok(f(r))
    | (Error(error), _) => Error(error)
    | (_, Error(error)) => Error(error)
    };

  let (<*>) = andMap;
};

module Int = {
  let fromString = s =>
    try (Some(int_of_string(s))) {
    | _ => None
    };
};

module String = {
  let length = String.length;
};


module Fun = {
  let id = x => x;

  let const = (x, _) => x;
};
```

_Link 2: https://sketch.sh/s/eWFZQaqrqqWaT9PRq3V6PR/_

# Basics

Functional programming often revolves around using functions... But that's not the only important concept!

Here are some things we will discuss, and how they can be useful with some real world examples:

- Variants/ADT
- Pattern matching
- Opaque types
- Phantom types
- GADT
- Optionals/Results
- Category theory (a very few examples)
- Monads
- ...

## Variants/ADT

Adt stands for Algebraic Data Type

```reasonml
/* Do you remember redux? And using it with TypeScript? Here is a reducer in ReasonML */

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
  | Decrement => state + 1
  | Set(newState) => newState
  | Add(value) => state + value
  | Reset => 0
  };

let newState = reducer(2, Add(3));

/* We can even use this pattern in function definitions */

let reducer' = state =>
  fun
  | Increment => state + 1
  | Decrement => state + 1
  | Set(newState) => newState
  | Add(value) => state + value
  | Reset => 0;

let newState' = reducer(23, Increment);
```

## Records

Records are fixed structures in ReasonML:

```reasonml
type person = {
  id: int,
  age: int,
  name: string,
};

/* You can use pattern matching */
let canDrink =
  fun
  | {name: "Chuck Norris"} => "Of course!"
  | {age} when age >= 20 => "It's probably ok"
  | {age} when age == 19 => "Too bad"
  | _ => "No way!";
```

## Opaque types?

It can be sometimes important to disambiguate types that carry the same kind of values:

```reasonml
let setAge = (id: int, age: int) => (); /* We do something here */

/* I need to change the age for the person with id 20, but how can I guess the order of the arguments? */

setAge(20, 24);

setAge(24, 20);

/* ??? */

type id =
  | Id(int);

type age =
  | Age(int);

type person' = {
  id,
  age,
  name: string,
};

let setAge' = (id: id, age: age) => ();

/* Now I know how to call it! */

setAge'(Id(20), Age(24));

/* Notice that ReasonML has some neat named arguments to solve this issue too: */
let setAge'' = (~id: int, ~age: int) => ();

setAge''(~id=20, ~age=24);
```

## ~Obake~ Phantom types!!

Very useful when you want to validate some data a runtime, like a form for instance, and ensure that no one tries to send some invalid data. Impossible? Not with the fancy phantom types!

```reasonml
/* These types can't be built. */
type validated;
type unvalidated;

/* We need define the type first, so we can hide the inner representation of our phantom type */
module type PersonFormData = {
  /* Our phantom type. */
  type t(_);

  /* We need to expose some creator for our type. */
  let create: (~age: int, ~name: string) => t(unvalidated);

  let validate: t(unvalidated) => option(t(validated));

  /* We can only send validated values. */
  let send: t(validated) => unit;
};

module PersonFormData: PersonFormData = {
  /* Our phantom type. */
  type t(_) = person;

  let validate = ({age, name} as person: person): option(t(_)) =>
    age >= 0 && Std.String.length(name) >= 1 ? Some(person) : None;

  let create = (~age, ~name): person => {age, name, id: 1};

  let send = a => ();
};

let personData = PersonFormData.create(~age=20, ~name="foobar");

/* Will only send valid data. */
Std.Option.map(PersonFormData.send, PersonFormData.validate(personData));

/* PersonFormData.send(personData); /* Doesn't compile! */ */
```

# Other common patterns

## Category theory

[What??](https://imgflip.com/i/39q7ey)

Everywhere: lists (arrays), promises, functions, strings, optional types, etc...

### Functors

The most common one: Functor (!= OCaml's Functor)
A functor can be mapped over, like a list, or a promise, and is therefore compliant with the `map` function.

```reasonml
/* Notice that arrays are not lists, and don't behave exactly the same.
      Here we will access arbitrary indexes and arrays are more efficient.
   */
let xs = [|1, 2, 3|];

/* We want to add 1 to every entries */

/* Imperative way: */

/* Creates an array of length similar to xs and with values being all 0 by default */
let xs' = Std.Array.make(Std.Array.length(xs), 0);

for (index in 0 to Std.Array.length(xs) - 1) {
  Std.Array.set(xs', index, xs[index] + 1);
};

xs';

/* Functional way: let's use a function! */

let xs' = Std.Array.map(x => x + 1, xs);
```

### One word on options

Nullable values can be hard to deal with. In OCaml/ReasonML they are explicitely wrapped in an `option` type:

```reasonml
/* Notice how both values have the same type */
let optionalValue: option(int) = Some(42);
let optionalEmptyValue: option(int) = None;

/* First approach: pattern matching */
switch (optionalValue) {
| Some(value) => Some(value + 1)
| None => None
};

switch (optionalEmptyValue) {
| Some(value) => Some(value + 1)
| None => None
};

/* Second approach: using functors' map! */
Std.Option.map(x => x + 1, optionalValue);

Std.Option.map(x => x + 1, optionalEmptyValue);
```

### An other useful category: Applicatives

An applicative is a superset of a Functor, it contains both a `map` and an `andMap` functions (sometimes called differently).

```reasonml
/* We received some strings from different inputs, and we want to sum them all! */

let x = Std.Int.fromString("1");
let y = Std.Int.fromString("2");
let z = Std.Int.fromString("3");

let sumInputs = (x, y, z) => x + y + z;

let f = Std.Option.map(sumInputs, x);

/* Nice, now we have an option containing a function........ */
/* And that's where Applicatives strike! */

let g = Std.Option.andMap(f, y);

/* And again */

let h = Std.Option.andMap(g, z);

/* More efficiently, in one time! */

open Std.Option;

let f' = andMap(andMap(map(sumInputs, x), y), z);

/* Quite ugly, what about operators? */
/* Where <$> is map, and <*> is andMap */

let result = sumInputs <$> x <*> y <*> z;
```

### Monads

Monads a superset of Applicative, it therefore contains `map`, and `andMap`, but also the `flatMap` functions.
Options, lists, arrays, etc... are all monads already!

```reasonml
/* What if our inputs above were also optionals? */
let x = Some("1");
let y = None;
let z = Some("3");

/* It can be super tedious to work with the above values */

let x' = Std.Option.map(x => Std.Int.fromString(x), x);

/* And so on with the other values... It also prevents us from using andMap map.
      Finally, we will need some deeply nested pattern matching... Or we can use the Monad
      capabilities of the option type!
   */

let x'' = Std.Option.flatMap(x => Std.Int.fromString(x), x);

/* Still too verbose! */
/* >>= == flatMap */

module Int = Std.Int;
open Std.Option;

let result =
  sumInputs
  <$> (x >>= Int.fromString)
  <*> (y >>= Int.fromString)
  <*> (z >>= Int.fromString);

/* With only Some values */
let result' =
  sumInputs
  <$> (Some("1") >>= Int.fromString)
  <*> (Some("2") >>= Int.fromString)
  <*> (Some("3") >>= Int.fromString);
```

# Handling errors, the monadic way

You can use exceptions in ReasonML

```reasonml
exception MyExn(string);

let myExn = MyExn("foo");

/* Simple try/catch, notice how we can match the whole exception type. */

try (raise(myExn)) {
| MyExn(what) => print_string(what)
};

/* We can even catch when pattern matching. */

let weirdFunctionThatRaises = (): option(int) => raise(myExn);

print_string(
  switch (weirdFunctionThatRaises()) {
  | Some(x) => "Got an int!"
  | None => "Got nothing"
  | exception (MyExn(value)) => value
  | exception _ => "What happened?"
  },
);
```

But most of the time (if not all the time), you will prefer the `result` type. It's also a monad (and therefore an applicative functor)!

```reasonml
/* Could be an exception, it's perfectly ok, as long as we don't raise it. */
type parseIntError =
  | ParseIntError;

let parseInt = (input: string): Std.Result.t(int, parseIntError) =>
  /* Let's pretend there is some computation here. An error could also be returned. */
  Ok(42);

switch (parseInt("42")) {
| Ok(value) => value
| Error(_) => 0
};

/* Of course, we can use the super nice monad operators! */

open Std.Result;

sumInputs <$> parseInt("42") <*> parseInt("42") <*> parseInt("42");
```

# GADT: Implementing a Promise-like type

## The naive way

What we probably want to do:

- Handles resolve (everything was ok) and reject (errors)
- Can be mapped over (a Promise is a Functor)
- Can be ran once

```reasonml
module PromiseNaive = {
  /* We're using variants, as already saw earlier */
  type t('resolve, 'reject) =
    | Pending('resolve)
    | Resolved('resolve)
    | Rejected('reject);
  /* | ResolvedNoData; /* Since we will often ignore some value when pattern matching, adding a new value would break many of the following functions. */ */

  let create = (value: 'resolve): t('resolve, 'reject) => Pending(value);

  /* The following functions are verbose, and dangerous as stated above. */
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

  let map =
      (f: 'resolveIn => 'resolveOut, promise: t('resolveIn, 'reject))
      : t('resolveOut, 'reject) =>
    switch (promise) {
    | Pending(v) => create(f(v))
    | _ => assert(false) /* Fails at runtime, could be solved with an "empty" promise of type t(unit, 'reject), but then it leads to undesired behaviors. */
    };

  let run =
      (f: 'resolveIn => 'resolveOut, promise: t('resolveIn, 'reject))
      : t('resolveOut, 'reject) =>
    switch (promise) {
    | Pending(v) => Resolved(f(v))
    | _ => assert(false) /* Same here. */
    };
};

let promise = PromiseNaive.create(42);

promise |> PromiseNaive.map(_ => "foo") |> PromiseNaive.run(print_string); /* Works */
/* promise |> PromiseNaive.run(print_string) |> PromiseNaive.run(print_string); /* Fails at runtime */ */
/* promise |> PromiseNaive.run(_ => "foo") |> PromiseNaive.map(_ => 42); /* Fails at runtime */ */
```

Conclusion: the above works, but it's also a bit verbose, and can be dangerous at runtime.

## The GADT way

### What's GADT?

Generalized Algebraic Data Type. They are basically an "extension", an "improvement", of the traditional ADT/Variants pattern.

```reasonml
/* ADT */
type myOption('a) =
  | MySome('a)
  | MyNone;

/* GADT */
type myOption(_) =
  | MySome('a): myOption('a)
  | MyNone: myOption(_);

/* GADT is super flexible. */

type myOption =
  /* Can be opaque. */
  | MySome('a): myOption
  | MyNone: myOption;

type myOption(_, _) =
  /* Can have extra arguments */
  | MySome('a): myOption('a, _)
  | MyNone: myOption(_, _);

type myOption(_, _, _) =
  /* And can go crazy */
  | MySome('a): myOption('a, int, float)
  | MyNone: myOption(_, string, list(option(int)));
```

## How is it useful? Promise, the GADT way

```reasonml
module PromiseGadt = {
  type pending;
  type fulfilled;

  type t(_, _, _) =
    | Pending('resolve): t('resolve, _, pending)
    | Resolved('resolve): t('resolve, _, fulfilled)
    | Rejected('reject): t(_, 'reject, fulfilled);
  /* | ResolvedNoData: t(_, _, fullfilled); /* New value would be caught at compile time, since our patterns are now exhaustive. */ */

  let create = (value: 'resolve): t('resolve, 'reject, pending) =>
    Pending(value);

  /* No need for `is*` functions here, the type already carries this information
        Here is a dumb implementation if needed:
     */
  let isPending = (_: t('resolve, 'reject, pending)): bool => true;

  let isFulfilled = (_: t('resolve, 'reject, fulfilled)): bool => true;

  let map =
      (
        type resolveIn,
        f: resolveIn => 'resolveOut,
        promise: t(resolveIn, 'reject, pending),
      )
      : t('resolveOut, 'reject, pending) =>
    switch (promise) {
    | Pending(v) => create(f(v))
    };

  let run =
      (
        f: 'resolveIn => 'resolveOut,
        promise: t('resolveIn, 'reject, pending),
      )
      : t('resolveOut, 'reject, fulfilled) =>
    switch (promise) {
    | Pending(v) => Resolved(f(v))
    };
};

let promise: PromiseGadt.t(int, exn, PromiseGadt.pending) =
  PromiseGadt.create(42);

promise |> PromiseGadt.map(_ => "foo") |> PromiseGadt.run(print_string); /* Works */
/* promise |> PromiseGadt.run(Js.log) |> PromiseGadt.run(Js.log); /* Fails at compile time */ */
/* promise |> PromiseGadt.run(_ => "foo") |> PromiseGadt.map(_ => 42); /* Fails at compile time */ */
```

We can even be a bit fancier:

```reasonml
module PromiseFancyGadt = {
  type pending;
  type fulfilled;

  type t(_, _, _) =
    | Pending('resolve): t('resolve, _, pending)
    | Resolved('resolve): t('resolve, _, fulfilled)
    | Rejected('reject): t(_, 'reject, fulfilled);
  /* | ResolvedNoData: t(_, _, fullfilled); /* New value would be caught at compile time, since our patterns are now exhaustive. */ */

  let create = value => Pending(value);

  /* No need for `is*` functions here, the type already carries this information
        Here is a dumb implementation if needed:
     */
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

let promise: PromiseFancyGadt.t(int, exn, PromiseFancyGadt.pending) =
  PromiseFancyGadt.create(42);

promise
|> PromiseFancyGadt.map(_ => "foo")
|> PromiseFancyGadt.run(print_string); /* Works */
```

It works, it's less verbose, more robust, without any runtime issues, nor undesired behaviors...

# And more, way more...

You also can use modules, functors (the OCaml's ones), private types, etc... So many ways to improve your code safety, and readability, by keeping it as terse as possible.
