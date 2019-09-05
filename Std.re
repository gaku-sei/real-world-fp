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
