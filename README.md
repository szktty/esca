# Esca

Esca is an experimental strongly-typed language which compiles to Go.

## Design Goals

The main goal is type-safe programming which makes the best of features of Go.

- Type Safety: Strongly typing, type inference.
- Interoperable: Using goroutine, calling libraries in Go.
- Friendly Syntax: Syntax like Go, simplifies idioms of Go.
- Enumeration: Defining common types for grouping related values.
- Pattern Matching: Comparing and destructing values easily.
- Bitstring: Constructing binaries and matching binary patterns.
- Generics: Avoiding duplication of definitions.
- Tail Recursion Optimization: Supporting efficiency tail recursion functions.

## Requirements

- OCaml 4.04.2+
- OPAM 1.2.2+
- OMake 0.9.8.6-0.rc1
- Core 0.9.1+
- Menhir 20170607

## Installing Esca

```
$ opam pin add omake 0.9.8.6-0.rc1
$ opam pin add esca .
```

## License

Apache License, Version 2

## Build Tool

[Rugo](https://github.com/szktty/rugo)

## Specification

### Top-Level Execution

```
func main() {
    ...
}
```

## Syntax

IMPLEMENTING

### Comments

```
// comment
```

### Functions

#### Function Call

```
f(x, y, z)
```

#### Function Definition

```
func add(x: Int, y: Int) {
    return x + y
}
```

#### Function Definition (Tail recursion optimization)

```
tailrec func findFixPoint(x: Double) -> Double {
    if x == math.cos(x) {
        return x
    } else {
        return findFixPoint(math.cos(x))
    }
}
```

#### Anonymous Functions

```
// no arguments
{ print("hello") }

// no return
{ x, y in show(x + y) }

// return
{ x, y in return x + y }
```

### Contral Flow

#### Conditional

```
if x = y {
    print("x = y")
} else {
    print("x != y")
}
```

#### Guard

NOT YET IMPLEMENTED

```
guard is_ok else {
    print("not ok")
}
```

```
guard name := get_name() else {
    print("no name")
}
print(name)
```

#### Loop

```
for i in 0...10 {
    show(i)
}
```

#### Pattern Matching

```
func fizzbuzz(i: Int) {
    switch (i % 3, i % 5) {
    case (0, 0):
        print("FizzBuzz")
    case (0, _):
        print("Fizz")
    case (_, 0):
        print("Buzz")
    default:
        show(i)
    }
}
```

#### Return

```
return
return value
```

### Error Handling

```
pass file := os.Open("foo.txt") else { error in
    log.Fatal(error)
}
```

In Go:

```
file, err := os.Open("foo.txt")
if err != nil {
    log.Fatal(error)
}
```

### Literals

#### Void

```
()
```

#### Boolean

```
true
false
```

#### Integer

#### Floating-Point Numbers

#### String

```
"Hello, world!"
```

#### Reference (Pointer)

```
&i
```

#### List

```
[] // empty
[1, 2, 3]
```

#### Tuple

```
(1, 2, 3)
```

#### Map

```
[:] // empty
["a": 1, "b": 2, "c": 3]
```

#### Range

```
1...10 // 1-10
1..<10 // 1-9
```
