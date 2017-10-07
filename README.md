# Esca

Esca is an experimental strongly-typed language which compiles to Go.

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

#### Anonymous Functions

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
