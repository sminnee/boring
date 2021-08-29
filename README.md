A Boring Language
=================

**Boring** is be an unsurprising language.

## Example

```
/*
 * Calculate the factorial of an integer
 */
let factorial = (x: i32): i32 =>
  if x <= 0 {
    1
  } else {
    x => x * factorial(x-1)
  }


// log() will pretty-print any number of input values
log("factorial(10) = ", factorial(10))
```

## Language structurs

### Assignment

`let` assigns values to immutable variables. Literal will be one of 4 types by default.

```
let x = 32 // i32
let y = 5.0 // f32
let z = "Hello" // string
let w = true // bool
```

### Operators

The usual range of arithemtic, comparions, and boolean operators are available.

```
let x = 5 + 3 * 4 ^ 2 // x = 53
let y = x > 50 & x < 60  // y = true
let z = x <= 50 | x >= 60  // z = false
let v = x = 53 = true
let w = !z / w = true
```

### Control structures

if ... else requires braces and returns an expression. Else is always required.

```
let size = if x > 50 {
  "Big"
} else {
  "Little"
}
```

### Functions

Functions are always expressed as lambdas, and can be assigned to a name with let.

```
let add = (a, b) => a + b

let y = add(5, 3) // y = 8

let thrice = (fn, a, b, c, d) => fn(fn(a, b), fn(c, d))

let z = thrice(add, 1, 2, 3, 4) // z = 10

let w = thrice((a, b) => a * b, 1, 2, 3, 4) // w = 24
```

Functions can be typed, otherwise type is inferred.

```
let add = (a, b) => a + b // type ('a,'a) => 'a

let add2 = (a: i32, b: i32) => a + b // type (i32, i32) => i32

let add3 = (a: i32, b: i32): i32 => a + b // type (i32, i32) => i32
```

### Records

Records are defined with the `type` keyword, as a strictly typed list of parameters.

```
type vec3 = {
  x: f32,
  y: f32,
  z: f32,
}

let myVec = { x: 1.0, y: 2.0, z: 3.0 }

let dot = (a: vec3, b: vec3) => a.x * b.x + a.y * b.y + a.z * b.z

let x = dot(myVec, myVec) // x = 14
```

## Built-in functions

`log(a: 'a, a: 'b, c: 'c, ...) => void`: Pretty-prints any number of arguments, one after the other.

`print(a: string) => void`: Print a string.