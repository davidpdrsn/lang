# lang

This is a small programming language I'm working on in my spare time. Currently not much works.

## Features

- Interpreted, might compile to JS
- Imperative
- Static typing
- Local type inference
- Product types (structs)
- Sum types (enums)

## Syntax

```
fn main() {
    fizz_buzz(1);
}

fn fizz_buzz(n: Integer) {
    if n <= 100 {
        if n % 15 == 0 {
            println("Fizz Buzz");
        } else {
            if n % 3 == 0 {
                println("Fizz");
            } else {
                if n % 5 == 0 {
                    println("Buzz");
                } else {
                    println(n);
                }
            }
        }

        fizz_buzz(n + 1);
    }
}
```
