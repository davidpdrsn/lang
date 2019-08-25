# lang

This is a small programming language I'm working on in my spare time. Currently not much works.

## Example

Features:

- Interpreted, might compile to JS
- Imperative
- Static typing
- Local type inference
- Product types (structs)
- Sum types (enums)

Syntax:

```
fn main() {
    let a = 1;
    add_one_and_print(a);
    println(a);
}

fn add_one_and_print(a: Integer) {
    a = a + 1;
    println(a);
}
```
