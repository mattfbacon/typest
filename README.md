# "Typest"

A POC static type checker for Typst

## Example

```typst
// @type int
#let a = 3
// @type string
#let b = 4

// @type int
#let c(
	// @type int
	x
) = x + 2
#let d(
	// @type string
	x
) = c(x)
```

```
problem:
  span: 4:9 (50..51) "4"
  message: type mismatch. expected String, got Int
problem:
  span: 14:6 (135..136) "x"
  message: invalid type. expected Int, got String
```

## License

AGPL-3.0-or-later
