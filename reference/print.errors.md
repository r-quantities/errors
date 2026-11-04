# Print Values

S3 method for `errors` objects.

## Usage

``` r
# S3 method for class 'errors'
print(x, ...)
```

## Arguments

- x:

  an `errors` object.

- ...:

  further arguments passed to or from other methods.

## Examples

``` r
x <- set_errors(1:10, 1:10 * 0.05)
print(x)
#> Errors: 0.05 0.10 0.15 0.20 0.25 ...
#>  [1]  1  2  3  4  5  6  7  8  9 10
print(x[1:3])
#> Errors: 0.05 0.10 0.15
#> [1] 1 2 3
print(x[1])
#> 1.00(5)
print(x[1], digits=2)
#> 1.000(50)
print(x[1], notation="plus-minus")
#> 1.00 ± 0.05
```
