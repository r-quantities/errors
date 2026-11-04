# Scatterplot with Error Bars

S3 method for `errors` objects which automatically prints the error
bars.

## Usage

``` r
# S3 method for class 'errors'
plot(x, y, ...)
```

## Arguments

- x, y:

  the `x` and `y` arguments provide the x and y coordinates for the
  plot. Any reasonable way of defining the coordinates is acceptable.
  See the function
  [`xy.coords`](https://rdrr.io/r/grDevices/xy.coords.html) for details.
  If supplied separately, they must be of the same length.

- ...:

  additional arguments (see
  [`plot`](https://rdrr.io/r/graphics/plot.default.html)).

## Examples

``` r
cars <- as.matrix(cars)
cars <- as.data.frame(set_errors(cars, cars * 0.05))
plot(cars$speed)

plot(cars)

```
