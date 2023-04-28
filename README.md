
<!-- README.md is generated from README.Rmd. Please edit that file -->

# withdots

<!-- badges: start -->
<!-- badges: end -->

The `withdots()` function adds `...` to the argument list of a function
if it does not already have it. This lets the function tolerate
extraneous arguments that are passed to it.

## Installation

You can install the development version of withdots like so:

``` r
remotes::install_github("NikKrieger/withdots")
```

## Demo

The base R function `match()` has no `...` in its argument list:

``` r
match
#> function (x, table, nomatch = NA_integer_, incomparables = NULL) 
#> .Internal(match(x, table, nomatch, incomparables))
#> <bytecode: 0x2646880>
#> <environment: namespace:base>
```

Therefore, it can’t handle extraneous arguments:

``` r
match("z", letters, bad_arg = "error!")
#> Error in match("z", letters, bad_arg = "error!"): unused argument (bad_arg = "error!")
```

But if we give it dots, then it can:

``` r
library(withdots)

match_with_dots <- withdots(match)

match_with_dots("z", letters, can_now_handle = "junk arguments")
#> [1] 26
```

Functions that already have `...` in their argument list are returned as
is:

``` r
identical(lapply, withdots(lapply))
#> [1] TRUE
identical(c, withdots(c))
#> [1] TRUE
```

## Notes

### A note about primitive functions

If a function is a primitive function (see `?primitive`) with a
well-defined argument list containing `...` (e.g., `c()`, `sum()`,
`as.character()`), then `withdots()` will return it as is. The test for
a “well-defined argument list,” given a function `fn`, is
`is.function(args(fn))`.

#### primitive functions with well-defined argument lists not containing `...`

If the primitive has a well-defined argument list that does not contain
`...` (e.g., `round()`, `is.na()`, `sqrt()`), then `withdots()` throws
an error. To bypass this, all of these functions can be pre-processed
with `rlang::as_closure()`, whose result can be then passed to
`withdots()`. Observe:

``` r
# Observe that round() is a primitive function with no ... in its arg list:
round
#> function (x, digits = 0)  .Primitive("round")
```

``` r
# So, we can't pass it to withdots() as is:
withdots(round)
#> Error: f must be a closure (non-primitive) or a primitive with a
#> well-defined argument list that already contains ...
#> Consider passing f to rlang::as_closure() first.
```

``` r
# But if we turn it into a closure, we can give it dots:
library(rlang)

round <- withdots(as_closure(round))

round
#> function (x, digits = 0, ...) 
#> .Primitive("round")(x, digits)
```

``` r
# And now it can handle extraneous arguments:
round(45.78, digits = 1, junk, arguments)
#> [1] 45.8
```

**However**, keep in mind that the argument matching behavior of the
result ***may*** be different from what is expected, since primitives
***may*** use nonstandard argument matching.

#### Primitive functions without well-defined argument lists

If the primitive function does not have a well-defined argument list
(e.g., `[`, `~`, `function`, `for`), then `withdots()` throws an error.
**Some** of these functions can be pre-processed with
`rlang::as_closure()`, whose result definitely can be passed to
`withdots()`. They are:

``` r
all_base <- getNamespaceExports("base")
all_base <- setNames(nm = all_base)
all_base <- lapply(all_base, function(x) getExportedValue("base", x))
all_primitives <- Filter(is.primitive, all_base)

primitive_non_well_defined_args <-
  Filter(function(fn) !is.function(args(fn)), all_primitives)

as_closure_coercible <-
  Filter(
    function(fn) tryCatch({as_closure(fn); TRUE}, error = function(e) FALSE),
    primitive_non_well_defined_args
  )

names(as_closure_coercible)
#>  [1] "$"    "("    ":"    "="    "@"    "["    "{"    "~"    "&&"   "<-"  
#> [11] "[["   "||"   "[[<-" "$<-"  "<<-"  "@<-"  "[<-"
```

However, there are a handful of primitives that `rlang::as_closure()` is
unwilling to process and are therefore ineligible for `withdots()`. They
are:

``` r
as_closure_noncoercible <-
  Filter(
    function(fn) tryCatch({as_closure(fn); FALSE}, error = function(e) TRUE),
    primitive_non_well_defined_args
  )
  
names(as_closure_noncoercible)
#> [1] "if"       "function" "repeat"   "for"      "break"    "return"   "next"    
#> [8] "while"
```

### The `srcref` `attribute`.

Many functions—including those created with `function()`—have a `srcref`
`attribute`. When a function is `print`ed, `print.function()` relies on
this `attribute` by default to depict the function’s `formals` and
`body`.

`withdots()` adds `...` via `formals<-`, which expressly drops
`attributes` (see `` ?`formals<-` ``). To prevent this loss,
`withdots()` sets the function’s `attributes` aside at the beginning and
re-attaches them at the end. Normally, this would re-attach the original
function’s `srcref` `attribute` to the new function, making it so that
the newly added `...` would not be depicted when the new function is
`print`ed. For this reason, the old `srcref` `attribute` is dropped, and
only the remaining `attributes` are re-attached to the new function.

Observe what would happen during `print`ing if **all** original
`attributes` were naively added to the modified function:

``` r
# Create a function with no dots:
foo <- function(a = 1) {
  # Helpful comment
  a
}

# Give it important attributes that we can't afford to lose:
attr(foo, "important_attribute") <- "crucial information"
class(foo) <- "very_special_function"

# Print foo, which also prints its important attributes:
foo
#> function(a = 1) {
#>   # Helpful comment
#>   a
#> }
#> attr(,"important_attribute")
#> [1] "crucial information"
#> attr(,"class")
#> [1] "very_special_function"
```

``` r
# Save its attributes:
old_attributes <- attributes(foo)

# Add dots:
formals(foo)[["..."]] <- quote(expr = )

# See that the important attributes have been dropped:
foo
#> function (a = 1, ...) 
#> {
#>     a
#> }
```

``` r
# Add the attributes back:
attributes(foo) <- old_attributes

# Print it again, and we see that the attributes have returned.
# However, the ... disappears from the argument list.
foo
#> function(a = 1) {
#>   # Helpful comment
#>   a
#> }
#> attr(,"important_attribute")
#> [1] "crucial information"
#> attr(,"class")
#> [1] "very_special_function"
```

``` r
# We know the actual function definitely has dots, since it can handle
# extraneous arguments:
foo(1, 2, junk, "arguments", NULL)
#> [1] 1
```

``` r
# Remove the "srcref" attribute, and the function is printed accurately.
# Furthermore, its important attributes are intact:
attr(foo, "srcref") <- NULL
foo
#> function (a = 1, ...) 
#> {
#>     a
#> }
#> attr(,"important_attribute")
#> [1] "crucial information"
#> attr(,"class")
#> [1] "very_special_function"

# Success! However, the comments in the body of the function are lost.
# Even so, this is better than inaccurate `print`ing.
```
