
#' Give a [function] [`...`][dots] if it does not have it
#'
#' Adds [`...`][dots] to a [closure]'s [`args`] if it does not have it already.
#'
#' If `f` already has [`...`][dots] in its [`args`], then it is returned with no
#' changes. Otherwise, [`...`][dots] is added to `f`'s [formals] and then `f` is
#' returned. See **Handling of [primitive]s** below.
#'
#' @section How [`...`][dots] is added to [closure]s: These are the steps that
#'   `withdots()` takes **only** if `f` is a [closure] without [`...`][dots] in
#'   its [`formals`]:
#'
#'   1. [`attributes`]`(f)` are temporarily saved and set aside.
#'
#'   1. If there is a [`srcref`] [`attribute`][attr] among the set-aside
#'   [`attributes`]`(f)`, it is removed (see **Why the [`srcref`]
#'   [`attribute`][attr] is removed** below).
#'
#'   1. [`...`][dots] is added to the [`formals`] of `f` using [`formals<-`].
#'
#'   1. The remaining set-aside [`attributes`] are added back to `f` with
#'   [`attributes<-`].
#'
#'   1. `f` is returned.
#'
#' @section Handling of [primitive]s: If `f` is [primitive] and already has
#'   [`...`][dots] in its [`args`] (e.g., [c()], [rep()], [max()]), then it is
#'   returned as is.
#'
#'   If `f` is [primitive] and does **not** have [`...`][dots] in its [`args`],
#'   then an error will be thrown. The user can bypass this error by processing
#'   `f` with [rlang::as_closure()] before passing it to `withdots()`.
#'   **However, keep in mind that the argument matching behavior of the
#'   resulting [closure] may be different from what is expected, since
#'   [primitive]s may use nonstandard argument matching.**
#'
#' @section Why the [`srcref`] [`attribute`][attr] is removed: Many
#'   [function]s---including those created with [function()]---have a [`srcref`]
#'   [`attribute`][attr]. When a [function] is [print]ed, [print.function()]
#'   relies on this [`attribute`][attr] by default to depict the [function]'s
#'   [formals] and [body].
#'
#'   `withdots()` adds [`...`][dots] via [`formals<-`], which expressly drops
#'   [`attributes`] (see its [documentation page][formals<-]). To prevent this
#'   loss, `withdots()` sets [`attributes`]`(f)` aside at the beginning and
#'   re-attaches them to `f` at the end. Normally, this would re-attach the
#'   original `f`'s [`srcref`] [`attribute`][attr] to the new `f`, making it so
#'   that the newly added [`...`][dots] would not be depicted when the new `f`
#'   is [print]ed. For this reason, the old [`srcref`] [`attribute`][attr] is
#'   dropped, and only the remaining [`attributes`] are re-attached to the new
#'   `f`.
#'
#'   Observe what would happen during [print]ing if **all** original
#'   [`attributes`]`(f)` were naively added to the modified `f`:
#'
#'   ```{r naive_withdots}
#'   # Create a function with no dots:
#'   foo <- function(a = 1) {
#'     # Helpful comment
#'     a
#'   }
#'
#'   # Give it important attributes that we can't afford to lose:
#'   attr(foo, "important_attribute") <- "crucial information"
#'   class(foo) <- "very_special_function"
#'
#'   # Print foo, which also prints its important attributes:
#'   foo
#'
#'   # Save its attributes:
#'   old_attributes <- attributes(foo)
#'
#'   # Add dots:
#'   formals(foo)[["..."]] <- quote(expr = )
#'
#'   # See that the important attributes have been dropped:
#'   foo
#'
#'   # Add the attributes back:
#'   attributes(foo) <- old_attributes
#'
#'   # Print it again, and we see that the attributes have returned.
#'   # However, the ... disappears from the argument list.
#'   foo
#'
#'   # We know the actual function definitely has dots, since it can handle
#'   # extraneous arguments:
#'   foo(1, 2, junk, "arguments", NULL)
#'
#'   # Remove the "srcref" attribute, and the function is printed accurately.
#'   # Furthermore, its important attributes are intact:
#'   attr(foo, "srcref") <- NULL
#'   foo
#'
#'   # Success (although the comments in the body() of the function are lost)
#'   ```
#'
#' @param f A [function]. See **Handling of [primitive]s** in case `f` is
#'   [primitive].
#'
#' @return If `f` has [`...`][dots] in its [`args`], then `f`.
#'
#'   Otherwise, a [closure]: a tweaked version of `f`, whose only differences
#'   are:
#'
#'   1. [`...`][dots] has been appended to the end of its [`formals`], and
#'
#'   1. any [`srcref`] [`attribute`][attr] has been removed (see **Why the
#'   [`srcref`] [`attribute`][attr] is removed** below).
#'
#' @examples
#' # The base::match() function has no ... and can't handle extraneous arguments
#' if (FALSE) {
#'   match("z", letters, cannot_handle_ = "junk arguments")
#' }
#'
#' # But if we give it dots...
#' match_with_dots <- withdots(match)
#'
#' # ...it can now handle extraneous arguments:
#' match_with_dots("z", letters, can_now_handle = "junk arguments")
#' @export
withdots <- function(f) {
  if (!is.function(f)) {
    stop("f must be a function.",
         "\nConsider passing to rlang::as_function() first.",
         call. = FALSE)
  }

  if (is.primitive(f)) {
    args_fn <- args(f)
    if (!is.function(args_fn) || !any(names(formals(args_fn)) == "...")) {
      stop("f must be a closure (non-primitive) or a primitive with a",
           "\nwell-defined argument list that already contains ...",
           "\nConsider passing f to rlang::as_closure() first.",
           call. = FALSE)
    }
    return(f)
  }

  if (any(names(formals(f)) == "...")) {
    return(f)
  }

  a <- attributes(f)
  a[["srcref"]] <- NULL

  formals(f)[["..."]] <- quote(expr = )

  attributes(f) <- a

  f
}
