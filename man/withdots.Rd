% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/withdots.R
\name{withdots}
\alias{withdots}
\title{Give a \link{function} \code{\link[=dots]{...}} if it does not have it}
\usage{
withdots(f)
}
\arguments{
\item{f}{A \link{function}. See \strong{Handling of \link{primitive}s} in case \code{f} is
\link{primitive}.}
}
\value{
If \code{f} has \code{\link[=dots]{...}} in its \code{\link{args}}, then \code{f}.

Otherwise, a \link{closure}: a tweaked version of \code{f}, whose only differences
are:
\enumerate{
\item \code{\link[=dots]{...}} has been appended to the end of its \code{\link{formals}}, and
\item any \code{\link{srcref}} \code{\link[=attr]{attribute}} has been removed (see \strong{Why the
\code{\link{srcref}} \code{\link[=attr]{attribute}} is removed} below).
}
}
\description{
Adds \code{\link[=dots]{...}} to a \link{closure}'s \code{\link{args}} if it does not have it already.
}
\details{
If \code{f} already has \code{\link[=dots]{...}} in its \code{\link{args}}, then it is returned with no
changes. Otherwise, \code{\link[=dots]{...}} is added to \code{f}'s \link{formals} and then \code{f} is
returned. See \strong{Handling of \link{primitive}s} below.
}
\section{How \code{\link[=dots]{...}} is added to \link{closure}s}{
 These are the steps that
\code{withdots()} takes \strong{only} if \code{f} is a \link{closure} without \code{\link[=dots]{...}} in
its \code{\link{formals}}:
\enumerate{
\item \code{\link{attributes}}\code{(f)} are temporarily saved and set aside.
\item If there is a \code{\link{srcref}} \code{\link[=attr]{attribute}} among the set-aside
\code{\link{attributes}}\code{(f)}, it is removed (see \strong{Why the \code{\link{srcref}}
\code{\link[=attr]{attribute}} is removed} below).
\item \code{\link[=dots]{...}} is added to the \code{\link{formals}} of \code{f} using \code{\link{formals<-}}.
\item The remaining set-aside \code{\link{attributes}} are added back to \code{f} with
\code{\link{attributes<-}}.
\item \code{f} is returned.
}
}

\section{Handling of \link{primitive}s}{
 If \code{f} is \link{primitive} and already has
\code{\link[=dots]{...}} in its \code{\link{args}} (e.g., \code{\link[=c]{c()}}, \code{\link[=rep]{rep()}}, \code{\link[=max]{max()}}), then it is
returned as is.

If \code{f} is \link{primitive} and does \strong{not} have \code{\link[=dots]{...}} in its \code{\link{args}},
then an error will be thrown. The user can bypass this error by processing
\code{f} with \code{\link[rlang:as_closure]{rlang::as_closure()}} before passing it to \code{withdots()}.
\strong{However, keep in mind that the argument matching behavior of the
resulting \link{closure} may be different from what is expected, since
\link{primitive}s may use nonstandard argument matching.}
}

\section{Why the \code{\link{srcref}} \code{\link[=attr]{attribute}} is removed}{
 Many
\link{function}s---including those created with \code{\link[=function]{function()}}---have a \code{\link{srcref}}
\code{\link[=attr]{attribute}}. When a \link{function} is \link{print}ed, \code{\link[=print.function]{print.function()}}
relies on this \code{\link[=attr]{attribute}} by default to depict the \link{function}'s
\link{formals} and \link{body}.

\code{withdots()} adds \code{\link[=dots]{...}} via \code{\link{formals<-}}, which expressly drops
\code{\link{attributes}} (see its \link[=formals<-]{documentation page}). To prevent this
loss, \code{withdots()} sets \code{\link{attributes}}\code{(f)} aside at the beginning and
re-attaches them to \code{f} at the end. Normally, this would re-attach the
original \code{f}'s \code{\link{srcref}} \code{\link[=attr]{attribute}} to the new \code{f}, making it so
that the newly added \code{\link[=dots]{...}} would not be depicted when the new \code{f}
is \link{print}ed. For this reason, the old \code{\link{srcref}} \code{\link[=attr]{attribute}} is
dropped, and only the remaining \code{\link{attributes}} are re-attached to the new
\code{f}.

Observe what would happen during \link{print}ing if \strong{all} original
\code{\link{attributes}}\code{(f)} were naively added to the modified \code{f}:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{# Create a function with no dots:
foo <- function(a = 1) \{
  # Helpful comment
  a
\}

# Give it important attributes that we can't afford to lose:
attr(foo, "important_attribute") <- "crucial information"
class(foo) <- "very_special_function"

# Print foo, which also prints its important attributes:
foo
#> function(a = 1) \{
#>   # Helpful comment
#>   a
#> \}
#> <environment: 0x571c620>
#> attr(,"important_attribute")
#> [1] "crucial information"
#> attr(,"class")
#> [1] "very_special_function"

# Save its attributes:
old_attributes <- attributes(foo)

# Add dots:
formals(foo)[["..."]] <- quote(expr = )

# See that the important attributes have been dropped:
foo
#> function (a = 1, ...) 
#> \{
#>     a
#> \}
#> <environment: 0x571c620>

# Add the attributes back:
attributes(foo) <- old_attributes

# Print it again, and we see that the attributes have returned.
# However, the ... disappears from the argument list.
foo
#> function(a = 1) \{
#>   # Helpful comment
#>   a
#> \}
#> <environment: 0x571c620>
#> attr(,"important_attribute")
#> [1] "crucial information"
#> attr(,"class")
#> [1] "very_special_function"

# We know the actual function definitely has dots, since it can handle
# extraneous arguments:
foo(1, 2, junk, "arguments", NULL)
#> [1] 1

# Remove the "srcref" attribute, and the function is printed accurately.
# Furthermore, its important attributes are intact:
attr(foo, "srcref") <- NULL
foo
#> function (a = 1, ...) 
#> \{
#>     a
#> \}
#> <environment: 0x571c620>
#> attr(,"important_attribute")
#> [1] "crucial information"
#> attr(,"class")
#> [1] "very_special_function"

# Success (although the comments in the body() of the function are lost)
}\if{html}{\out{</div>}}
}

\examples{
# The base::match() function has no ... and can't handle extraneous arguments
if (FALSE) {
  match("z", letters, cannot_handle_ = "junk arguments")
}

# But if we give it dots...
match_with_dots <- withdots(match)

# ...it can now handle extraneous arguments:
match_with_dots("z", letters, can_now_handle = "junk arguments")
}
