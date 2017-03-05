# helper function from Josh O'Brien from here:
# http://stackoverflow.com/questions/26539441/r-remove-null-elements-from-list-of-lists

# removes NULL from deeply nested lists.
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

## Recursively step down into list, removing all such objects
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}


# Emulate '+' python function
`%p%` <- function(e1,e2) return(paste0(e1,e2))
