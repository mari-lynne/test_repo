# The SAME logic, now reusable:
square_all <- function(x) {
  out <- numeric(length(x))          # empty output, same length as input
  for (i in seq_along(x)) {          # loop over every element
    out[i] <- x[i] ^ 2               # save result in matching slot
  }
  return(out)
}