# ==============================================================================
# FUNCTIONS IN R  -  follow on from the For Loops tutorial
# ==============================================================================

# So far we have cleaned and manipulated data, made plots, and written loops.
# You may have noticed we keep copy-pasting the same chunks of code and just
# swapping out a value here or there. Every time you copy-paste code you risk:
#   1) Making a typos
#   2) Fixing a bug in one place but forgetting others
#   3) A script that is 400 lines long and difficult to read

# A function is a reusable chunk of code that you write:
# It requires input and output, then you can call it whenever you need it - a bit like how you
# already use mean(), ggplot() or filter(). But you can write your own! :)

# Read in data (same file as last time):
in_dir <- "C:/Users/mjohnso5/Downloads"
other_causes <- read.csv(file = file.path(in_dir, "other_causes.csv"), stringsAsFactors = T)
str(other_causes)

library(dplyr)
library(ggplot2)
library(ggpubr)

rm(square_all)

source(file = "C:/Users/mjohnso5/Downloads/functions_test.R")


# Basic Structure --------------------------------------------------------------

# name      <- function(arguments) {
#   body    (do something with the arguments)
#   return  (the thing you want back)
# }

double_number <- function(x) {
  result <- x * 2
  return(result)
}

double_number(5)

# In plain English:
#   name       = what you'll type to call it later
#   arguments  = the inputs the function needs to do its job (the blanks to fill)
#   body       = the code that does the work
#   return     = what the function hands back to you


# Examples ---------------------------------------------------------------------

# The simplest possible function - no arguments
say_hi <- function() {
  print("Hello stats class")
}

# Nothing happens when you DEFINE a function. You have to CALL it (with the ()):

say_hi()


# A function WITH an argument. 'x' is a placeholder - it becomes whatever you
# put inside the brackets when you call it.
double_it <- function(x) {
  x * 2
}

double_it(10)   # x becomes 10
double_it(5)    # x becomes 5
double_it(other_causes$enteric_deaths_num)  # x can be a whole vector too!


# Note: if you don't write return(), R hands back the LAST thing evaluated.
# These two functions are identical:

double_it <- function(x) {
  x * 2
}

double_it <- function(x) {
  return(x * 2)
}

# Being explicit with return() is a good habit when functions get longer, so you
# (and your reader) can see exactly what comes back out.


# More than one argument -------------------------------------------------------

# Remember calculating percentages during data cleaning? Let's package it.
# part = the count, whole = the total. Order matters when you call it.

percentage <- function(part, whole) {
  (part / whole) * 100
}

percentage(part = 79342, whole = 300000)

# You can name the arguments (part = , whole = ) or rely on position.
# These give the same answer:
percentage(79342, 300000)          # by position
percentage(whole = 300000, part = 79342)  # by name (order no longer matters)

# TIP: name your arguments when you call a function you don't use often.
# 'Future you' reading the script in 6 months will thank you.


# Default argument values ------------------------------------------------------

# You can give an argument a default so it's optional. Here we default to
# rounding to 1 decimal place, but the user can override it.

?mean

percentage <- function(part, whole, digits = 1) {
  round((part / whole) * 100, digits = digits)
}

percentage(79342, 300000)          # uses default digits = 1
percentage(79342, 300000, digits = 3)  # override the default


# Q) Write a function called r_squared() that takes a Pearson R value and
#    returns R^2. Then test it on 0.8.

r_squared <- function(x){
  return(x ^ 2)
}

r_squared(3)


# Tip: it's one line in the body. Think: what's the argument, what comes back.




# Scope: what happens inside a function, stays inside a function ----------------

# Variables you create INSIDE a function are local - they don't leak out into
# your Environment. It stops functions messing with your data.

demo_scope <- function() {
  secret <- 42   # created inside the function
  secret
}

demo_scope()   # returns 42
# secret       # <- uncomment this: ERROR, 'secret' not found. It never escaped.

# The flip side: a function can SEE variables from
# your global Environment, so a function can appear to "work" while secretly
# depending on something outside it. That makes it fragile.

# BAD - relies on 'other_causes' existing in the global environment:
bad_mean <- function() {
  mean(other_causes$malaria_number)   # where did other_causes come from?!
}

# GOOD - everything it needs is passed IN as an argument. Self-contained.
good_mean <- function(data, column) {
  mean(data[[column]])
}

good_mean(other_causes, "malaria_number")

# Rule of thumb: if a function needs something, pass it in as an argument.
# Don't make the function reach outside itself to grab it. This is lazy coding aand will come back to bite you. 


# Returning more than one value ------------------------------------------------

# A function returns ONE object - but that object can be a named vector or a
# list, so you can bundle several results together.

summarise_column <- function(data, column) {
  x <- data[[column]]
  c(mean   = mean(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE))
}

summarise_column(other_causes, "malaria_number")
summarise_column(other_causes, "enteric_deaths_num")

# Because it's a function, running it on a new column is now trivial - no
# copy-pasting four lines of mean/sd/min/max each time.


# Functions + Loops together ---------------------------------------------------

# Let's wrap the previous R2 loop it in a function.


output_R2 <- numeric(length(R_values))
for (i in seq_along(R_values)) {
output_R2[i] <- R_values[i] ^ 2
}

# The SAME logic, now reusable:
square_all <- function(x) {
  out <- numeric(length(x))          # empty output, same length as input
  for (i in seq_along(x)) {          # loop over every element
    out[i] <- x[i] ^ 2               # save result in matching slot
  }
  return(out)
}

R_values <- c(0.8, 0.2, 0.53, 0.64, 0.5)


square_all(R_values)

# It works on ANY numeric vector now, not just R_values:
square_all(1:5)


# The apply family: loops without writing the loop -----------------------------

# R has built-in functions that do the "loop over a vector and collect the
# results" pattern for you. They're often cleaner than a hand-written loop.

# sapply() = "simplify apply". It loops over each element, applies a function,
# and returns a vector. This does the exact same job as square_all():
sapply(R_values, function(r) r ^ 2)

# lapply() = "list apply". Same idea but always returns a LIST (handy when each
# result is bigger than a single number):
lapply(R_values, function(r) r ^ 2)

# For simple maths R is even lazier - it's already vectorised, no loop needed:
R_values ^ 2

# So you now have THREE ways to square a vector:
#   1) a for loop          (most explicit - good for learning / complex steps)
#   2) sapply / lapply     (compact - good for one clear operation per element)
#   3) vectorised x^2      (fastest - when the operation already works on vectors)
# Pick whichever is clearest for the problem in front of you.


# A plotting function ----------------------------------------------------------

# Let's turn one plot into a function, so we can call it on demand for ANY column, with sensible defaults.

plot_vs_enteric <- function(data, yvar, colour_var = "year") {
  ggplot(data, aes(x = enteric_deaths_num,
                   y = .data[[yvar]],           # .data[[ ]] lets us pass a name
                   color = .data[[colour_var]])) +
    geom_point() +
    stat_cor(method = "pearson") +
    theme_bw() +
    geom_smooth(method = lm, alpha = 0.25, color = "black", fill = "darkgreen")
}

# Now one clean line makes a fully formatted plot:
plot_vs_enteric(other_causes, "malaria_number")


plot_vs_enteric(other_causes, "typhoid_and_paratyphoid_number")

# Override the default colour:
plot_vs_enteric(other_causes, "malaria_number", colour_var = "location")


# Best of both worlds: the function INSIDE a loop --------------------------------

# Now the loop body is a single readable line, because the messy plot code lives
# in the function. This is how real analysis scripts are structured.

cols     <- other_causes %>% select(contains("num"))
col_names <- colnames(cols)

for (i in col_names) {
  print(plot_vs_enteric(other_causes, i))
  Sys.sleep(2)
}

?


# ------------------------------------------------------------------------------
# TASK
# ------------------------------------------------------------------------------
# 1) Write a function count_deaths() that takes 'data' and a 'column' name and
#    returns the TOTAL (sum) of that column. Remember to handle NAs.
#
# 2) Using the col_names vector of "..._num" columns, loop over each one and
#    print the total for each column using your function.
#
# Tips: pass everything the function needs as arguments (no reaching outside!).
#       Think back to good_mean() for the data[[column]] pattern.
# ------------------------------------------------------------------------------


# ==============================================================================
# ANSWERS
# ==============================================================================

# Q) r_squared function
r_squared <- function(R) {
  R ^ 2
}
r_squared(0.8)


# TASK 1) count_deaths function
count_deaths <- function(data, column) {
  sum(data[[column]], na.rm = TRUE)
}
count_deaths(other_causes, "malaria_number")


# TASK 2) loop it over every "..._num" column
for (i in col_names) {
  total <- count_deaths(other_causes, i)
  print(paste0(i, ": ", total))
}

# (Bonus) the same thing without a loop, using sapply:
sapply(col_names, function(i) count_deaths(other_causes, i))