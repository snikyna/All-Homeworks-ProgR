
# This exercise is concerned with vectors and basic operations on them.


# Suppose you have a vector "numbers" of ages of people, and you want to know how many of them are "Youth" (younger
# than 18 years), "Young Adult" (at least 18, less than 36), "Adult" (at least 36, less than 56) or "Senior" (56 and
# up). Or another problem: you have a vector of people's BMI and you want to know how many are "Severely underweight"
# (BMI < 16), "Underweight" (BMI between 16 and 18.5), "Healthy" (18.5 to 25) or "Overweight" (BMI > 25).
#
# For these problems, you get as input a vector of numbers, a vector of bin cutoffs, and a vector of bin names.
# The bin cutoffs indicate the upper bounds of each bin (the upper bound of the last bin is `Inf` (infinity) and not
# provided). Your task is to count the numbers in each bin and return them in a *named* vector, in order of increasing
# cutoff value.
#
# The inputs for the first problem described above could be
# > numbers = c(10.3, 32.7, 50.5, 62.4, 32.0, 50.4, 19.7, 60.1, 69.0, 50.6, 11.1, 48.6, 17.4, 34.3, 78.7)
# > cutoffs = c(18, 36, 56)
# > binnames = c("Youth", "Young Adult", "Adult", "Senior")
# and the expected result would be
# > c(Youth = 3, "Young Adult" = 4, Adult = 4, Senior = 4)
# The second problem could have input
# > numbers = c(20, 23, 28)
# > cutoffs = c(16, 18.5, 25)
# > binnames = c("Severely underweight", "Underweight", "Healthy", "Overweight")
# and the result should be
# > c("Severely underweight" = 0, Underweight = 0, Healthy = 2, Overweight = 1)
#
# You may assume that the cutoff values are already sorted.
# Cutoff bounds are exclusive, so a data point of 18.0 in the first example is counted as "Young Adult".
#
# There are different ways to solve this; among others you could solve this task by itself, you could also solve the
# following exercise (ex02Binning) first and call it here using the `table()` method, or you could
# try if the `hist()` function is useful. (If you use `table()`, you will have to wrap the result in `c()` to get
# a named vector from the table: `c(table(...))`).
ex01BinCounting <- function(numbers, cutoffs, binnames) {
  bincount <- numeric(length(binnames))
  for (i in seq_len(length(cutoffs))) {
    if (i == 1) {
      bincount[i] <- sum(numbers < cutoffs[i])
    }
    else {
      bincount[i] <- sum(numbers >= cutoffs[i - 1] & numbers < cutoffs[i])
    }
  }
  bincount[length(binnames)] <- sum(numbers >= cutoffs[length(cutoffs)])
  x <- setNames(bincount, binnames)
  x
}

# Related to the last exercise: Now you should not only *count* the number of data points in each bin, you
# should return, for each data point, the bin that it belongs to, as an *ordered factor* variable. This is called data
# binning. For the first input, this would be
# > numbers = c(10.3, 32.7, 50.5, 62.4, 32.0, 50.4, 19.7, 60.1, 69.0, 50.6, 11.1, 48.6, 17.4, 34.3, 78.7)
# > cutoffs = c(18, 36, 56)
# > binnames = c("Youth", "Young Adult", "Adult", "Senior")
# return:
# > ordered(c("Youth", "Young Adult", "Adult", "Senior", "Young Adult", "Adult",  "Young Adult", "Senior",
#             "Senior", "Adult", "Youth", "Adult",  "Youth", "Young Adult", "Senior"),
#           levels = c("Youth", "Young Adult", "Adult", "Senior"))
# The second problem would be
# > numbers = c(20, 23, 28)
# > cutoffs = c(16, 18.5, 25)
# > binnames = c("Severely underweight", "Underweight", "Healthy", "Overweight")
# and the result should be
# > ordered(c("Healthy", "Healthy", "Overweight"),
#   levels = c("Severely underweight", "Underweight", "Healthy", "Overweight"))
#
# The `cut()` function could be helpful here.
ex02Binning <- function(numbers, cutoffs, binnames) {
  x <- cut(numbers,
           breaks = c(-Inf, cutoffs, Inf),
           labels = binnames,
           level = factor(binnames),
           ordered = TRUE)
  x
}

# "Fizz Buzz" is a game for children where players take turns saying numbers
# counting up, but say "Fizz" instead if a number is divisible by 3 and "Buzz"
# if a number is divisible by 5, saying "Fizz Buzz" if both is the case.
# Read about it at Wikipdia: <https://en.wikipedia.org/wiki/Fizz_buzz>.
#
# Write a function that plays this game with itself. The function should
# return a `character` vector containing the numbers from 1 up to `up.to`, or
# "Fizz", "Buzz" or "Fizz Buzz" at the appropriate places. The numbers at which
# "Fizz" and "Buzz" should be returned are optional arguments.
#
# Example:
# up.to = 10, fizz.number = 3, buzz.number = 5
# --> returns c("1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz")
# up.to = 6, fizz.number = 2, buzz.number = 4
# --> returns c("1", "Fizz", "3", "Fizz Buzz", "5", "Fizz")
#
# You can rely on fizz.number, buzz.number and up.to being integer values greater
# than 0, though not necessarily greater than 1 and not necessarily different from
# each other.
#
# Although you are allowed to use loops, it is probably simpler to solve this
# without loops.
ex03FizzBuzz <- function(up.to, fizz.number = 3, buzz.number = 5) {
  x <- 1:up.to
  x <- ifelse(x %% buzz.number == 0 & x %% fizz.number == 0,
              "Fizz Buzz",
              ifelse(x %% buzz.number == 0,
                     "Buzz", ifelse(x %% fizz.number == 0,
                                    "Fizz", x)))
  x
}

# This exercise is concerned with basic operations on matrices and data frames.
# We are using only methods contained in the standard R installation; don't use
# tidy here.

# Imagine a square matrix as a chess board, where the top left field is white, the
# second row left field as well as the first row, second column field are black, etc.
#
# Given a square matrix as input, return the sum of the values of all elements that
# would be on black fields:
#
# 1 2
# 3 4  --> 2 and 3 are on 'black' fields --> result 5
#
# 1 2 3
# 4 5 6 --> 2 + 4 + 6 + 8 --> 20
# 7 8 9
#
# 1 0 0 0
# 0 1 0 0
# 0 0 1 0 --> 0
# 0 0 0 1
#
# Make sure this works with the trivial matrix with only one element!
# The `row()` and `column()` function are useful here.
ex01ChessboardSum <- function(mat) {
  x <- col(mat) %% 2 == 0
  y <- row(mat) %% 2 == 0
  sum(mat[x != y])
}

# Given a matrix as input, return the coordinates (row and column) of the minimum element,
# as a two element numeric vector. If multiple elements are tied for minimum, you
# can return the coordinates of any of them.
#
# E.g.
#
# 1 2 3
# 4 5 6 --> c(1, 1)
# 7 8 9
#
#  0  0
#  0  0 --> c(3, 1)
# -3  0
#
# 1 0 --> both c(2, 1) and c(1, 2) would be correct
# 0 1
ex02MatrixWhichMin <- function(mat) {
  x <- as.vector(which(mat == min(mat), arr.ind = TRUE))
  if (length(x) > 2) {
    head(x, - 2)
  } else {
    x
  }
}

# Your function is given a `data.frame` with columns that can each be of any of numeric, logical
# or factor type. You are supposed to return a `data.frame` containing only columns selected
# by type. For this you are given the "type" argument: a character vector containing a single
# argument containing one of c("numeric", "logical", "character").
#
# E.g. * data = iris (the "iris" dataset as contained in R)
#        type = "numeric"
#        --> return
#                     Sepal.Length Sepal.Width Petal.Length Petal.Width
#                   1          5.1         3.5          1.4         0.2
#                   2          4.9         3.0          1.4         0.2
#                   3          4.7         3.2          1.3         0.2
#                   .........
#      * data = iris
#        type = "logical"
#        --> return a data frame with 0 columns and 150 rows
#      * data = data.frame(a = c(1, 2, 3), b = c("alpha", "beta", "gamma"))
#        type = "character"
#        --> return
#                        b
#                  1 alpha
#                  2  beta
#                  3 gamma
#    ... etc.
# The order of all included columns should not be changed with respect to their order in the input.
# Be aware that there are could be other columns that are not one of numeric, logical, or character,
# in particular "factor" columns; they should always be discarded.
ex03SelectDF <- function(data, type) {
  sel.col <- vapply(data, function(col) {
    any(type %in% class(col))
  }, TRUE)
  data[sel.col]
}


# You are given a `data.frame` with some data about study participants. Its columns
# are numeric, some of which have missing values (NA). Your task is to *impute* the missing values,
# i.e. set them to putative numeric values inferred from the other participants. In particular,
# you are to set the missing values in each variable to the *mean value* of all the *non-missing values*
# in that variable.
#
# E.g. for the input
# > data.frame(
#     height = c(178, 185, NA, 157, NA, 174), weight = c(95, 90, 99, 70, 77, NA),
#     age = c(23, NA, NA, NA, 21, 22))
# the returned value should be
# > data.frame(
#     height = c(178, 185, 173.5, 157, 173.5, 174), weight = c(95, 90, 99, 70, 77, 86.2),
#     age = c(23, 22, 22, 22, 21, 22))
#
# The return value should be a data.frame with the same columns as the input and with the missing values
# imputed. You can assume that there is at least one value present in each variable.
# Your input may have different column names.
ex04Imputation <- function(data) {
  for (i in seq_len(ncol(data))) {
    data[is.na(data[, i]), i] <- mean(data[, i], na.rm = TRUE)
  }
  data
}

# This exercise is concerned with control structures: conditional execution and loops.
# (we will skip functions for now).

# Consider the following `data.frame` that records "opposites" of concepts:
#
#        concept           opposite
#      1   black              white
#      2    slow               fast
#      3     big              small
#      4     man              woman
#      5    many                few
#      6     man              woman
#      7    fast               slow
#
# indicating that "many" is the opposite of  "few" etc.
#
# Write a function that takes a database of this form, as well as a concept, as input and returns
# the opposite of the given concept. Note, however, that the opposite of an opposite is the concept
# itself, so with the database above, your function should report "white" as the opposite of "black",
# but also "black" as the opposite of "white".
#
# The function should now take two inputs, `database` and `concept` and return a `character(1)` string.
#
# Example inputs using the the knowledge database from above:
# > database = data.frame(concept = c("black", "slow", "big", "man", "many", "man", "fast"),
#     opposite = c("white", "fast", "small", "woman", "few", "woman", "slow"), stringsAsFactors = FALSE)
# The function should give the following outputs, as examples:
# > ex01Opposite(database, "black")
# --> "white"
# > ex01Opposite(database, "slow")
# --> "fast"
# > ex01Opposite(database, "small")
# --> "big"
# > ex01Opposite(database, "man")
# --> "woman"
# > ex01Opposite(database, "woman")
# --> "man"
#
# You may assume that the `concept` input can always be found in at least one of the
# two columns of `database`. Some of the concepts and their opposites may occur more than once.
# However, there are never any contradictions: if a concept occurs more than once, its opposite
# is the same, and if an entry of the `opposite` column is also in the `concept` column, then the
# corresponding `concept` entry is in the `opposite` column (see the "fast"/"slow" example above).
ex01Opposite <- function(database, concept) {
  concept.row <- database[database$concept == concept, ]
  opposite.row <- database[database$opposite == concept, ]
  if (nrow(concept.row) > 0) {
    return(concept.row$opposite[[1]])
  } else if (nrow(opposite.row) > 0) {
    return(opposite.row$concept[[1]])
  }
}
# A "cellular automaton" is a discrete model of state evolution. We consider a state
# of a vector with "cells" of values 0 or 1, for example the vector c(0, 1, 0, 1, 0, 0, 0).
# The state now changes according to a special rule: Every cell changes to state 1
# whenever its immediate left neighbour has state 1. A cell that has state 1 remains at state 1.
# Each cell that has no left neighbour or a left neighbour that is 0, and that is also 0 itself, remains 0 otherwise.
# The evolution of the vector above would be
# > c(0, 1, 0, 1, 0, 0, 0)
# > c(0, 1, 1, 1, 1, 0, 0)
# > c(0, 1, 1, 1, 1, 1, 0)
# > c(0, 1, 1, 1, 1, 1, 1)
#
# Write a function that takes as input a vector of 0s and 1s and "evolves" the state until no
# more changes occur. The return value should be a
# list of states, with the first entry being the input vector.
# The upper example would look like this:
# > initial.state = c(0, 1, 0, 1, 0, 0, 0)
# --> returns
# list(c(0, 1, 0, 1, 0, 0, 0),
#      c(0, 1, 1, 1, 1, 0, 0),
#      c(0, 1, 1, 1, 1, 1, 0),
#      c(0, 1, 1, 1, 1, 1, 1))
#
# Other examples:
#
# > initial.state = c(0, 1, 0, 0, 0, 1)
# --> returns
# list(c(0, 1, 0, 0, 0, 1),
#      c(0, 1, 1, 0, 0, 1),
#      c(0, 1, 1, 1, 0, 1),
#      c(0, 1, 1, 1, 1, 1))
#
# > initial.state = c(0, 0, 0, 0, 0)
# --> returns the value as of
# list(c(0, 0, 0, 0, 0))
# (i.e. a single entry, since this is already a state that will not change.)
#
ex02CellularAutomaton <- function(initial.state) {
  states <- list(initial.state)
  while (TRUE) {
    last.state <- states[[length(states)]]
    new.state <- numeric(length(last.state))
    for (i in seq_along(last.state)) {
      if (i == 1 || last.state[i - 1] == 0) {
        new.state[i] <- last.state[i]
      } else {
        new.state[i] <- 1
      }
    }
    if (identical(new.state, last.state)) {
      break
    }
    states <- append(states, list(new.state))
  }
  return(states)
}

# Write a function that converts distances between units.
#
# Your function should convert distances between the units "km",
# "miles", and "nautical miles", where one "miles" is exactly 1.609344 "km",
# and one "nautical miles" is exactly 1.852 "km".
#
# Your function should take three arguments:
# * `distance`, a finite numeric vector of arbitrary length
# * `from`, a string (single element character vector) which should be one of
#   "km", "miles", or "nautical miles"
# * `to`, a sring (single element character vector) which should be one of
#   "km", "miles", or "nautical miles"
# Output should always be rounded to three decimal places; you will probably
# want to use the `round()` function.
# The `from` argument should be *optional*; if it is not given, conversion should
# be from "km".
# The `to` argument should be *optional*; if it is not given, conversion should
# be to "km".
# You may want to edit the `function(...)` line to get default arguments.
#
#
# Example behaviour:
# input: ex01DistConversion(0)
# output: 0
# input: ex01DistConversion(numeric(0))
# output: numeric(0)
# input: ex01DistConversion(100, "miles", "nautical miles")
# output: 86.898
# input: ex01DistConversion(c(1, 10000000), from = "miles")
# output: c(1.609, 16093440)
# input: ex01DistConversion(c(1, 2, 3, 4), "miles", "miles")
# output: c(1, 2, 3, 4)
ex01DistConversion <- function(distance, from = "km", to = "km") {
  assertNumeric(distance, any.missing = FALSE)
  assertChoice(from, c("miles", "nautical miles", "km"))
  assertChoice(to, c("miles", "nautical miles", "km"))
  distance.km <- switch(from,
                        km = distance,
                        miles = distance * 1.609344,
                        "nautical miles" = distance * 1.852)
  result <- switch(to,
                   km = distance.km,
                   miles = distance.km / 1.609344,
                   "nautical miles" = distance.km / 1.852)
  round(result, 3)
}

# Write a function that returns the n'th Fibonacci number
#
# The Fibonacci-Numbers are defined as
# F(0) = 0,  F(1) = 1,  F(n) = F(n - 1) + F(n - 2)
#
# Your function should take one parameter:
# * `n` a non-negative scalar integer value
# and should return a scalar value: the n'th Fibonacci number.
#
# Example behaviour:
# ex02Fibonacci(-1) -> ERROR
# ex02Fibonacci(0.5) -> ERROR
# ex02Fibonacci(0) -> 0
# ex02Fibonacci(1) -> 1
# ex02Fibonacci(2) -> 1
# ex02Fibonacci(3) -> 2
# ex02Fibonacci(4) -> 3
# ex02Fibonacci(5) -> 5
# ex02Fibonacci(6) -> 8
# ...
# ex02Fibonacci(11) -> 89
# ... and so on
#
# Think how you can use the definition above and recursion to solve this
# problem in very few lines.
ex02Fibonacci <- function(n) {
  assertCount(n, tol = 0)
  if (n <= 1) {
    return(n)
  }
  if (n %% 2 == 0) {
    n <- n / 2
    fn <- ex02Fibonacci(n)
    (2 * ex02Fibonacci(n - 1) + fn) * fn
  } else {
    n <- (n + 1) / 2
    ex02Fibonacci(n)^2 + ex02Fibonacci(n - 1)^2
  }
}
# Write a function that collects a bunch of strings into one string.
#
# By default, the function should take multiple arguments that are then collected
# together into one string, quoted with quotation marks and separated by commas.
#
# ex01Collate("a", "b", "c") --> '"a", "b", "c"'
# ex01Collate("x") --> '"x"'
# ex01Collate(" ", "") --> '" ", ""'
# ex01Collate('"') --> '"""'
# ex01Collate() --> ""
#
# (Note that the separation is with comma and a space, so ', ', not ',')
#
# The arguments should take a scalar string each.
# ex01Collate(character(0)) --> ERROR
# ex01Collate(c("a", "b", "c")) --> ERROR
# ex01Collate(3) --> ERROR
# ex01Collate(NULL) --> ERROR
#
ex01Collate <- function(..., sep = ", ", quote = '"') {
  assertString(sep)
  assertString(quote)
  args <- list(...)
  lapply(args, assertString)
  if (!length(args)) {
    return("")
  }
  paste0(quote, unlist(args), quote, collapse = sep)
}
# Write a function that calls the ex01Collate-function multiple times.
#
# ex02CollateLists(list(c("a", "b"), c("c", "d", "e"))) --> c('"a", "b"', '"c", "d", "e"')
#
# This function should take three arguments:
# * `inputs`: a list of character vectors that are given to the `ex01Collate` function.
#   The ex01Collate function is called with each list element in turn, and the individual
# The return value should be a character vector of the same length as the `input` list
# Examples:
# ex02CollateLists(list()) --> character(0)
# ex02CollateLists(list(c(" ", ""), '"', character(0), c("a", "b", "c"))) -->
#   c('" ", ""', '"""', "", '"a", "b", "c"')
# ex02CollateLists(list("a", c("b", "c"))) --> c('"a"', '"b", "c"')
# ex02CollateLists(list("a", c(x = "b", y = "c"))) --> c('"a"', '"b", "c"')  # names of vector elements are ignored
# ex02CollateLists(list(c("a", "b"), c(1, 2, 3))) --> ERROR (only character vectors should be given in the list)
# ex02CollateLists(list(list("a", "b", "c"))) --> ERROR (only char vectors in the list)
#
# Of course you can also solve this problem without using `ex01Collate` (we are just testing that
# this function behaves *as if* ex01Collate is being called) but that would probably be more work.
#
# You may find `do.call` to be useful here, as well as `lapply` or `vapply`.
ex02CollateLists <- function(inputs, sep = ", ", quote = '"') {
  assertList(inputs, types = "character", any.missing = FALSE)
  assertString(sep)
  assertString(quote)
  vapply(inputs, function(inp) {
    inp <- as.list(unname(inp))
    inp$sep <- sep
    inp$quote <- quote
    do.call(ex01Collate, inp)
  }, "")
}
# Write a function that takes in a `matrix` and returns a vector indicating, for each row of the input,
# the column of the first nonzero element.
# e.g.
#
# 0 1 0
# 1 2 3
# 0 0 1
# --> c(2, 1, 3)
# (the first row's first nonzero element is in the 2nd column, the second row has the first column nonzero,
# and in the last column the last column is the only nonzero one)
#
# Your function should have one input:
# * `mat`: a matrix with numeric
#
# Examples:
# ex03RowFirsts(diag(4)) --> c(1, 2, 3, 4)
# ex03RowFirsts(matrix(c(0, 1, 0, 1, 2, 0, 0, 3, 1), nrow = 3)) --> c(2, 1, 3)
#
# You can assume that every row has at least one nonzero element.
#
# You may find the `apply` function useful here.
ex03RowFirsts <- function(mat) {
  assertMatrix(mat, mode = "integerish", any.missing = FALSE)
  result <- apply(mat, 1, function(row) {
    which(row != 0)[[1]]
  })
}
# write a function that returns the diagonal of a data.frame.
#
# Your function should have one argument
# * `df` a data.frame with arbitrary rows and columns
# the return value should be an unnamed `list` of the diagonal elements of `df`.
# The diagonal elements are those elements, where the row index and column index
# coincide; this is defined even for non-square `data.frame` inputs; the length
# of the output is `min(nrow(df), ncol(df))`.
#
# Example:
# ex04TableDiag(data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE))
# --> list(1, "y")
# ex04TableDiag(matrix(c(1, 2, 2, 1), nrow = 2)) --> ERROR (not a dataframe)
# ex04TableDiag(iris)
# --> list(5.1, 3.0, 1.3, 0.2, factor("setosa", levels = c("setosa", "versicolor", "virginica")))
# ex04TableDiag(iris[FALSE, ]) --> list()
# # (zero-row data.frame has lengt-0 diagonal)
ex04TableDiag <- function(df) {
  assertDataFrame(df)
  n.row <- nrow(df)
  n.col <- ncol(df)
  min.length <- min(n.row, n.col)
  if (min.length == 0) {
    return(list())
  }
  diagonal <- vector("list", min.length)
  for (i in 1:min.length) {
    diagonal[[i]] <- df[i, i]
  }
  return(diagonal)
}

# Write a function that finds the root (i.e. the input for which its value is 0) of a function.
#
# The input function is always continuous and monotonically increasing, so fun(x + y) is
# greater or equal to f(x) when y is positive.
#
# Your function should have three arguments:
# * `fun`, a function
# * `lower`, a finite numeric scalar (single element vector)
# * `upper`, a finite numeric scalar greater or equal to `lower`
#
# `fun` can be assumed to be zero somewhere between `lower` and `upper`.
# You only need to find the root with a tolerance of 0.001, so
# > ex01FindRoot(function(x) x, lower = -1, upper = 1)
# has the result `0`, but `-0.001`, `0.001`, and `0.0002422213` are all accepted.
#
# Your result should always be between `lower` and `upper`, even if `fun` is also 0 outside this region.
#
# A simple way to solve this is to call `fun` with values `lower`, `lower + 0.001`, `lower + 0.002`, ...
# and return the input value for which `fun` is non-negative for the first time.
#
# Advanced students may try to use a more efficient way to solve this, such as e.g. the
# "Bisection method": <https://en.wikipedia.org/wiki/Bisection_method>.
#
# Your function should not use the `uniroot()` method or similar library methods.
#
# Example behaviour:
# input: ex01FindRoot(cos, -pi, 0)
# output: -1.570796 (or someting between -1.571796 and -1.569796)
# input: ex01FindRoot(function(x) round(x), -1, 1)  # should work even for functions that are not strictly monotonic
# output: something between -0.501 and 0.501
# input: ex01FindRoot(function(x) round(x), -1, 0)
# output: anything between -0.501 and 0
# input: ex01FindRoot(cos, 1, -1)
# --> ERROR because the upper bound is below the lower bound
# input: ex01FindRoot(function(x, y) x^3, -1, 1)  # y-parameter is ok here since it is not used
# output: anything between -0.001 and 0.001
ex01FindRoot <- function(fun, lower, upper) {
  assertNumber(lower, finite = TRUE)
  assertNumber(upper, finite = TRUE, lower = lower)
  assert_function(fun)
  tol <- 0.001
  x <- lower
  while (x <= upper) {
    if (fun(x) >= 0) {
      return(x)
    }
    x <- x + tol
  }
  stop
}


# Write a function that returns a function that checks a vector for conditions
#
# Your function should take one input:
# * `threshold`: a scalar numeric
# Your function should *return a function*. The returned function should have
# one argument `vect`: a numeric vector with no missing values.
# The returned function should return either TRUE or FALSE.
#
# The returned function should check whether its input contains
# at least one element greater than `threshold`.
#
# Examples
# fun <- ex02VectorCondition(threshold = 10)
# fun(c(1, 2, 3, 4)) --> FALSE
# fun(9:11) --> TRUE
# fun(numeric(0)) --> FALSE
#
# fun <- ex02VectorCondition(threshold = -1)
# fun(0) --> TRUE
# fun(c(-200, -100)) --> FALSE
# fun(c("10", "20")) --> ERROR (argument is not a numeric)
#
# ex02VectorCondition(threshold = "10") --> ERROR (not a numeric)
# ex02VectorCondition(threshold = c(1, 2, 3)) --> ERROR (not a scalar)
ex02VectorCondition <- function(threshold) {
  assertNumber(threshold)
  return(function(vect) {
    if (!is.numeric(vect)) {
      stop("Input is not a numeric vector.")
    }
    if (any(is.na(vect))) {
      stop("Input contains missing values (NAs).")
    }
    any(vect > threshold, na.rm = TRUE)
  })
}
# Write a function that selects out elements of a list based on a condition
#
# Your function should have two arguments
# * `vectors`: a list of `numeric` vectors
# * `threshold`: a scalar numeric
# Your function should return a character vector containing all
# elements from `vectors` that contain at least one number greater
# than `threshold`.
#
# Examples:
# ex03VectorThreshold(
#   list(numeric(0), 1:3, 8:11, c(-100, 100)),
#   threshold = 10)
# --> list(8:11, c(-100, 100))
# ex03VectorThreshold(list(), 10) --> list()
# ex03VectorThreshold(list(numeric(0), 0, -1), 10) --> list()
# ex03VectorThreshold(10, 10) --> ERROR (`vectors` is not a list of numerics)
# ex03VectorThreshold(list(10), "10") --> ERROR (threshold is not a numeric)
#
# You may want to use `Filter`, and the solution of ex02VectorCondition may be useful here.
ex03VectorThreshold <- function(vectors, threshold) {
  assertList(vectors, types = "numeric", any.missing = TRUE)
  assertNumber(threshold)
  Filter(ex02VectorCondition(threshold), vectors)
}
# Write a function that does ProgR-course homework grading
#
# The function takes two arguments, `problems` and `completion`:
# * `problems`: a character vector listing the available exercise problems.
#
#   Exercise problems given in the format "rfile.R:ex01FunctionName" -- the R-file,
#   followed by a colon, followed by the function name of the exercise. The '.R'
#   part is non-essential here, but the colon is, separating the "problem set"
#   from the "problem".
#
#   The `problems`-vector for the second homework would for example be:
problems.example <- c(
  "01_exercise_getting_started.R:ex01Multiply",
  "01_exercise_getting_started.R:ex02MultiplyVectors",
  "01_exercise_getting_started.R:ex03VectorType",
  "01_exercise_getting_started.R:ex04Odd",
  "02_exercise_vectors.R:ex01BinCounting",
  "02_exercise_vectors.R:ex02Binning",
  "02_exercise_vectors.R:ex03FizzBuzz",
  "03_exercise_matrices_and_dataframes.R:ex01ChessboardSum",
  "03_exercise_matrices_and_dataframes.R:ex02MatrixWhichMin",
  "03_exercise_matrices_and_dataframes.R:ex04SelectDF",
  "03_exercise_matrices_and_dataframes.R:ex05Imputation",
  "04_control_structures.R:ex01Opposite",
  "04_control_structures.R:ex02CellularAutomaton"
)
# * `completion`: a `data.frame` with two columns
#   - `student`: a character column indicating the student
#   - `solved`: a character column indicating the problems
#     completed successfully by the student.
#   The `completion` parameter contains one row per student and completed
#   exercise. If "studentA" has solved all problems from 01_exercise_vectors.R
#   and the first three problems of 02_exercise_matrices_and_dataframes.R, while
#   "studentB" has only solved the Counterdiagonal and the CellularAutomaton problem
#   then the input `completion` argument could look like this:
completion.example <- data.frame(
  student = c("studentA", "studentA", "studentB", "studentA", "studentA",
              "studentA", "studentA", "studentB", "studentA", "studentA", "studentA", "studentA"),
  solved = c("01_exercise_getting_started.R:ex02MultiplyVectors",
             "02_exercise_vectors.R:ex03FizzBuzz",
             "04_control_structures.R:ex02CellularAutomaton",
             "01_exercise_getting_started.R:ex01Multiply",
             "01_exercise_getting_started.R:ex03VectorType",
             "01_exercise_getting_started.R:ex04Odd",
             "02_exercise_vectors.R:ex02Binning",
             "03_exercise_matrices_and_dataframes.R:ex01ChessboardSum",
             "03_exercise_matrices_and_dataframes.R:ex02MatrixWhichMin",
             "03_exercise_matrices_and_dataframes.R:ex01ChessboardSum",
             "02_exercise_vectors.R:ex01BinCounting",
             "01_exercise_getting_started.R:ex03VectorType"),
  stringsAsFactors = FALSE
)
# The function should return a named `numeric` vector enumerating the scores of the students in any order.
#
# Scoring works as follows: Each student gets 0.25 points for each problem set (i.e. .R-file) that
# they solved completely. In the example above, "studentA" would get 0.5 points because the
# 01_exercise_getting_started.R-set and the 02_exercise_vectors.R-set are complete.
# "studentB" would get 0 points. The return for this example should therefore be
# ex01ProgrGrading(problems = problems.example, completion = completion.example)
# --> c(studentA = 0.5, studentB = 0)
# or
# --> c(studentB = 0, studentA = 0.5)
#
# Other example calls:
# ex01ProgrGrading(
#   problems = problems.example,
#   completion = data.frame(student = character(0), solved = character(0), stringsAsFactors = FALSE)
# ) --> numeric(0)
# ex01ProgrGrading(
#   problems = problems.example,
#   completion = data.frame(student = "studentA", solved = "01_exercise_vectors.R:ex01Multiplication",
#     stringsAsFactors = FALSE)
# ) --> ERROR (because "01_exercise_vectors.R:ex01Multiplication" is not one of the problems)
# ex01ProgrGrading(
#   problems = c("A:one", "B:one"),
#   completion = data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> c(studentA = 0.25)
# ex01ProgrGrading(
#   problems = c("A:one", "B"),
#   completion = data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> ERROR ('problems' list is badly formatted, because there is no ':' in problem "B")
# ex01ProgrGrading(
#   problems = c("A:one", "B::one"),
#   completion = data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> ERROR ('problems' list is badly formatted, because there are two ':' in problem "B")
# ex01ProgrGrading(
#   problems = c("A:one", "B:one", "A:two", "C:three"),
#   completion = data.frame(
#     student = c("studentA", "studentB", "studentC", "studentB", "studentC", "studentA"),
#     solved = c("C:three", "A:two", "C:three", "A:one", "B:one", "B:one"),
#     stringsAsFactors = FALSE)
# ) --> c(studentB = 0.25, studentC = 0.5, studentA = 0.5)
# You may find the `tapply()` function useful here.
ex01ProgrGrading <- function(problems, completion) {
  assertCharacter(problems, any.missing = FALSE)
  assertDataFrame(completion, ncol = 2)
  assertSubset(colnames(completion), c("student", "solved"))
  problems.split <- strsplit(problems, ":")
  if (any(vapply(problems.split, length, 0) != 2)) {
    stop("problem names malformed")
  }
  assertSubset(completion$solved, problems)
  if (!nrow(completion)) {
    return(numeric(0))
  }
  problems.sets <- vapply(problems.split, `[[`, 1, FUN.VALUE = character(1))
  resultvect <- tapply(completion$solved, completion$student, function(completed) {
    uncompleted.sets <- problems.sets[!problems %in% completed]
    completed.sets <- setdiff(problems.sets, uncompleted.sets)
    length(completed.sets) * .25
  })
  x <- as.vector(resultvect)
  names(x) <- names(resultvect)
  x
}

# Write a function that measures how long another function was running. The
# function should return the runtime of the given function, in seconds, rounded
# to the nearest integer second.
#
# The function being timed may sometimes throw an error, in which case the
# return value should depend on the `impute.inf`-argument: If it is `TRUE`, a
# value of `Inf` should be returned when an error was thrown, instead of the
# actual runtime. This would indicate that the function never actually finished.
#
# This function gets three arguments:
# - `fn` (function) the function to measure. It takes no arguments and should
#   be called as `fn()`.
# - `impute.inf` (`logical(1)`) whether to return `Inf`, instead of the runtime,
#   when `fn` throws an error.
#
# Functions that may be useful here are the `system.time()` function, or the
# `proc.time()` function. Make sure to use the `elapsed` part of the times that
# they report.
#
# Example functions to try out:
sleep1 <- function() Sys.sleep(1)
sleep2 <- function() Sys.sleep(2)
sleepErr <- function() {
  Sys.sleep(1)
  stop("error :-(")
}
# Example calls:
# > ex01TimeFun(sleep1, impute.inf = FALSE)  # 1
# > ex01TimeFun(sleep1, impute.inf = TRUE)  # 1
# > ex01TimeFun(sleep2, impute.inf = FALSE)  # 2
# > ex01TimeFun(sleepErr, impute.inf = FALSE)  # 1
# > ex01TimeFun(sleepErr, impute.inf = TRUE)  # Inf
ex01TimeFun <- function(fn, impute.inf = FALSE) {
  assertFunction(fn)
  assertLogical(impute.inf)
  assert_scalar(impute.inf)
  try.result <- try({
    start.time <- proc.time()
    fn()
    end.time <- proc.time()
    timing <- end.time - start.time
  }, silent = TRUE)
  if (inherits(try.result, "try-error")) {
    if (!impute.inf) {
      if (grepl("error", try.result, fixed = TRUE) && grepl("error :-(", try.result, fixed = TRUE)) {
        return(1)
      } else {
        return(0)
      }
    } else {
      return(Inf)
    }
  } else {
    elapsed.time <- as.numeric(timing[[3]])
    return(round(elapsed.time))
  }
}
# Write a function that behaves like a simple version of `lapply()`: It applies
# a given `FUN` to a list or vector `X`. However, if `FUN` throws an error,
# instead of aborting, the function should impute a value `impute`.
#
# This function gets three arguments:
# - `X`: Any kind of object that `lapply()` can iterate over. It is not
#   necessary to use a `checkmate` assert on this argument.
# - `FUN`: A function taking one argument.
# - `impute`: The value to use in cases where `FUN` throws an error.  This can
#   be any type, it is not necessary to use a `checkmate` assert here.
#
# > ex02SafeLapply(c(2, 16, 64), log2, impute = -1)  # list(1, 4, 6)
# > ex02SafeLapply(list(2, 16, "x"), log2, impute = -1)  # list(1, 4, -1)
# > ex02SafeLapply(list(2, 16, "x"), log2, impute = NULL)  # list(1, 4, NULL)
#
# Be aware that the return value of `FUN` could be the result of a `try()` call,
# so you should likely use `tryCatch()` instead of `try()` here:
# > ex02SafeLapply(list(1, NULL, try(stop("."), silent = TRUE)), identity, impute = -1)
# ## returns `list(1, NULL, try(stop("."), silent = TRUE))` -- -1 is not imputed
# ## here.
ex02SafeLapply <- function(X, FUN, impute) {
  assertFunction(FUN)
  lapply(X, function(x) {
    tryCatch({
      result <- FUN(x)
      return(result)
    },
    error = function(e) {
      return(impute)
    })
  })
}
# Palindromes are words or sentences that, when read backwards, give the same sentence,
# ignoring punctuation, spacing, or capitalization. Examples are
# > "Was it a car or a cat I saw?"
# > "Go hang a salami, I'm a lasagna hog"
# Write a function that returns TRUE if a given string is a palindrome, FALSE otherwise.
# Input arguments:
# - `input`: A `character(1)`.
# The input value is a character vector with one element (written `character(1)` and what we
# call a "string"). You can rely on there only being latin letters, punctuation marks and
# spaces being present, and that the string contains at least one letter.
#
# The `strsplit()` and `toupper()` or `tolower()` functions may help you here.
ex01Palindrome <- function(input) {
  assertString(input)
  input <- tolower(gsub("[^[:alpha:]]", "", input))
  rev.input <- paste(rev(unlist(strsplit(input, ""))), collapse = "")
  return(input == rev.input)
}

# We call the positive integer N a 'palindromic integer' if reversing the
# (decimal) digits of that number does not change its value. Examples of
# palindromic integers are: 1, 121, 45854. Examples of integers that are not
# palindromic are 10, 12, 46814.
# Write a function that, given a number, computes the smallest palindromic
# number greater than `n`.
# Input arguments:
# - `n`: non-negative integer `numeric(1)` scalar.
#
#> ex03NextPalindrome(100) --> 101
#> ex03NextPalindrome(4243) --> 4334
# Note that the return value is always greater than the input:
#> ex03NextPalindrome(1) --> 2
#> ex03NextPalindrome(0) --> 1
#> ex03NextPalindrome(11) --> 22
ex02NextPalindrome <- function(n) {
  assertNumeric(n, lower = 0)
  assertInt(n)
  assertScalar(n)
  while (TRUE) {
    n <- n + 1
    n.str <- as.character(n)
    reversed.n.str <- paste0(rev(strsplit(n.str, "")[[1]]), collapse = "")
    if (n.str == reversed.n.str) {
      return(n)
    }
  }
}
# The "Caeser" Cipher is a method of encrypting text that has sometimes been used,
# supposedly by the namesake.
# It uses a `plaintext` and a `key`, which is a single letter, and generates encrypted text.
# Each letter in the plaintext is converted to a number corresponding to its position in the alphabet.
# Likewise, the key is converted to a number.
# The value of the key is added to the letter values of the plaintext and converted back
# to letters (modulo the number of letters). Decryption can be done by subtracting,
# instead of adding, the key.
#
# We are working with the alphabet of the space character " " (value 0) and the 26 capital
# letters of the latin alphabet (contained in the R variable `LETTERS`, numbered 1..26.)
#
# Example:
# plaintext     = I LOVE MY PARENTS KYLIE MINOGUE AND KERMIT THE FROG
# key           = L
# plaintext converted to numbers:
#               = c( 9,  0, 12, 15, 22,  5,  0, 13, 25,  0, 16,  1, 18,  5, 14, 20, 19,  0, .....
# value of the key: 12
# sum of these two
#               = c(21, 12, 24, 27, 34, 17, 12, 25, 37, 12, 28, 13, 30, 17, 26, 32, 31, 12, .....
# some of the values are larger than 26, so we have to wrap them back around (modulo 27 -- note
# how we have 27 letters: 26 in the alphabet plus the space!):
# sum of plaintext and key with modulo:
#               = c(21, 12, 24,  0,  7, 17, 12, 25, 10, 12,  1, 13,  3, 17, 26,  5,  4, 12, .....
# converted back to letters:
#               = ULX GQLYJLAMCQZEDLWJXUQLYUZ SFQLMZPLWQCYUELETQLRC S
#
# A few more examples:
# plaintext: COME LETS EAT GRANDPA
# key:       A
# result:    DPNFAMFUTAFBUAHSBOEQB
# plaintext: I LIKE COOKING MY FRIENDS AND MY FAMILY
# key:       " " (space)
# result:    I LIKE COOKING MY FRIENDS AND MY FAMILY
#            (no encryption because " " corresponds to 0, so values are not changed)
# Implement a function that performs Caesar encryption or decryption, taking one
# `plaintext` and one `key` parameter. Both are Strings.
# Input:
# - `plaintext`: `character(1)`
# - `key`: `character(1)`
# - `decrypt`: `logical(1)` with *default value* `FALSE`.
# You should check that `key` is exactly a single uppercase letter from `LETTERS` (or single space)
# and throw an error if not. Likewise, if `plaintext` contains anything other than uppercase
# letters or spaces, an error should be thrown (note the empty string `""` should be permitted).
# Also your function should take a logical `decrypt` argument. If `decrypt` is TRUE,
# then decryption should be performed instead of encryption (subtraction instead of addition).
# Default should be FALSE.
# You may find the `match()` function and the modulo operator %% useful.
#
# Be aware that this cipher is very insecure and you should not use it to actually hide information,
# as you will find in the following exercises.
# You can read more about the cipher at Wikipedia: <https://en.wikipedia.org/wiki/Caesar_cipher>
ex01CaesarCipher <- function(plaintext, key, decrypt = FALSE) {
  assertString(plaintext)
  assertString(key)
  assertString(match.arg(key, c(" ", LETTERS)))
  assertLogical(decrypt)
  alphabet <- c(" ", LETTERS)
  plaintext <- toupper(plaintext)
  key <- toupper(key)
  if (!(key %in% c(" ", LETTERS))) {
    stop("Invalid key. Please provide a single uppercase letter or space.")
  }
  if (!grepl("^[A-Z ]*$", plaintext)) {
    stop("Invalid plaintext. Please provide uppercase letters or spaces only.")
  }
  plaintext.values <- match(strsplit(plaintext, "")[[1]], alphabet) - 1
  key.value <- match(key, alphabet) - 1
  if (decrypt) {
    encrypted.values <- (plaintext.values - key.value) %% length(alphabet)
  } else {
    encrypted.values <- (plaintext.values + key.value) %% length(alphabet)
  }
  encrypted.text <- paste(alphabet[encrypted.values + 1], collapse = "")
  return(encrypted.text)
}
# The following concerns itself with breaking the Caesar Cipher.
# This means we are writing a function that, when given a long enough
# encrypted text, can infer the encryption key and decrypt the message
# automatically.

# To efficiently break encryption, we need an indicator that tells us whether
# the decrypted text is, in fact, a plain text that would be interesting.
#
# Write a function that estimates the log-likelihood that a given text is, in
# fact, a non-encrypted plain text, using the distribution of letters.
# Input:
# - text: A `character(1)` string made up of upper case letters and space
# Return: a scalar `numeric` giving the log likelihood of a given text. It can
# be calculated as the sum of the log likelihoods of individual letters in
# the text. The likelihoods of individual letters for the english language is
# given in the following table (based on an average word length of 4.79,
# as well as on the table in <https://en.wikipedia.org/wiki/Letter_frequency>):
letterfrequencies <- 1 / 100 * c(
  A = 6.756, B = 1.234, C = 2.302, D = 3.518, E = 10.508, F = 1.843, G = 1.667,
  H = 5.041, I = 5.763, J = 0.127, K = 0.639, L = 3.330, M = 1.990, N = 5.583,
  O = 6.210, P = 1.596, Q = 0.079, R = 4.953, S = 5.234, T = 7.492, U = 2.282,
  V = 0.809, W = 1.952, X = 0.124, Y = 1.633, Z = 0.061, ` ` = 17.272)
# Example results of this are:
# ex02TextLikelihood("COME LETS EAT GRANDPA")
# #> -59.58358
# ex02TextLikelihood("DQPFBOFVVAGDUBJSCQERD")
# #> -86.87443
# Note that the log likelihood is larger (i.e. less negative) for the true
# english language text, and smaller for the encrypted text.
ex02TextLikelihood <- function(text) {
  assertString(text, pattern = "^[A-Z ]+$")
  sum(log(letterfrequencies[strsplit(text, "")[[1]]]))
}


# Write a function that estimates the most likely key for a given ciphertext.
# This is the key that generates a text that is most likely according to
# ex02TextLikelihood. The possible keys are the 26 letters as well as the space
# (`" "` -- this one does not change the text). You can make use of your
# `ex01CaesarCipher()` function with `decrypt = TRUE` to find out the
# decrypted text for a key.
#
# Input:
# - `ciphertext`: A `character(1)` string made up of upper case letters and space
# Return: a list with two entries:
# - `key`: `character(1)` giving an upper case letter or space.
# - `log.likelihood`: `numeric(1)` giving the log likelihood of the text when
#   decrypting with this key.
# The result for the ciphertext
#   "NDJPBJHIPLDG PILUCINPVDJGPXDJGPHXYVIHPIXYHPBDCIX"
# should, for example, be
# #> list(key = "P", log.likelihood = -142.08608554750788)
ex03BreakCaesar <- function(ciphertext) {
  assertString(ciphertext, pattern = "^[A-Z ]+$")
  keys <- c(LETTERS, " ")
  result <- vapply(keys, function(k) {
    ex02TextLikelihood(ex01CaesarCipher(ciphertext, k, TRUE))
  }, numeric(1))
  list(key = names(which.max(result)), log.likelihood = max(result))
}

# Someone tries to hide his encrypted text by combining it with other text that
# is made up of actual random letters, for example:
example.hidden <- c(
  "VBXLMQEYX XCKDCCLWZKWUUYBIFEDUQOMFNBLCWIARSCMMGK",
  "SXEUTM SOGYPLPPBRRJQWYKRCSMITBULZJVSWZTKEVFB JLB",
  "JTILBWAPELZEPD SNFTWEFMPSICAPJNMCASVZTKYGPDEGQQW",
  "ZDMEETHKBUZEPI  ZCKTDVHYH NLIOGENKDMVDUEOHXXTZNL",
  "NDJPBJHIPLDG PILUCINPVDJGPXDJGPHXYVIHPIXYHPBDCIX",
  "SOOKGBYLFOSXVGTOZNJMQMRHFKRRK UQPNLOUHFZDXILX ES",
  "NQKLUWOVNHAB F LSOWGOT E JSWR ZCWOCQIJLRRVYJHJTT",
  "EI UENSNER KELJFNVHTWNICT FWYJINYYXYEEGPJBSMJBWO",
  "MBDVGJDKZILLIOLINAAAMGJI  HGDOVPLQQCYSG SVJOWQQM",
  "WVRHDDWBAQRJCJNYJLRPVENPGOKBM CXLQHJPJBAXBCNLDLJ"
)
# The 5th element of this example vector is the ciphertext from the ex03 example
# above, the others are noise, i.e. letters sampled uniformly at random.
#
# Write a function that takes one input:
# - `hidden`: a `character` vector with no missing values and at least one element.
# Return: a `character(1)`: the decrypted text of the one element that is not random noise.
#
# The function can identify the true element by calling `ex03BreakCaesar` and
# deciding based on `log.likelihood` -- the highest (least negative) value
# is most likely a true sentence. The identified ciphertext can then be used to
# decrypt the identified element.
#
# (You may assume that if `hidden` is a `character`, then all its elements have the
# same length (otherwise the `log.likelihood` would need to be treated differently.)
#
# With input c("TSGWUQSHQURORLFBUUFLY", "DPNFAMFUTAFBUAHSBOEQB", "LQMOYL OPFJIFHBLQPYPM")
# the result should be "COME LETS EAT GRANDPA".
ex04BreakHidden <- function(hidden) {
  assertCharacter(hidden)
  best.likelihood <- -Inf
  best.result <- NULL
  best.key <- NULL
  for (i in seq_along(hidden)) {
    result <- ex03BreakCaesar(hidden[i])
    likelihood <- result$log.likelihood
    if (likelihood > best.likelihood) {
      best.likelihood <- likelihood
      best.result <- result
      best.key <- i
    }
  }
  decrypted.text <- ex01CaesarCipher(hidden[best.key], best.result$key, TRUE)
  return(decrypted.text)
}


# The Game "Tic Tac Toe" is a two player game with players taking turns occupying spaces on a 3x3 grid.
# A player wins by occupying either a full diagonal, a row, or a column of 3 spaces.
#
# The playing field is a 3x3 matrix, with columns named A..C, and rows numbered 1..3
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 |   |   |   |
# --+---+---+---+
#
# We are considering a modified "Tic Tac Toe" game with "gravity": The players take turns choosing a
# column "A", "B" or "C". They then occupy the lowest empty field of that column, as if they had put
# a token into the column and it had fallen down. E.g.:
#
# Player "X" chooses "A":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 | X |   |   |
# --+---+---+---+
#
# Player "O" chooses "B":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 | X | O |   |
# --+---+---+---+
#
# Player "X" chooses "A":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 | X |   |   |
# --+---+---+---+
# 3 | X | O |   |
# --+---+---+---+
#
# We are calling this version of the game "Gravity Tic Tac Toe"
#
# The goal of this exercise is to write a function that lets someone play 3x3 Gravity Tic Tac Toe against another
# human player, or against the computer.
# The state of the game (i.e. which fields are occupied by which players) is called the "position", similar to chess.
# Positions are represented by a 3x3-matrix with entries "X", "O" or NA.

# Part 01: Write a function that takes a position matrix and determines if there is a winner, and if
# so, who has won. The function should have a single input `position`; If this is not
# a 3x3 character matrix that may only contain "X", "O" or NA, an error should be thrown.
# The return value should be:
# - "X", if the "X"-player has won
# - "O", if the "O"-player has won
# - "" (empty string), if the game is a draw: no player has won, but there are no more possible moves
# - NA, if no player has won yet
# an error should be thrown if there are "two winners", e.g. a row of "X" and another row of "O",
# since this is an invalid position.
# It is not necessary to check for other indicators of invalid states. A position with two "X" and
# no "O" should explicitly be allowed, even though it would not be a reachable state in normal play
# (this could happen if one is giving one player an initial "advantage").
# Likewise, it is not necessary to verify whether `position` is a valid position when considering "gravity":
# Although a position where A3 is NA but A2 is "X" or "O" is not possible in this game, checking for this
# is not necessary.
#
# Hint: If you use `assertMatrix(mode = "character")`, you will find that
# `matrix(NA, nrow = 3, ncol = 3)` will trigger an error, because the "mode" of `NA` is `logical`
# by default. To get a character-NA-matrix, use `matrix(NA_character_, nrow = 3, ncol = 3)`. See
# > is.character(NA)  # FALSE
# > is.character(NA_character_)  # TRUE
# (Advanced students: There is a reason why the default `NA` is of type `logical`, can you think of what it is?)
ex01Winner <- function(position) {
  assertMatrix(position, mode = "character", nrow = 3, ncol = 3)
  assertSubset(position, c("X", "O", NA))
  getWinner <- function(spaces) {
    if (identical(spaces, rep("X", 3))) {
      "X"
    } else if (identical(spaces, rep("O", 3))) {
      "O"
    } else {
      NA_character_
    }
  }
  winners <- c(
    apply(position, 1, getWinner),
    apply(position, 2, getWinner),
    getWinner(diag(position)),
    getWinner(diag(position[, 3:1]))
  )
  winners <- unique(winners[!is.na(winners)])
  if (length(winners) > 1) {
    stop("Bad starting position")
  }
  if (length(winners)) {
    return(winners)
  }
  if (!any(is.na(position))) {
    return("")
  }
  NA
}

# Part 02: Write a function that takes a Gravity Tic Tac Toe position and a column that a player chooses to
# play, and converts them to numeric c(row, col) matrix coordinates of where the player's token will come
# to rest, checking if the column is a valid move in the process.
#
# Your function should take two inputs: `column`, and `position`.
# - `column` must be checked to be a `character(1)` with no missing values (using checkmate).
# - `position` should be a 3x3 position matrix with same constraints as in ex01Winner.
#   (It is *not* necessary to check whether `position` has a winner, or if there are two winners etc.,
#   but it should be checked for being a character matrix with valid content)
# `column` must be one of "A", "B", or "C". If it is not one of these, or if the indicated column is already
# full (i.e. the first row of that column is occupied), an error of class
# "InvalidMoveError" should be thrown, for example by using
# `stop(errorCondition(<message>, class = "InvalidMoveError"))`
# Note the difference between having an invalid type for `column` (e.g. if it is not a `character`, or if it
# is a `character` vector with more than one element), and having `column` of correct type (`character(1)`) but
# with invalid *content*. The first should be checked with `checkmate`, the second should generate an
# `"InvalidMoveError"`.
# You may assume without checking that `position` is a valid matrix when considering "gravity", i.e. that if
# row r, col c are occupied by "X" or "O", then all rows below r of that column c are also occupied.
# An example `position` could be
example.position <- matrix(
  c("X",  NA,  NA,
    "O", "X",  NA,
    "X", "O",  NA),
  ncol = 3, byrow = TRUE)
# In this case, with `column` "A", the function should throw an `InvalidMoveError`. With `column` "B", the
# function should return c(1, 2). With `column` "C", the return value should be c(3, 3).
ex02MoveStringToCoordinate <- function(column, position) {
  assertMatrix(position, mode = "character", nrow = 3, ncol = 3)
  assertSubset(position, c("X", "O", NA))
  assertCharacter(column, len = 1, any.missing = FALSE)
  columnToNumber <- list("A" = 1, "B" = 2, "C" = 3)
  if (!column %in% names(columnToNumber)) {
    x <- sprintf("Invalid move: %s.", column)
    stop(structure(list(message = x), class = c("InvalidMoveError", "error", "condition")))
  }
  colNumber <- columnToNumber[[column]]
  if (!is.na(position[1, colNumber])) {
    x <- sprintf("Column %s already full.", column)
    stop(structure(list(message = x), class = c("InvalidMoveError", "error", "condition")))
  }
  rowNumber <- max(which(is.na(position[, colNumber])))
  return(c(rowNumber, colNumber))
}
# Part 03: Write a function that interacts with a human player. Your function should take the arguments
# `position` and `player`.
# - `position` should be a 3x3 position matrix, checked in the same way as in ex02MoveStringToCoordinate.
# - `player` should be checked to be either the string "X" or the string "O".
#
# The function is called whenever the current position is `position`, and player `player` is asked to
# make a move. If the game is already over (a player has won, or there is a draw, as per ex01Winner),
# then an error should be thrown, as the player can no longer make a move.
# Otherwise, the function should prompt the player to make his choice, e.g. by printing the position,
# announcing that player `player` is to play, and getting an input by using the `readline()` function.
# (It is essential that `readline()` is called to prompt for input; this function will be replaced
# in the submission check).
# The user input should be sanitized (removing spaces e.g. using `gsub` or `trimws`, converting input
# to uppercase using `toupper`, so that an input of "  a " still counts as valid) and checked:
#  - if the input is "Q", an error should be thrown because the player has ended the game.
#  - if the input is not a valid coordinate as by `ex02MoveStringToCoordinate`, e.g. because the chosen
#    space is occupied or the input is not valid, then an information message should be printed
#    and the user should be prompted *again* for a new input. It is recommended to use the
#    ex02MoveStringToCoordinate function here, in combination with tryCatch, to check user input.
#
# The function should return the numeric coordinate c(row, col) where the token of the player will be
# placed; this is the result of ex02MoveStringToCoordinate of the user's input if the input was valid.
#
# Example interaction with the program:
# > ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3), "X")
## Position:
##   A   B   C
## 1 "X" NA  NA
## 2 "O" "O" NA
## 3 "X" "O" NA
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> d
## Invalid move: D.
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> a
## Column A already full.
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> b
# <RETURN VALUE:> c(1, 2)
#
# > ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3), "X")
## Position:
##   A   B   C
## 1 "X" NA  NA
## 2 "O" "O" NA
## 3 "X" "O" NA
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> Q
## Error in ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA),  :
##   Player X Quit
#
# The output of your answer does not need to be exactly like this, and
# the tests do not check if the function prints anything, but it is recommended to give some output
# to make everything nicer.
ex03HumanPlayer <- function(position, player) {
  assertMatrix(position, mode = "character", nrow = 3, ncol = 3)
  assertSubset(position, c("X", "O", NA))
  assertSubset(player, c("X", "O"))
  assertChoice(player, choices = c("X", "O"))
  message("Position:")
  print(position)
  message(paste("Player", player, "to move. What is your move?"))
  message('("A", "B", "C"; or "Q" to quit):')
  while (TRUE) {
    move <- toupper(trimws(readline()))
    if (move == "Q") {
      stop(paste("Player", player, "Quit"))
    }
    tryCatch({
      coord <- ex02MoveStringToCoordinate(move, position)
      return(coord)
    }, error = function(e) {
      message(e$message)
      message(paste("Player", player, "to move. What is your move?"))
      message('("A", "B", "C"; or "Q" to quit):')
    })
  }
}
# Part 04: Write a function that makes random, but valid, Gravity Tic Tac Toe moves
#
# The function should have the same arguments as the human player: `position`, and `player`,
# and they should be checked the same way (validity, and whether the game is already over).
# The function should return a numeric coordinate c(row, col) where the player's token is
# placed, which must be the result of a a valid move.
#
# > ex04RandomPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3), "X")
# --> c(1, 2) or c(3, 3)
#
# Regardless of the name of the function, the output does not *need* to be randomised, it should
# just be *something* that is not further checked by the submission script besides being a legal move.
#
# Note how both ex03HumanPlayer and ex04RandomPlayer work the same way: they have the same
# inputs and outputs, only one of them works internally while the other asks something of the user.
ex04RandomPlayer <- function(position, player) {
  assertMatrix(position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(position, c("X", "O", NA))
  assertChoice(player, c("X", "O"))
  valid.columns <- c("A", "B", "C")[colSums(is.na(position)) > 0]
  random.column <- sample(valid.columns, 1)
  coord <- ex02MoveStringToCoordinate(random.column, position)
  coord
}

# Write a function that lets two human players play against each other, or one player play
# against a computer opponent making random moves.
#
# Your function should have four arguments: `playerX`, `playerO`, `starting.player`, and `starting.position`.
# - `playerX` should be a *function*. It could, for example, be ex03HumanPlayer, or ex04RandomPlayer.
# - `playerO` should be a function as well, just as playerX.
# - `starting.player` should be a string and must be either "X" or "O". This argument should be optional,
#    defaulting to "X".
# - `starting.position` should be a valid position matrix, as already seen in ex02MoveStringToCoordinate.
#    This argument should be optional, defaulting to `matrix(NA_character_, 3, 3)`.
#
# This function is supposed to host a Gravity Tic Tac Toe game and should operate in a loop.
# 1. It should be checked whether the game is over. If so, the identifier of the winning player should be returned,
#    "X", "O", or "" (draw). It is recommended to use ex01Winner for this.
# 2. The next "player" should be asked for his move. Call the playerX or playerO function with the current
#    position (this may be a human player or the random player); the return value of the function is a
#    numeric c(row, col) coordinate where the player's token should be placed.
# 3. Do not trust the return value of the `"player"` function and check that the move made by the player is valid;
#    throw an error if not. You should use checkmate, and also check by verifying that ex02MoveStringToCoordinate
#    applied to the `col` (when converted to a letter) returns the same c(row, col).
# 4. update the position matrix by inserting an "X" or an "O" at the correct space.
#
# If the playerX / playerO functions don't return invalid moves, this loop should eventually end and
# return a "X", "O" or "" string. In fact, if the starting position is a win for a player (or a draw), this
# function should return immediately without calling a player function.
#
# It may be useful to have this function generate some output. However,
# if you choose to generate output, you *MUST* use `message()` to communicate with the user. Don't use print(), cat(),
# dput() or similar.
#
# Example calls:
# > ex05Tournament(ex03HumanPlayer, ex04RandomPlayer)
#  (human player playing against random, the "X" player begins on an empty field)
# > ex05Tournament(ex03HumanPlayer, ex03HumanPlayer, "O")
#  (two humans playing against each other, the "O" player starting)
# > ex05Tournament(ex04RandomPlayer, ex04RandomPlayer,
#     starting.position = matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3))
#  (two random players on a partially occupied field; enjoy the show.)
ex05Tournament <- function(playerX, playerO, starting.player = "X", starting.position = matrix(NA_character_, 3, 3)) {
  # your code
  assertMatrix(starting.position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(starting.position, c("X", "O", NA))
  assertChoice(starting.player, c("X", "O"))
  assertFunction(playerX)
  assertFunction(playerO)
  position <- starting.position
  currentPlayer <- starting.player
  while (TRUE) {
    winner <- ex01Winner(position)
    if (!is.na(winner)) {
      return(winner)
    }
    if (currentPlayer == "X") {
      move <- playerX(position, currentPlayer)
    } else {
      move <- playerO(position, currentPlayer)
    }
    tryCatch({
      validMove <- identical(move, ex02MoveStringToCoordinate(col = LETTERS[move[[2]]], position))
      if (!validMove) {
        stop("Invalid move returned by the player function")
      }
    }, error = function(e) {
      stop("Error valid move: ", e$message)
    })
    position[move[[1]], move[[2]]] <- currentPlayer
    currentPlayer <- ifelse(currentPlayer == "X", "O", "X")
  }
}
# Write a function that pads non-negative integer numbers to the same length by
# preprending 0s when necessary.
#
# Input:
# - `numbers`: A `numeric` containing positive integer numbers.
#
# Return: A `character` with the same length as `numbers`, containing strings
# that each have as many digits as the longest number in `numbers`.
#
# > ex01PadNumbers(c(1, 2, 3))  # c("1", "2", "3")
# > ex01PadNumbers(c(1, 2, 30))  # c("01", "02", "30")
# > ex01PadNumbers(c(100, 2, 30))  # c("100", "002", "030")
# > ex01PadNumbers(numeric(0))  # character(0)
# > ex01PadNumbers(c(0, 0, 0))  # c("0", "0", "0")
# > ex01PadNumbers(NA)  # NA
# > ex01PadNumbers(c(1, NA, 100))  # c("001", NA, "100")
#
# `numbers` may contain missing values, which should result in `NA` values. Make
# sure that your function does not return a string containing "NA" here.
#
# Your function should assert that numbers are non-negative values.
#
# `spintf()` could help you here.
ex01PadNumbers <- function(numbers) {
  assertIntegerish(numbers, lower = 0, any.missing = TRUE, min.len = 0)
  if (length(numbers) == 0) {
    return(character(0))
  }
  if (all(is.na(numbers))) {
    return(rep(NA_character_, length(numbers)))
  }
  if (length(numbers) == 1) {
    max.digits <- nchar(max(numbers, na.rm = TRUE))
    formatted.numbers <- sprintf("%0*d", max.digits, numbers)
    return(formatted.numbers)
  }
  max.digits <- max(nchar(numbers), na.rm = TRUE)
  formatted.numbers <- sprintf("%0*d", max.digits, numbers)
  formatted.numbers[is.na(numbers)] <- NA_character_
  formatted.numbers
}


# Write a function that pads numbers in file-names.
#
# Your function is given a vector of file names that may each contain at most
# one number. The numbers that occur in all files should be padded to the same
# length, as done in ex01PadNumbers.
#
# Padding numbers like this can be useful when sorting files alphabetically, to
# make sure that the same files that differ by a number are sorted correctly.
#
# Input:
# - `filenames`: A `character` containing file-names.
#
# Return: A `character` with the same length as `filenames`, containing
# filenames with padded numbers, if any.
#
# > ex02PadFiles("file.pdf")  # "file.pdf"
# > ex02PadFiles(c("podcast_ep1.mp3", "podcast_ep3.mp3", "podcast_ep22.mp3"))
#   # c("podcast_ep01.mp3", "podcast_ep03.mp3", "podcast_ep22.mp3")
# > ex02PadFiles(c("file1.pdf", "file10.pdf"))  # c("file01.pdf", "file10.pdf")
# > ex02PadFiles(c("file.pdf", "file10.pdf"))  # c("file.pdf", "file10.pdf")
# > ex02PadFiles(c("100-music.pdf", "file-10.pdf", "help.txt"))
#   # c("100-music.pdf", "file-010.pdf", "help.txt")
# > ex02PadFiles(character(0))  # character(0)
#
# Your function should not accept missing values.
ex02PadFiles <- function(filenames) {
  assertCharacter(filenames, any.missing = FALSE)
  if (!any(grepl("\\d", filenames))) {
    return(filenames)
  }
  numbers <- as.numeric(gsub("\\D", "", filenames))
  padded.numbers <- ex01PadNumbers(numbers)
  for (i in seq_along(filenames)) {
    if (!is.na(numbers[i])) {
      filenames[i] <- gsub("\\d+", padded.numbers[i], filenames[i])
    }
  }
  filenames
}
# Describe a parent-child relationship.
#
# This function gets three arguments:
# - `parent` (character vector length 1, i.e. string)
# - `male` (length 1 `logical` vector, indicating whether `parent` is male)
# - `child` (character vector length 1, i.e. string)
# Return a single string describing this relationship in human words.
# E.g. parent = "Eric", male = TRUE, child = "Bob" --> `Eric is the father of "Bob".`
#      parent = "Herbert", male = TRUE, child = "Klaus Dieter" --> `Herbert is the father of "Klaus Dieter".`
#      parent = "Hildegard", male = FALSE, child = "Adelheid" --> `Hildegard is the mother of "Adelheid".`
#      parent = "Y", male = FALSE, child  = "A"     --> `Y is the mother of "A".`
# Watch out for punctuation (quotation around children but not parent, period at the end).

ex01Children <- function(parent, male, child) {
  assertCharacter(parent, len = 1, any.missing = FALSE)
  assertLogical(male, len = 1)
  assertCharacter(child, len = 1)
  gender <- ifelse(male, "father", "mother")
  relationship <- paste0(parent, " is the ", gender, " of \"", child, "\".")
  return(relationship)
}

# Now reverse the above:
# Given a string `sentence`, extract the `parent`, `male`, and `child` arguments
# from above in that order as a named list.
# Input:
# - `sentence`: A `character(1)` string containing a sentence to analyse.
# E.g. 'Eric is the father of "Bob".' --> list(parent = "Eric", male = TRUE, child = "Bob")
#      'Herbert is the father of "Klaus Dieter".' --> list(parent = "Herbert", male = TRUE, child = "Klaus Dieter")
# (You can rely on the sentence structure, but do assert that `sentence` is a non-missing string.)
# You should be able to handle the case that the child's name contains quotation marks:
# 'Gudrun is the mother of "Rosamunde ("Rosi")".'
#   --> list(parent = "Gudrun", male = FALSE, child = 'Rosamunde ("Rosi")')

ex02ChildrenInverse <- function(sentence) {
  assertCharacter(sentence, any.missing = FALSE)
  assertVector(sentence, len = 1, any.missing = FALSE)
  # Extracting parent, male, and child from the sentence
  matches <- regmatches(sentence, regexec('^(.+) is the (father|mother) of "(.+)"\\.$', sentence))[[1]]
  # Extracted information
  parent <- matches[[2]]
  male <- matches[[3]] == "father"
  child <- matches[[4]]
  # Create a named list
  result <- list(parent = parent, male = male, child = child)
  return(result)
}

# Write a function that extracts all words from a text that are capitalized but
# not at the beginning of a sentence. Words can be returned in any order, but
# each word should only be returned at most once.
#
# Your function should take one argument:
#  - `text`: `character(1)` scalar string, the text from which to extract
#    capitalized words.
# Your function should return a `character` vector: The capitalized words found
# in the string.
#
# Example results:
#
#> ex03ProperNoun("Proper Nouns are usually Capitalized.")
# --> "Nouns"  "Capitalized"
#> ex03ProperNoun("proper nouns are usually Capitalized. This is, Proper for proper nouns.")
# --> "Capitalized" "Proper"
#> ex03ProperNoun("The IBM5100 Portable Computer was one of the first portable computers.")
# --> "IBM5100" "Portable" "Computer"
#> ex03ProperNoun("IBM5100 is the name of one of the first portable computers.")
# --> character(0)
# You can assume that the text only contains uppercase and lowercase letters
# from the latin alphabet, as well as digits, points (".") and commas (",").
# Words are separated from each other by exactly one space, and are possibly
# followed by a point (which starts a new sentence, so capitalization of the
# following word is ignored) or a comma (which does not start a new sentence).

ex03ProperNoun <- function(text) {
  assertCharacter(text, any.missing = FALSE)
  assertScalar(text)
  # Extract capitalized words not at the beginning of a sentence
  proper.nouns <- regmatches(text, gregexpr("(?<!\\.)\\s[A-Z][a-zA-Z0-9]*\\b", text, perl = TRUE))[[1]]
  # Remove leading and trailing spaces
  proper.nouns <- gsub("^\\s+|\\s+$", "", proper.nouns)
  # Sort the result to ensure consistency
  return(sort(unique(proper.nouns)))
}

# Write a function that normalizes date formats.
#
# Your function gets a vector of strings that my contain dates in various
# formats:
# - `YYYY-MM-DD`, e.g. "2023-12-01"
# - `YY-MM-DD`: "23-12-01"
# - `DD.MM.YYYY`: "01.12.2023"
# - `DD.MM.YY`: "01.12.23"
# - `MM/DD/YYYY`: "12/01/2023"
# - `MM/DD/YY`: "12/01/23"
#
# Your function should return the vector, where all occurrences of dates in
# these formats were converted to the `YYYY-MM-DD` format (a.k.a. the "correct
# format").
#
# It is not necessary to check for correctness of dates (i.e. silently accept
# "2023-13-32").
#
# When converting years from `YY` to `YYYY`, you should make use of the argument
# `century.cutoff` -- years years that are below or equal to the two-digit
# `century.cutoff` year should be converted to `20YY`, years above
# `century.cutoff` should be converted to `19YY`.
#
# Inputs:
# - `strings`: a `character` vector with no missing values, potentially
#   containing dates in different formats. Each entry of this vector may contain
#   multiple dates, and their format may differ.
# - `century.cutoff`: An integer `numeric(1)` between 0 and 99 inclusive,
#   indicating for which year to count a two-digit year as being in the 21st
#   century.
#
# Return: A `character` vector, with the same length as `strings`, with
# converted dates.
#
# The following input:
example.datestrings <- c("01/23/45",
                         "He was born on 22.11.31 and died on 01.06.30 at the age of 98.",
                         "This happened on 12/11/19, not on 12/11/1919.",
                         "'You went on a date?' -- 'Yes, I went on 12/12/22, which is a date'",
                         "",
                         "The YYYY-MM-DD format can be naturally sorted, making it superior to DD.MM.YYYY.",
                         "She was born on '01.02.03', but some people think she was born on '01/02/03'.")
# > ex01DateConvert(example.datestrings, century.cutoff = 30)
# should give:
# c("1945-01-23",
#   "He was born on 1931-11-22 and died on 2030-06-01 at the age of 98.",
#   "This happened on 2019-12-11, not on 1919-12-11.",
#   "'You went on a date?' -- 'Yes, I went on 2022-12-12, which is a date'",
#   "",
#   "The YYYY-MM-DD format can be naturally sorted, making it superior to DD.MM.YYYY.",
#   "She was born on '2003-02-01', but some people think she was born on '2003-01-02'.")
ex01DateConvert <- function(strings, century.cutoff) {
  assertCharacter(strings, any.missing = FALSE)
  assertIntegerish(century.cutoff, lower = 0, upper = 99, len = 1)
  patterns <- c(
    "YYYY-MM-DD" = "(\\b\\d{4})-(\\d{2})-(\\d{2}\\b)",
    "YY-MM-DD" = "(\\b\\d{2})-(\\d{2})-(\\d{2}\\b)",
    "DD.MM.YYYY" = "(\\b\\d{2})\\.(\\d{2})\\.(\\d{4}\\b)",
    "DD.MM.YY" = "(\\b\\d{2})\\.(\\d{2})\\.(\\d{2}\\b)",
    "MM/DD/YYYY" = "(\\b\\d{2})/(\\d{2})/(\\d{4}\\b)",
    "MM/DD/YY" = "(\\b\\d{2})/(\\d{2})/(\\d{2}\\b)"
  )
  convert.year <- function(year, cutoff) {
    if (as.integer(year) <= cutoff) {
      return(paste0(2000 + as.integer(year)))
    } else {
      return(paste0(1900 + as.integer(year)))
    }
  }
  strings <- lapply(strings, function(string) {
    for (pattern in names(patterns)) {
      matches <- gregexpr(patterns[pattern], string)
      if (any(matches[[1]] > 0)) {  # If there is a match
        match <- regmatches(string, matches)[[1]]  # Extract all matches
        for (m in match) {
          parts <- strsplit(m, "[-./]")[[1]]  # Split the match into parts
          if (pattern %in% c("DD.MM.YYYY", "DD.MM.YY")) {
            day <- parts[[1]]
            month <- parts[[2]]
            year <- parts[[3]]
          } else if (pattern %in% c("MM/DD/YYYY", "MM/DD/YY")) {
            month <- parts[[1]]
            day <- parts[[2]]
            year <- parts[[3]]
          } else {
            year <- parts[[1]]
            month <- parts[[2]]
            day <- parts[[3]]
          }
          if (nchar(year) == 2) {
            year <- convert.year(year, century.cutoff)
          }
          replacement <- paste(year, month, day, sep = "-")
          string <- gsub(m, replacement, string, fixed = TRUE)  # Replace the match
        }
      }
    }
    return(string)
  })
  return(unlist(strings))
}

# Write a function that checks whether given equations are correct or not.  An
# equation is given as a character string and will only consist of the
# operations '+', '-', and '='. You can rely on the fact that there will only be
# a '=' present, but both sides may contain multiple operations.  There will
# also not be any 'x + (-3)' or similar, that will always just be 'x - 3'.
#
# The input 'equations' shall be a character vector of separate equations.  Your
# function should return a logical vector indicating if the equations were
# correct or not.  As an example, the return for an input of:
example.equations <- c("3 + 4 - 10 = -3", "3+5 -2 + 2 =12", "9 - 2 + 4 = 0",
                       "2 =5", "3-4 = 5 - 6", "-30 + 40 = 10")
# should be 'c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)'
#
# NOTE: You should not use functions like `parse()`, `str2lang()` or
# `str2expression()` here. The use of `eval(parse())` is generally bad practice
# that you shouldn't learn, and would also miss the purpose of this exercise. It
# is not a solution accepted by the tests.  Functions like `do.call()` or
# `Reduce()` may be helpful here.
#
# NOTE 2: R is not able to represent decimal numbers exactly, which can lead to
# small deviations due to imprecision:
# > -.1 == 1 - 1.1
# #> [1] FALSE
# You should therefore accept both sides of the equation as equal when they
# differ by less than about 10^-8.
ex02EquationCheck <- function(equations) {
  assertCharacter(equations, any.missing = FALSE)
  result <- logical(length(equations))
  for (i in seq_along(equations)) {
    parts <- strsplit(equations[i], "=")[[1]]
    left.parts <- unlist(strsplit(gsub("(\\+|-)", " \\1 ", parts[[1]]), " "))
    right.parts <- unlist(strsplit(gsub("(\\+|-)", " \\1 ", parts[[2]]), " "))
    left.value <- 0
    right.value <- 0
    operator <- "+"
    for (part in left.parts) {
      if (part %in% c("+", "-")) {
        operator <- part
      } else if (grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", part)) {
        left.value <- get(operator)(left.value, as.numeric(part))
      }
    }
    operator <- "+"
    for (part in right.parts) {
      if (part %in% c("+", "-")) {
        operator <- part
      } else if (grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", part)) {
        right.value <- get(operator)(right.value, as.numeric(part))
      }
    }
    result[i] <- abs(left.value - right.value) < 10^-8
  }
  return(result)
}
# Write a function that finds all URLs in a text. Input is a long string `input`
# containing text, some parts of which are URLs.
#
# A URL is the sequence 'http://' or 'https://' (where the http/https-part
# indicates the PROTOCOL) if it is not immediately preceded by another letter
# ("abchttp://" does not start a url, but "abc http://" may), followed by the
# DOMAIN NAME (containing two or more "labels" separated by a dot, where each
# label consists of letters, numbers or minus-signs but does not start with a
# minus-sign; labels contain at least one character), followed by "/", followed
# by a PATH. We limit ourselves to PATHs that contain only latin letters,
# numbers, minus-signs and "/"s (slashes). All these rules are case-insensitive,
# so a URL may start with HTTP or hTTp.
#
# In short: <PROTOCOL>://<DOMAIN NAME>/<PATH>
#
# Given an input String, return a `data.frame` with the character columns
# "protocol", "domainname" and "path", containing the information about all the
# URLs found *in the order in which they were found*. (The same URL may occur
# multiple times and should then be listed in the data frame multiple
# times). Make sure you don't include things that are not URLs, i.e. which don't
# satisfy all the rules.
#
# Input:
# - `input`: A `character(1)` containing the input string.
#
# Example input:
# > "https://www.google.com/ is probably the most popular url, sometimes used ashttp://www.google.com/ if one forgets
#    to use the more secure https:// protocol. Another URL that is popular is http://youtube.com which is owned
#    by the same company. On some computers, it is possible to find a website at http://localhost/ if the
#    computer runs a webserver. A URL used in this course was HTTPS://GITHUB.COM/PROGR-2324/01_GIT."
# Output:
# > data.frame(
#     protocol = c("https", "HTTPS"),
#     domainname = c("www.google.com", "GITHUB.COM"),
#     path = c("", "PROGR-2324/01"))
# Example input:
# > "this text does not contain a url."
# Output:
# > data.frame(protocol = character(0), domainname = character(0),
#     path = character(0))
#
# Notes: many of the occurrences of http.... in the example do not count because
# they are either preceded directly by a letter, have no "/" following the
# domain name, or have a domain name with less than two labels. The path of the
# last URL is cut off early because we don't consider underscores as parts of
# the path.
#
# You should probably look into `regexpr`, `gregexpr`, `regexec`, `gregexec` and
# similar to solve this problem.
# First, install and load the checkmate package
#install.packages("checkmate")
ex01UrlFinder <- function(input) {
  assert_character(input, len = 1, any.missing = FALSE)
  if (nchar(input) == 0) {
    data.frame <- data.frame(protocol = character(0),
                             domainname = character(0),
                             path = character(0),
                             stringsAsFactors = FALSE)
    return(data.frame)
  }
  pattern <- "(?i)\\b(http|https)://([a-z0-9]+([\\.-]?[a-z0-9]+)*\\.[a-z]{2,})/([a-z0-9/-]*)?"
  matches <- gregexpr(pattern, input, perl = TRUE)
  protocol <- domainname <- path <- character(0)
  for (i in seq_along(matches[[1]])) {
    url <- regmatches(input, matches)[[1]][i]
    components <- regmatches(url, regexec(pattern, url))[[1]]
    protocol <- c(protocol, components[[2]])
    domainname <- c(domainname, components[[3]])
    if (length(components) > 4) {
      path <- c(path, components[[5]])
    } else {
      path <- c(path, "")
    }
  }
  data.frame(protocol = protocol, domainname = domainname, path = path, stringsAsFactors = FALSE)
}

# Most websites are rendered from HTML, which is a markup language where
# ordinary text is augmented through "tags" such as '<p>', '<strong>', '<br>'
# etc., which informs the browser about how the given text should be displayed.
# Many tags are used in pairs, with 'opening' tags such as '<p>', and
# corresponding 'closing' tags ('</p>').
#
# One way to display a *table* in HTML is to use the '<table>'/'</table>' tags
# (around the whole table), in combination with the tags '<tr>'/'</tr>' (which
# enclose individual *t*able *r*ows) and '<td>'/'</td>' (which enclose
# individual cells inside a row; the abbrevation is for *t*able *d*ata). The
# header of a table can be separated from the rest by using '<thead>'/'</thead>'
# and '<tbody>'/'</tbody>' tags. Header cells then use '<th>'/'</th>' instead of
# '<td>'/'</td>'. A very simple representation of the table
# | x | y |
# +---+---+
# | 1 | a |
# | 2 | / |
# Could, for example, be
#
example.table <- " The following is a table. this text preceding the table is not part of the table.
<table>
  <thead>
    <tr>
      <th> x </th>
      <th> y </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td> 1 </td>
      <td> a </td>
    </tr>
    <tr>
      <td> 2 </td>
      <td> / </td>
    </tr>
  </tbody>
</table>
"
#
# Where each line ends is non-essential here, and spaces around tags are
# ignored. The following is equivalent to the above:
#
# <table><thead><tr><th>x</th><th>y</th></tr></thead><tbody><tr><td>1
# </td><td>a</td></tr><tr><td>2</td><td>/</td></tr></tbody></table>
#
# (Although not necessary for this problem, if you are interested you can see
# https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table for a demo;
# you can e.g. go to https://www.ard-text.de/mobil/253, right-click and view
# source code to see how this used in the wild.)
#
# Write a function that, given the input 'html', a string containing an html
# table, extracts that table and returns a `data.frame` representing the table.
# The resulting `data.frame` should contain `character` typed columns that
# contain the data enclosed in '<tbody>'/'</tbody>', with column names according
# to the content in '<thead>'/'</thead>'. Text coming before and after the table
# should be ignored.
#
# Your function only needs to work with tables with cells that do not contain
# spaces, newlines, or the special characters "<" and ">" (i.e., the only "<"
# and ">" in the `html` string are the ones belonging to HTML tags). However,
# the content of the cells *may* contain "/". You can assume that the string is
# well-formed: There is exactly one table, with one '<thead>'/'</thead>' pair,
# containing one 'tr' pair, containing a number of 'th' pairs (at least
# one). '</thead>' is followed by a 'tbody'-pair with one or more 'tr' pairs,
# each containing as many 'td' pairs as there were 'th' pairs in 'thead'. Your
# function only needs to work with lowercase HTML-tags (i.e. no '<TABLE>',
# although that would be valid HTML elsewhere).
#
# The result for the `example.table` above should be
example.table.df <- data.frame(x = c("1", "2"), y = c("a", "/"))
#
# If you want to generate example cases for yourself, you can use knitr::kable:
#> example.input <-  knitr::kable(example.table.df, format = "html", align = NULL, row.names = FALSE)
# However, make sure you don't write this in the exercise R file, since the
# style checks won't accept the "::".
# The following input should also work:
#> example.input.stripped <- gsub(" |\n", "", example.input)
# Make sure that `ex02ParseHtmlTable(example.input)`,
# `ex02ParseHtmlTable(example.input.stripped)`, and
# `ex02ParseHtmlTable(example.table)` all give the same result, which should be
# `example.table.df`.
# Install the required packages
# Install the required packages
ex02ParseHtmlTable <- function(html) {
  assertString(html)
  column.names <- regmatches(html, gregexpr("<th>\\s*(.*?)\\s*</th>", html))[[1]]
  column.names <- gsub("<.*?>", "", column.names)
  column.names <- trimws(column.names)
  data.rows <- regmatches(html, gregexpr("<td>\\s*(.*?)\\s*</td>", html))[[1]]
  data.rows <- gsub("<.*?>", "", data.rows)
  data.rows <- trimws(data.rows)
  num.columns <- length(column.names)
  num.rows <- length(data.rows) / num.columns
  table.df <- data.frame(matrix(data.rows, ncol = num.columns, nrow = num.rows, byrow = TRUE))
  colnames(table.df) <- column.names
  return(table.df)
}
# In this exercise, you will be trying to make functions faster.
#
# Although performance is a decidedly tertiary goal of almost all code you will
# write, after correctness and understandability, it still helps to get a
# feeling for what operations are slow in R, and which ones are faster, and
# by how much.
#
# In the following exercises, you are given a reference-solution for each
# exercise. Your task is to write a function that behaves the same, but runs
# in less than 1/2 the time for the specified input (median speedup,
# as per `microbenchmark()`).
#
# Many of the functions can easily be sped up much more than by a factor of 2,
# and you are invited to see what methods give the largest gain.
# The speed measurement process with microbenchmark is a bit stochastic,
# however, and the test environment on the server probably also has a different
# CPU than your laptop, so different things may speed up by a different amount.
# The effect of this is not very large, but try to make the functions a bit
# faster than 2x speedup to have a safety buffer for the evaluation.
#
# As in the submissions before, you should only use R, not call any external
# programs or use any libraries except checkmate.
#
# Note: Because the reference functions were written to be deliberately slow,
# they should not serve as inspiration for your own code in the future!
#
# Hint: You probably want to copy the upper part of each function (the asserts)
# since they are usually not the slow part that can be made faster.


# You have taken a few noisy measurements, but you are worried about outlier
# values. You therefore want to extract a few of the largest, and of the
# smallest measurement values you have taken, so you can analyse them further.
#
# Write a function that keeps the `n.keep` largest, and the `n.keep`
# smallest values from a vector `x`, returning a vector of length
# `2 * n.keep` (or the original vector `x` if `n.keep` is larger than half the
# length of `x`).
# Input:
# - `x`: `numeric` vector of values from which to extract outliers. You can
#   assume that `x` does not have duplicate values.
# - `n.keep`: non-negative integer `numeric(1)` indicating the number of
#   outliers to keep.
# Returns: A `numeric` where the top `n.keep` and the bottom `n.keep` values
# of x are kept, but where their order is preserved.
#
# Your function should have a median runtime 1/2 as much as
# `ex01KeepOutliersReference` when called with a vector `x` of length
# 1000 and n.keep between 25 and 35.
# However, the correctness of your function is also checked for other input.
ex01KeepOutliersReference <- function(x, n.keep) {
  assertCount(n.keep, tol = 1e-100)
  assertNumeric(x, any.missing = FALSE)
  x.discard <- x
  # create a vector of everything that we want to discard
  for (i in seq_len(n.keep)) {
    x.discard <- x.discard[-which.max(x.discard)]
    x.discard <- x.discard[-which.min(x.discard)]
  }
  # get the complement of elements that are discarded. This works because
  # there are no duplicates in x
  setdiff(x, x.discard)
}

ex01KeepOutliers <- function(x, n.keep) {
  assertCount(n.keep, tol = 1e-100)
  assertNumeric(x, any.missing = FALSE)
  if (n.keep <= 0) {
    return(numeric(0))
  }
  if (n.keep >= length(x) / 2) {
    return(x)
  }
  order.indices <- order(x)
  outliers.indices <- c(order.indices[1:n.keep], order.indices[(length(x) - n.keep + 1):length(x)])
  return(x[sort(outliers.indices)])
}

# You have to make a decision between two alternative choices, choice A and
# choice B. To help in your decision, you have asked a large panel of experts
# in different fields how much they think choice A and choice B are preferrable.
# The experts evaluated the choices based on different things they consider
# relevant, such as profitability, environmental sustainability,
# effect on public relations, etc., and each expert has given choice A and
# choice B a score between 0 and 1. You will probably consider the experts'
# judgement in more detail, because some experts may be more reliable than
# others or consider more important issues than others. However, there is a
# specific shortcut you can take: If *no* expert has given choice A a *lower*
# score than choice B, but *at least one* expert has given choice A a *higher*
# score than choice B, then choice A "dominates" choice B, and you can disregard
# choice B right away.
# This is the concept of "Pareto Dominance" or "Pareto Efficiency":
# <https://en.wikipedia.org/wiki/Pareto_efficiency>.
#
# Write a function that calculates whether choice A dominates choice B that
# is faster than the following reference implementation.
# Inputs:
# - `scores.a`: `numeric` with values between 0 and 1 (inclusive), indicating
#   the scores given by each expert to choice A.
# - `scores.b`: `numeric` with values between 0 and 1 (inclusive), indicating
#   the scores given by each expert to choice B.
# `scores.a` and `scores.b` should have the same length.
# The `i`th component of `scores.a` and `scores.b` are the scores given by
# expert `i`, so they can be compared directly.
#
# Your function should have a median runtime 1/2 as much as
# `ex02DominatesReference` on input for `scores.a` and `scores.b` generated
# as follows:
# scores.a <- runif(200, 0.478, 1)
# scores.b <- runif(200, 0, 0.522)
# (I.e. evaluations from 400 experts, choice A dominates choice B in
# approximately 50% of cases)
# However, the correctness of your function is also checked for other input.
ex02DominatesReference <- function(scores.a, scores.b) {
  assertNumeric(scores.a, any.missing = FALSE, lower = 0, upper = 1)
  assertNumeric(scores.b, any.missing = FALSE, lower = 0, upper = 1, len = length(scores.a))
  
  a.can.dominate <- FALSE
  a.can.not.dominate <- FALSE
  for (i in seq_along(scores.a)) {
    if (scores.a[[i]] < scores.b[[i]]) a.can.not.dominate <- TRUE
    if (scores.a[[i]] > scores.b[[i]]) a.can.dominate <- TRUE
  }
  if (a.can.not.dominate) return(FALSE)
  if (a.can.dominate) return(TRUE)
  FALSE
}

ex02Dominates <- function(scores.a, scores.b) {
  assertNumeric(scores.a, any.missing = FALSE, lower = 0, upper = 1)
  assertNumeric(scores.b, any.missing = FALSE, lower = 0, upper = 1, len = length(scores.a))
  !any(scores.a < scores.b) && any(scores.a > scores.b)
}

# Ecologies
#
# The objective here is to again rewrite the following functions to be faster than
# the provided reference versions.
#
# Consider a very simple ecological model that predicts how many individuals
# of a given species are alive in time `t`. The system starts out with
# `x(1)` individuals at time 1. In every discrete time step, the population
# changes because of two effects:
# - reproduction with reproduction rate `qr`: After every time step `t`,
#   `(qr - 1) * x(t)` individuals are added.
# - starvation with carrying capacity `G`: The *proportion* of individuals that
#   die and are removed after time step `t` is `x(t) / G` (and therefore depends
#   on the quantity of individuals `x(t)`, i.e. the more individuals are alive,
#   the greater the fraction of individuals that die). The *absolute quantity*
#   of individuals that are removed is therefore `x(t) * x(t) / G`.
# With the two effects, the quantity of alive individuals at time step `t + 1`
# is therefore
#   x(t + 1) = x(t) + (qr - 1) * x(t)   - x(t) * x(t) / G
#            = qr * x(t)                - x(t) * x(t) / G
#            = (1 / G) * x(t) * qr * G  - (1 / G) * x(t) * x(t)
#            = (1 / G) * x(t) * (qr * G - x(t))
# (we are not doing rounding here and are working with continuous quantities.)
#
#
# Write a function that calculates the sequence of quantities `x(t)` for `t`
# between 1 and `t.max` (inclusive).
# Inputs:
# - `x1`: `numeric(1)` indicating `x(1)`, at least 0
# - `qr`: `numeric(1)` the reproduction rate, at least 1.
# - `g`: `numeric(1)` the carrying capacity, at least 0.
# - `t.max`: positive integer `numeric(1)` indicating the length of the result.
# Returns: a `numeric` vector giving the `x(t)` for `t` in 1 .. `t.max`.
#
# Your function should have a median runtime 1/2 as much as
# `ex01SimpleEcologyReference` for values of `t.max` of 1000, `qr` between 1
# and 4, `g` between 0 and 100, and x1 between 0 and `qr * g`.
#
# (If you are interested: This is the "logistic map" and plays a role in chaos
# theory: <https://en.wikipedia.org/wiki/Logistic_map>. To get from our formula
# to the one in Wikipedia, set `qr` to `r` and `G` to `1 / r`.)
ex01SimpleEcologyReference <- function(x1, qr, g, t.max) {
  assertNumber(x1, lower = 0)
  assertNumber(qr, lower = 1)
  assertNumber(g, lower = 0)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- x1
  xt <- x1
  for (t in seq_len(t.max - 1)) {
    xt.next <- 1 / g * xt * (qr * g - xt)
    result <- append(result, xt.next)
    xt <- xt.next
  }
  result
}

ex01SimpleEcology <- function(x1, qr, g, t.max) {
  assertNumber(x1, lower = 0)
  assertNumber(qr, lower = 1)
  assertNumber(g, lower = 0)
  assertInt(t.max, lower = 1, tol = 1e-100)
  xt <- x1 * numeric(t.max)
  xt[[1]] <- x1
  for (t in seq_len(t.max - 1)) {
    xt[t + 1] <- (1 / g) * xt[t] * (qr * g - xt[t])
  }
  xt
}

# A more complex ecological system, made up of two species, is described by the
# "Lotka-Volterra" equations. It is made up of the "predator" and the "prey"
# species, where the quantity of prey grows by a constant factor and gets
# reduced proportionally to how many predators are present, and the quantity of
# predators grows proportionally to how many prey are present and gets reduced
# by a constant factor. The actual Lotka-Volterra equations are differential
# equations, but here we will use a discretization of the model:
#   - prey(t + 1)     = prey(t) * qr.prey         - prey(t) * predator(t) * qi.prey
#   - predator(t + 1) = predator(t) * qr.predator + predator(t) * prey(t) * qi.predator
#   - `prey` and `predator` never become negative. When any of the equations
#     above has a negative result, then the values are set to 0.
# Where `qr.prey` >= 1 and 0 <= `qr.predator` <= 1: With no predators present,
# the prey would grow exponentially, and with no prey present, the predators
# would die out.
#
# Write a function that calculates the sequence of quantities `prey(t)` and
# `predator(t)` for `t` between 1 and `t.max` (inclusive).
# Inputs:
# - `x1`: `numeric(2)` indicating `prey(1)` in component 1 and `predator(1)`
#   in component 2, both at least 0.
# - `qr`: `numeric(2)` indicating the natural growth rates in the absence of
#   the other species: `qr[1]` is `qr.prey` (at least 1) and `qr[2]` is
#   `qr.predator` (between 0 and 1, inclusive).
# - `qi`: `numeric(2)` indicating the effect of growth rates of the species on
#   each other. `qi[1]` is `qi.prey` and `qi[2]` is `qi.predator`, both at least
#   0.
# - `t.max`: positive integer `numeric(1)` indicating the number of entries in
#   the result.
# Returns: a `matrix` with two columns and `max.t` rows, where columns are named
# `prey` and `predator` containing the quantities of both at each time between
# 1 and `max.t`.
#
# Your function should have a median runtime 1/2 as much as
# `ex02ComplexEcologyReference` for values of `t.max` of 100, `qr[1]` between
# 1 and 1.2, `qr[2]` between 0.8 and 1, and `qi` between 0 and 0.2. Note that `t.max`
# is less than in ex03!
ex02ComplexEcologyReference <- function(x1, qr, qi, t.max) {
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("prey", "predator")))
  xt <- x1
  for (t in seq_len(t.max)) {
    result <- rbind(result, xt, deparse.level = 0)
    xt <- xt * qr + c(-1, 1) * xt[[1]] * xt[[2]] * qi
    xt <- pmax(xt, 0)  # set values that are < 0 to 0
  }
  result
}

ex02ComplexEcology <- function(x1, qr, qi, t.max) {
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(0, ncol = 2, nrow = t.max, dimnames = list(NULL, c("prey", "predator")))
  result[1, ] <- x1
  for (t in seq_len(t.max - 1)) {
    prey.growth <- result[t, "prey"] * (qr[[1]] - result[t, "predator"] * qi[[1]])
    predator.growth <- result[t, "predator"] * (qr[[2]] + result[t, "prey"] * qi[[2]])
    result[t + 1, "prey"] <- max(prey.growth, 0)
    result[t + 1, "predator"] <- max(predator.growth, 0)
  }
  result
}

# Complex Fractals
#
# R can represent /complex numbers/, which can be written by using the letter i
# as the imaginary unit. The imaginary unit i is defined by (1i)^2 == -1.
# > x <- 1 + 2i.
# > x^2
# --> -3 + 4i
# (because: (1 + 2i) * (1 + 2i) == 1*1 + 1*2i + 2i*1 + 2i*2i == 1 + 2i + 2i + 4*(1i^2) == 1 + 4i + 4*(-1))
#
# Consider the sequence `Z[n] = Z[n-1]^2 + c`, with two parameters: the starting value `Z[0]`, and the
# offset `c`. For some values of `Z[0]` and `c`, this sequence diverges
# (with absolute value of Z[n] -> Inf as n -> Inf),
# for example for `c = 1`, `Z[0] = 1`, we have `Z[1] = 2`, `Z[2] = 5`, `Z[3] = 26`, ... -> Inf
# for others it does not,
# for example `c = -1/2`, `Z[0] = 0` --> `Z[1] = -0.5`, `Z[2] = -0.25`, `Z[3] = -0.4375` ... -> -0.366
#
# The sequence gets interesting when you start considering /complex numbers/ for `c` and `Z[0]`.
# The set of complex numbers `X`, for which this series does not diverge when having `Z[0] = c = X` is
# known as the *Mandelbrot Set*. The set of complex numbers `X`, for which this series does
# not diverge when having `Z[0] = X` and a given `c` is known as the *Julia Set* of `c`.
#
# The following function ex01FractalReference creates pictures of the Mandelbrot Set or Julia Set. It checks for
# divergence by applying the formula and checking if `Z` has reached an absolute value that is greater than
# `0.5 + sqrt(abs(c) + 0.25)`, where `abs(c)` is the absolute value of `c`.
#
# (This fact implies divergence, because
#   abs(Z[n + 1]) = abs(Z[n]^2 + c) >= abs(Z[n]^2) - abs(c) = abs(Z[n])^2 - abs(c).
# With abs(Z[n]) = 0.5 + sqrt(abs(c) + 0.25) + delta, delta > 0, this implies
#   abs(Z[n + 1]) >= abs(Z[n])^2 - abs(c) = (0.5 + sqrt(abs(c) + 0.25) + delta)^2 - abs(c)
#     = (0.5 + sqrt(abs(c) + 0.25))^2 + delta^2 + 2 * delta * 0.5 + [...]  # where (where [...] is > 0)
#     > (0.5 + sqrt(abs(c) + 0.25))^2 + delta
#     = 0.5^2 + sqrt(abs(c) + 0.25)^2 + 2 * 0.5 * sqrt(abs(c) + 0.25) + delta - abs(c)
#     = 0.25 + abs(c) + 0.25 + sqrt(abs(c) + 0.25) + delta - abs(c) = **0.5 + sqrt(abs(c) + 0.25) + delta**
# So abs(Z[n + 1]) > abs(Z[n]), and the sequence diverges)
#
# The function takes arguments `xrange` (a 2 element numeric), `yrange` (a 2 element numeric), and `resolution`
# (a numeric scalar) and creates a coordinate grid using
# `seq(from = <lower bound>, to = <upper bound>, by = 1 / resolution)`. Then a matrix is created representing
# complex numbers on this coordinate grid. When `c.param` is not given, the function creates a "Mandelbrot"
# matrix, and the matrix coordinate is used as the value for `c`. When `c.param` is given, then the
# matrix-coordinate is used as the initial value of Z ("Julia"-mode). Each matrix element receives the iteration,
# at which the recurrence formula gave rise to an absolute value > `0.5 + sqrt(abs(c) + 0.25)` for the first time.
#
# You can look at the results using the `image()` function:
# > res <- ex01FractalReference(30, 300)
# > image(t(res))
#
# Recommended calls:
#
# Mandelbrot Set:
# > ex01FractalReference(30, 300)
# > ex01FractalReference(100, 1000, xrange = c(-.3, .1), yrange = c(.8, 1.25))
# > ex01FractalReference(200, 10000, xrange = c(-0.8, -0.73), yrange = c(0.07, 0.18))  # SLOW!
#   ( Inspired by Wikipedia, <https://en.wikipedia.org/wiki/File:Mandel_zoom_02_seehorse_valley.jpg> )
# Julia Sets:
# > ex01FractalReference(300, 200, xrange = c(-1.5, 1.5), yrange = c(-1, 1), c.param = c(-0.512, 0.521))
#   ( Inspired by Wikipedia, <https://en.wikipedia.org/wiki/File:Julia_set,_plotted_with_Matplotlib.svg> )
# > ex01FractalReference(30, 400, xrange = c(-1.5, 1.5), yrange = c(-1.5, 1.5), c.param = c(0, 1))
#   ( "dendrite fractal", <https://mathworld.wolfram.com/DendriteFractal.html> )
# > ex01FractalReference(30, 400, xrange = c(-1.5, 1.5), yrange = c(-1.5, 1.5), c.param = c(-0.123, 0.745))
#   ( "Douady's Rabbit Fractal", <https://mathworld.wolfram.com/DouadysRabbitFractal.html> )
#
# The function given here is fully functional, but a bit slow. Your task is to *write the same function,
# but faster*. Write a function `ex01Fractal` that gives the same output and behaviour as the function
# ex01FractalReference. Your function should run in less than 1/2 of the time as the reference function,
# on a test-battery similar to the calls listed above (in particular, the 6 calls above with xrange and
# yrange close to the ones given here, with "resolution" 1/4 of the one given here.)
#
# As in the submissions before, you should only use R, not call any external programs or use any libraries
# except checkmate.
#
# Hint: before running the benchmark, make sure that your function is at least a little fast using the
# calls above.
# Hint: You probably want to copy the upper part of this function, in particular the part calculating
# the exact coordinates at which the matrix output is being sampled; this is helps to make sure the
# output of your ex01Fractal and the reference match well.
ex01FractalReference <- function(iters, resolution, xrange = c(-2, 0.5), yrange = c(-1.25, 1.25), c.param = NULL) {
  assertCount(iters, positive = TRUE)
  assertNumber(resolution, lower = 1, finite = TRUE)
  assertNumeric(xrange, any.missing = FALSE, finite = TRUE, len = 2)
  assertNumeric(yrange, any.missing = FALSE, finite = TRUE, len = 2)
  assertNumeric(c.param, any.missing = FALSE, finite = TRUE, len = 2, null.ok = TRUE)
  if (xrange[[2]] < xrange[[1]]) {
    stop("xrange[[2]] must be >= xrange[[1]]")
  }
  if (yrange[[2]] < yrange[[1]]) {
    stop("yrange[[2]] must be >= yrange[[1]]")
  }
  xvals <- seq(xrange[[1]], xrange[[2]], by = 1 / resolution)
  yvals <- seq(yrange[[1]], yrange[[2]], by = 1 / resolution)
  result <- matrix(0, nrow = length(yvals), ncol = length(xvals))
  for (row in seq_along(yvals)) {
    for (col in seq_along(xvals)) {
      current.complex <- xvals[[col]] + (1i) * yvals[[row]]
      Z <- current.complex
      if (is.null(c.param)) {
        c.val <- current.complex
      } else {
        c.val <- c.param[[1]] + 1i * c.param[[2]]
      }
      limit <- 0.5 + sqrt(abs(c.val) + 0.25)
      for (j in seq_len(iters)) {
        if (abs(Z) > limit) break
        Z <- Z^2 + c.val
      }
      result[row, col] <- j
    }
  }
  result
}

# I will display a ranking of the fastest functions that pass the test in a public GitHub repository
# for educational purposes (and as a small competition ;-) ) There will not be any reference to your name
# or your GitHub Username, unless you put these in a comment in the function, which I encourage you to do.
# However, if you really don't want me to publish your code / solution, please contact me by email or Mattermost
# -- Martin
# Complex Fractals
# ... (rest of the comments)

ex01Fractal <- function(iters, resolution, xrange = c(-2, 0.5), yrange = c(-1.25, 1.25), c.param = NULL) {
  assertCount(iters, positive = TRUE)
  assertNumber(resolution, lower = 1, finite = TRUE)
  assertNumeric(xrange, any.missing = FALSE, finite = TRUE, len = 2)
  assertNumeric(yrange, any.missing = FALSE, finite = TRUE, len = 2)
  assertNumeric(c.param, any.missing = FALSE, finite = TRUE, len = 2, null.ok = TRUE)
  if (xrange[[2]] < xrange[[1]]) {
    stop("xrange[[2]] must be >= xrange[[1]]")
  }
  if (yrange[[2]] < yrange[[1]]) {
    stop("yrange[[2]] must be >= yrange[[1]]")
  }
  xvals <- seq(xrange[[1]], xrange[[2]], by = 1 / resolution)
  yvals <- seq(yrange[[1]], yrange[[2]], by = 1 / resolution)
  cval <- c.param[[1]] + 1i * c.param[[2]]
  complexMat <- matrix(xvals, nrow = length(yvals), ncol = length(xvals), byrow = TRUE) +
    1i * matrix(yvals, nrow = length(yvals), ncol = length(xvals))
  res <- matrix(0, nrow = length(yvals), ncol = length(xvals))
  iters.number <- seq_len(iters)
  if (is.null(c.param)) {
    cval <- complexMat
  } else {
    cval <- matrix(cval, nrow = length(yvals), ncol = length(xvals))
  }
  limit <- 0.5 + sqrt(abs(cval) + 0.25)
  for (j in iters.number) {
    count <- !res
    element <- complexMat[count]
    limit.count <- limit[count]
    res[count] <- (abs(element) > limit.count) * j
    complexMat[count] <- element^2 + cval[count]
  }
  res[count] <- j
  res
}
# You may find this exercise a bit more challenging than other homework tasks,
# to balance the relatively easy 01 and 02 R-files in this homework. On the
# other hand, this is your chance to distinguish yourself!
#
# This is a continuation of the Gravity Tic Tac Toe exercise from a previous
# homework.
#
# To use the functions from the previous exercise, we have copied a solution of
# that exercise into `not_an_exercise_tictactoe.R`.
#
# If you are doing interactive experiments, you can "source" the file:
# > source("R/not_an_exercise_tictactoe.R")
# The evaluate_submission tests will make use of the content of
# not_an_exercise_tictactoe.R automatically.

# Write a function that plays Gravity Tic Tac Toe perfectly.
#
# This function should have arguments `position` and `player`,
# that behave just as in ex03HumanPlayer or ex04RandomPlayer from the Tic Tac
# Toe exercise, and return a move coordinate just as they do.
#
# Your function should make the optimal move in every situation: If it is
# to win, even against a perfrect opponent, your function should win.
# Otherwise, if it is possible to force a draw, your function should force a
# draw.
#
# Your function should find out which move is the best to make by:
#  - trying out all possible moves (i.e. all three columns) and
#  - creating a new position matrix representing the hypothetical position
#    where that move was played
#  - computing what the best move of the opposing player would be at this
#    point, and what the outcome of that would be, for example by recursively
#    calling ex01TBot itself directly or indirectly for each hypothetical
#    position.
# As a hint, a relatively elegant solution contains a call to ex05Tournament
# with ex01TBot for both players (using `suppressMessages` to keep output in
# check). If you want to read more about the system here (I don't think you
# have to to solve the exercise, but it gives some context):
# <https://en.wikipedia.org/wiki/Minimax#Minimax_algorithm_with_alternate_moves>.
#
# When you finish this, you should be able to play Gravity Tic Tac Toe against
# an unbeatable opponent using
# > ex05Tournament(ex03HumanPlayer, ex01TBot)
# (using ex05Tournament from not_an_exercise_tictactoe.R).
#
# ex01TBot should be able to make perfect moves even for 'unreachable'
# positions, such as two "O" and no "X" present, so don't rely on
# precomputation for this task.
ex01TBot <- function(position, player) {
  assertMatrix(position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(position, c("X", "O", NA))
  assertChoice(player, c("X", "O"))
  moves <- LETTERS[1:3]
  if (all(is.na(position))) return(c(3, 3))
  drawmove <- NULL
  validmove <- NULL
  for (m in moves) {
    coords <- tryCatch(
      ex02MoveStringToCoordinate(m, position),
      InvalidMoveError = function(e) {
        NULL
      })
    if (is.null(coords)) next
    validmove <- coords
    pos.new <- position
    pos.new[coords[[1]], coords[[2]]] <- player
    value <- suppressMessages(ex05Tournament(ex01TBot, ex01TBot, setdiff(c("X", "O"), player), pos.new))
    if (value == player) {
      return(coords)
    }
    if (value == "") {
      drawmove <- coords
    }
  }
  if (!is.null(drawmove)) {
    drawmove
  } else {
    validmove
  }
}
# In your career as a data scientist, you will often need to work on projects
# where you load and analyze data and create figures, tables, and reports.
# Having experience with a project structure that promotes reproduciblity will
# help you to keep your projects organized, enhancing collaboration,
# troubleshooting, and overall project sustainability.
#
# In this exercise, you will set up a project folder with a simple but general
# structure for a small data analysis project. While there are many ways to set
# up a project folder, in this exercise you will have to adhere to the structure
# presented in the slides. This will help you to get familiar with the
# structure, and it will give you some ideas for your own projects.
#
# The project itself is about analyzing the relationship between wind speed and
# wind electricity generation in Germany, using publicly available data. It
# should be stressed that the analysis that is being done in this exercise is
# mostly for demonstration purposes and may not be the best way to do this
# particular analysis in practice. However, it *is* a fun exercise that shows
# how easy it can be to generate nice plots and reports from publicly available
# data.
#
#
# The `ex_project` directory already contains many of the building blocks
# necessary for this project, since this exercise is mainly about organizing the
# files and folders in the right way. Your task is to put these files into the
# right places, and to create the missing files and folders. The following
# exercises will walk you through this process.
#
# Have a look at `example_windpower_report.pdf`, which is a precompiled version
# of the projects' report (when the project is completed) in order to understand
# better what this project is all about.
#
# ! ! !
# Everything you do in this exercise should be done inside the `ex_project`
# folder. You should not create any files or folders outside of it. We are going
# to refer to `ex_project` as the project's "root" folder.
#
# Because of the way the tests work, and the way RStudio handles `.Rmd`-files,
# all paths in this exercise should be relative to the project's root folder.
# This is *different* from the default path that you have when
# you open the exercise in RStudio. You should therefore run
# ```
#   setwd("ex_project")
# ```
# in the console to set the working directory to the project's root folder.
# ! ! !


# Exercise 01: Initialize Project
#
# As presented in the slides, we want to have cleanly organized sub directories
# in our project instead of just all files together. Create the directories
# `code/`, `data/`, `data/raw/`, `data/intermediate/` and `plots/` and move the
# files already present into the appropriate directories.
# Keep the already present `assets` directory, which is used to store additional
# report graphics as well as our references.
#
# Some of the directories you create will be empty at first. However, `git` does
# not keep track of empty directories. To make sure that these directories get
# pushed to GitHub (and recognized by the server-side tests!), you need to put
# an empty file into the directories that would otherwise be empty. It is
# recommended to create a file named ".gitkeep", by clicking on "New Blank File"
# in the RStudio file browser, choosing a "Text File", and entering the name
# `.gitkeep` as file name. You will have to click on "More" -> "Show Hidden
# Files" to make this file visible. If you want, you can also give the empty
# file some other name. The name `.gitkeep` is not mandatory, most other names
# would also work, but `.gitkeep` is the recommended name for this kind of file.
#
# Hint: You can create folders and move files directly from the "Files" tab in
# RStudio by using the menu at the top of that tab.
#
# Hint: All the data that you are provided with here is "raw" data, since
# "intermediate" data is data that is created by your code.
#
# ## Files relevant for this exercise: Most files in the `ex_project` folder.
# Create directories

# Exercise 02: Helper Script Files
#
# To simplify modifying global settings of a project that might change over time
# (like url of an api, or the timeframe for which a report is generated) it is
# useful to declare these in a separate script that is distinct from the code
# files.
#
# Here, we want you to create a script `settings.R` in the project's root folder
# and declare following global variables:
#
# - `path.raw` as `data/raw` (location of raw data in the project)
# - `path.intermediate` as `data/intermediate` (location of processed data)
# - `file.stations` as the relative path of the "stations.txt" file (hint:
#    starting with `data/raw/...`)
# - `file.measure` as the relative path of the "wind_raw.xz" file
# - `file.power` as the relative path "PowerGen.zip" file
# - `bins.long` as a vector containing the longitude bins: `c(6, 8, 10, 12, 14)`
# - `bins.lat` as a vector containing the latitude bins: `c(47, 49, 51, 53, 55)`
# - the following dates in the form of `Date` objects, created by calling
#   `as.Date("YYYY-mm-dd")`:
#   - `end.date.max`: set to 2023-11-01
#   - `start.date.min`: set to 2022-05-25
#   - `start.date.train`: 2022-06-01
#   - `end.date.train`: 2023-06-01
#   - `start.date.predict`: 2023-10-01
#   - `end.date.predict`: 2023-10-06
#   These dates will be used to filter the data to the appropriate timeframes.
#   You can also change `start.date.predict` and `end.date.predict` to different
#   values to see how they affect the resulting report.
# Also, do the following setup steps:
#   - set the ggplot theme to `theme_minimal()`, by using `theme_set()`
#   - load the `checkmate` library, since it is used in all other R-files.
#
# In addition to `settings.R`, the project should also have a helper script that
# sources all necessary R scripts by running the `source()` command on them.
# It should load files located in the `code/` folder, as well as the settings
# file. Therefore, create a second script `source_all.R` in the project's root
# folder that sources your `settings.R` script, as well as all `.R`-files in the
# `code/` directory. It should list the `settings.R` file directly, but it
# should not name any other files by name; instead, it should automatically
# detect all `.R` files in the `code/` directory and source them.
#
# Once you have this file, you should be able to load (and re- load after
# modifications) all relevant functions in this folder by running a single line
# of code interactively: `source("source_all.R")`.
#
# ## Files relevant for this exercise: `settings.R`, `source_all.R`.


# Exercise 03: Model code
#
# Some of the code is incomplete. Fill in the missing parts in the following
# functions in the `model.R` file:
# (a) Fill in the `windToWide()` function, which takes the wind speed data
#     that `windwpower_report.Rmd` loads into the `data.wind` variable and
#     transforms it into a wide format. The comment above the function explains
#     this in more detail and also provides an example input dataset.
# (b) Fill in the `fitModelRanger()` and `predictModelRanger()` functions, which
#     fit and predict a random forest model, respectively. You should use
#     `ranger::ranger()` to fit the model, as further explained by the comments
#     above the functions.
# Hint: You should run the first few chunks in the `windpower_report.Rmd` file
# to load the data that is used by these functions. You can then experiment with
# the functions interactively in the console.
# Use the green "play" button in the top right corner of each code chunk to run
# it, or use the grey triangle pointing downwards to "run all chunks above".
#
# Running the tests for this exercise may take a while, as they also attempt
# render the '.Rmd' file.
#
# ## Files relevant for this exercise: `model.R`.

# This exercise is a continuation of `01_project_structure_i.R`. You should solve
# it after you have completed the first exercise.

# Exercise 01: Setting Seeds
#
# One key aspect of reproducibility in statistical modeling involves setting
# seeds at specific points in your code. When you fit a statistical model, such
# as a machine learning algorithm, as the process involves some level of randomness
# (e.g., data splitting, initialization of weights). Setting a seed at a
# particular point ensures that these random processes generate the same
# sequence of numbers each time the code is run. As a result, the model fitting
# process becomes deterministic, guaranteeing the same set of parameters each
# run.
#
# While it would be sufficient in principle to set the seed once at the
# beginning of the report document, we want you to set the seed at the beginning
# of each code chunk that uses the RNG (random number generator) to a meaningful
# degree. This way, it would be possible to run each of the chunks individually
# and still get the same results.
# These chunks involving randomness are
# - "model penalized": fits the penalized models, which involves random data
#   splits for cross validation.
# - "squaresByOperator": fits another series of penalized models.
# - "prediction": fits the random forest model.
#
# Modify these chunks to set a seed at the beginning of each of them. Call
# `set.seed()` as the first line of each chunk, and use a different seed for
# each chunk. The seed needs to be hard-coded, so that it is possible to run
# these chunks individually.
#
# ## Files relevant for this exercise: `windpower_report.Rmd`


# Exercise 02: Cache Intermediate Results
#
# There is often no need to run expensive functions every time you re-knit a
# report or change some global settings that don't affect them. In our example
# report, some functions that load data or fit regression models take a
# relatively long time.
#
# The situation could be worse if you also had to download the data from an
# online source over an API, which can be slow or even unavailable at times.
#
# One possible solution for this would be to have a separate script that runs
# the analysis, whereas the report only loads the results from that script. The
# solution we want you to implement here, however, is to do caching directly in
# the report.
#
# (a): ** Cache the loading of input data **
#   In the `windpower_report.Rmd`, the first code chunk (not counting "setup")
#   loads and pre-processes various tables from the `data/raw` folder.
#   Loading the `data.energy` and `data.wind` tables takes a relatively long
#   time, and the data is not expected to change often.
#   Therefore, we want you to cache the loading of these two tables:
#   1. check if a file `file.path(path.intermediate, "tables.rds")` exists, e.g.
#      using `file.exists()`.
#   2. if it does not, create `data.energy` and `data.wind` the way it is
#      done in the report already.
#   3. save the two tables inside a named list
#        `list(data.energy = data.energy, data.wind = data.wind)`
#      and save that list into the `tables.rds` file, using `saveRDS()`.
#   4. If the `tables.rds` file *does* exist, then do *not* call
#      readDataEnergy(), readDataWindRaw(), or getWindInGrid(); instead, load
#      the `tables.rds` file using `readRDS()` and assign the two tables to
#      `data.energy` and `data.wind`.
#
#   (Note: In principle, another way to do this would be to use the `cache`
#   chunk option, we want you to do it manually here to get a better
#   understanding of what is going on.)
#
# (b): ** Cache the fitting / predicting regression models **
#   While fitting the linear model in `fitModelLM()` is relatively fast, the
#   other models are not:
#   - The penalized model takes a relatively long time, but creates a small
#     model object -- if we cache it on disk, we can save a lot of time.
#   - The random forest model takes a long time *and* creates a large model,
#     so we only want to cache the predictions that it makes.
#   Therefore, we want you to cache the fitting of the penalized model and the
#   predictions of the random forest model in 'model.R'.
#   For this, you should use the `cacheFunctionOnDisk()` function that is
#   provided in the `utils.R` file. You can use it by replacing a function
#   definition of the form
#     someFunction <- function(a, b) {
#       # <code>
#     }
#   with
#     someFunction <- cacheFunctionOnDisk(function(a, b) {
#         # <code>
#       },
#       file.path(path.intermediate, "cache_someFunction")
#     )
#   Note the usage of `path.intermediate` as the path where the cache files are
#   stored, and that the name of the function is used as part of the cache path.
#
#   For this exercise:
#   1. Find a way to make `source_all.R` load the `utils.R` file before sourcing
#      the `model.R` file. The easiest way is to just add a line
#      `source("code/utils.R")` at the top of `source_all.R`, but there are
#      other ways. It is not a problem if `utils.R` is loaded multiple times,
#      the only important thing is that it is loaded at least once before
#     `model.R`.
#   2. Cache the fitting of the penalized model by wrapping
#      `fitModelPenalized()` in `cacheFunctionOnDisk()`.
#   3. Cache the combined fitting and prediction of the random forest model by
#      wrapping `fitPredictModelRanger()` in `cacheFunctionOnDisk()`.
#   4. Calling Cached functions require *named* arguments, so you need to add
#      names to the arguments of the function calls to the cached functions in
#      the report '.Rmd' file.
#      Change all calls to `fitModelPenalized()` and `fitPredictModelRanger()`
#      to use named arguments (of the form `someFunction(a = val1, b = val2)`).
#
# You should not modify the `windpower_report.Rmd` in any way here, except that
# you should add the named arguments to the function calls. In particular, do
# not rename the functions; the tests check whether the functions in particular
# are cached.
#
# ## Files relevant for this exercise: `source_all.R` (make sure `utils.R` is
#    loaded before anything else), `model.R` (wrap the required functions),
#    `windpower_report.Rmd` (call the functions with named arguments).


# Exercise 03: README.md
#
# The project is still missing documentation. Create a `README.md` file that
# gets displayed when the `ex_project` folder is opened in GitHub. It must
# contain a title header for the project with the name 'Analysis of Wind Power
# in Germany', as well as at least 3 subheadings. (The tests require you to use
# the `#` syntax for the headers, not the `===` syntax.)
# Look at 'https://www.makeareadme.com/' for ideas as well as general
# information about writing in markdown and good practices.
#
# One of these subsections has to be named 'Usage', which has to have
# runnable(!) R-code. Include the code in a block that starts with
#   ```r
# and ends with
#   ```
# which tells Markdown to format the included text like R-code.
# The code should fit a linear model, e.g. by calling `fitModelLM()`. The code
# should build the same model as the report does, so it needs to `source()` the
# `source_all.R` file and then build the training data by calling the various
# `readData*()` and `get*()` functions in the same way as in the report.
# This code should not make use of caching!
# The result of the last line of code in the 'Usage' section should be the
# fitted model.
#
# Another section has to be 'Acknowledgements', which should contain a copy
# of the 'Acknowledgements' section from the report.
#
# Also, the report 'Rmd' file creates a plot of power prediction using the
# fitted models. It saves the plot as 'windPowerPrediction.png' in the folder
# 'plots'.
# Include this image at a reasonable point in the README.md file, using either
# `![](...)` or `<img src="...">` syntax.
#
# The tests only check for the specifics mentioned above, but feel free to
# make the README.md file more elaborate and fitting for the project.
#
# If you are using 'Rstudio', you can click 'preview' at the top of the '.md'
# file to see what it looks like compiled, or run
# `rmarkdown::render("README.md", "html_document")` in the console.
#
# ## Files relevant for this exercise: `README.md`


# Now your project is complete and you should be able to knit the
# 'windpower_report.rmd' to output a report in pdf form that is equivalent to
# the supplied 'example_windpower_report.pdf', by clicking the `Knit`-button
# in RStudio at the top of the editor window.
# You can also play around with the `[start|end].date.predict` variables
# in the `settings.R` file to see how the model predicts different time windows
# or how the model changes when trained on different data.
# You do not need to worry about the tests failing as long as there is wind data
# available for the specified time window.

# A few notes about the setup of this project:
# The project is supposed to teach you about good practices in structuring a
# project, and we have tried to have the code in it in a state that would be a
# good standard for an actual project. However, there are a few compromises that
# we had to make to keep the project simple enough, to make the tests work, and
# to adjust it to the current level of the course:
# - Usually, the root folder of the project would be the RStudio project folder
#   and the Git root folder at the same time.
# - One should ideally use `renv` to manage the project's dependencies.
# - Instead of using `source_all.R`, one could load all required functions in
#   the `.Rprofile` file, which is automatically loaded when R starts.
#   (This has some disadvantages as well: it makes problems if there is ever a
#   bug in a code file that prevents R from starting).
# - The data handling functions all convert from `data.frame` to `data.table`
#   and back. We did this so that you do not need to handle `data.table` for
#   this project. In an actual project, you would be using `data.table`
#   throughout.
# - For projects that are more complex, you would create reports separately from
#   running the analysis; you would have script files that save results, and the
#   report `Rmd`-file(s) would load these results.
# "Bootstrapping" is an approach to estimating distributions of statistics
# in which random artificial data is generated, often by randomly drawing with
# replacement from observed samples, and then used to calculate the statistic.
#
# Write a function that estimates the 50% confidence interval ("CI") of the
# regression coefficients of a simple linear model.
#
# The linear model is fitted to a dataset given to the function, which resembles
# the `cars` dataset that comes bundled with R. You can use
# > model <- lm(dist ~ speed, data)
# to fit a linear model which regresses the breaking distance against speed,
# and then use
# > coefficients(model)
# to get the model's coefficients (there are 2 coefficients in this model: the
# intercept and the coefficient of the `speed` feature).
#
# To make a bootstrap estimate of the CI, you should repeatedly generate random
# new datasets and fit a new model for each dataset. Take the model coefficients
# of each of the resulting models and calculate the 25% and the 75% quantile for
# each coefficient (e.g. by using the `quantile()` function).
#
# The random datasets should be created by randomly sampling rows from the
# original data *with replacement*; the number of rows in the sampled datasets
# should be the same as the number of rows in the original dataset. The
# expression
# > sample.int(nrow(data), size = nrow(data), replace = TRUE)
# might come in handy.
#
# Input:
#  - data: a `data.frame` with numeric columns `"speed"`, `"dist"`, similar to
#   the `cars` dataset that comes bundled with R.
#  - replicates: an integer valued `numeric(1)` greater than 4, indicating the
#   number of bootstrap samples to use.
# Return value: A 2x2 `matrix`. The first row should contain the 50% CI of the
# regression intercept, the second row should contain the 50% CI of the
# coefficient of the `speed` feature. The first column should contain the
# lower end of the CIs (i.e. the 25% quantiles of sampled coefficients), the
# second row the bootstrap estimate of the 75% quantile (the second row should
# therefore contain larger values than the first).
# The `replicate()` function may be helpful in this exercise (and some of the
# following ones).
#
# Your code should *not* set a seed (and should therefore return slightly
# different probabilistic results with every invocation).
#
# An example of the expected output -- the actual output is random, of course,
# but should get values that are close to this:
# > ex01BootstrapCars(cars, 1000)
#            [,1]       [,2]
# [1,] -21.246444 -13.657819
# [2,]   3.648419   4.195206
# (This indicates a 50% CI of the intercept between -21.2 and -13.7, and a 50%
# CI of the coefficient of the `speed` feature between 3.6 and 4.2.)
ex01BootstrapCars <- function(data, replicates) {
  assertDataFrame(data, any.missing = FALSE)
  assertNumeric(data$speed, any.missing = FALSE)
  assertNumeric(data$dist, any.missing = FALSE)
  assertIntegerish(replicates, lower = 5, upper = Inf, len = 1)
  bootstrap.results <- replicate(replicates, {
    sampled.indices <- sample(nrow(data), size = nrow(data), replace = TRUE)
    sampled.data <- data[sampled.indices, ]
    model <- lm(dist ~ speed, data = sampled.data)
    return(coefficients(model))
  }, simplify = "array")
  ci <- apply(bootstrap.results, 1, function(x) quantile(x, probs = c(0.25, 0.75)))
  ci <- matrix(c(ci[1, ], ci[2, ]), nrow = 2)
  return(ci)
}
# You are studying the effect that a specific food supplement has on the growth
# of mice. You do this by feeding the supplement to a small number of mice and
# measuring their weight in grams. You compare this to the weight that mice of
# the same species have after a control diet. You reject the null hypothesis of
# no effect if the p-value of the difference of average weights between the
# groups is below a threshold.
# There are not many mice in the treatment group, and there is another
# limitation: The weight of the treatment-group is measured in integer grams,
# Therefore, if a mouse actually weighs 10.33244524 grams, its reported value is
# rounded to 10.
#
# Write a function that simulates this experiment under the null-hypothesis,
# that the weight of the treatment group mice equals the weight of the control
# group mice on average, but is normally distributed around that average.
#
# Your lab has a large number of control mice, and you therefore know the
# average control group weight quite precisely; this is also the average
# treatment group weight under the null hypothesis. You can therefore simulate
# the experiment by drawing random mouse weights with a given mean and standard
# deviation. You need to incorporate the rounding in your simulations!
#
# Input values:
# - `weight.avg` : `numeric(1)` average weight of the control mice.
# - `weight.stddev`: standard deviation of mouse weights to use.
# - `treatment.size`: number of mice in the treatment group.
# Return value: Vector of length `treatment.size` of (positive, integer valued)
# simulated mouse weights.
ex02MouseWeightSim <- function(weight.avg, weight.stdev, treatment.size) {
  assertNumeric(weight.avg, len = 1, lower = 0, any.missing = FALSE)
  assertNumeric(weight.stdev, len = 1, lower = 0, any.missing = FALSE)
  assertIntegerish(treatment.size, lower = 1, len = 1, any.missing = FALSE)
  mouse.weights <- rnorm(treatment.size, mean = weight.avg, sd = weight.stdev)
  rounded.weights <- pmax(1, round(mouse.weights))
  return(rounded.weights)
}
# Continuation of ex02:
#
# Write a function that, given experimental data, calculates an approximate
# p-value.
#
# Wikipedia:
# > The p-value is the probability of obtaining test results at least as extreme
# > as the results actually observed, under the assumption that the null
# > hypothesis is correct.
#
# Do this by simulating experiments: Simulate experiments under the
# null-hypothesis (i.e. by calling your solution for ex02). Do this
# `simulation.rounds` times. See how often the difference between
# `control.weight.avg` and the average of mice in a simulated experiment is
# greater or equal to the actually observed difference between
# `control.weight.avg` and the average of `treatment.weights`.
#
# This is an estimate of the `p`-value, since it counts the fraction of cases
# under the null-hypothesis, in which the difference in averages between control
# and treatment group is at least as much as the observed difference.
# Input values:
# - `control.weight.avg` : `numeric(1)` average weight of the control mice. As
#   mentioned before, this value is known precisely.
# - `weight.stddev`: standard deviation of mouse weights to use.
# - `treatment.weights`: integer `numeric`: vector of weights of treatment mice.
# - `simulation.rounds`: number of experiments to simulate.
# Return return the estimated p-value as a `numeric(1)`.
ex03MouseWeightPVal <- function(control.weight.avg, weight.stdev, treatment.weights, simulation.rounds) {
  assertNumeric(control.weight.avg, len = 1, lower = 0, any.missing = FALSE, null.ok = FALSE)
  assertNumeric(weight.stdev, len = 1, lower = 0, any.missing = FALSE, null.ok = FALSE)
  assertIntegerish(treatment.weights, any.missing = FALSE, null.ok = FALSE)
  assertIntegerish(simulation.rounds, lower = 1, len = 1, any.missing = FALSE, null.ok = FALSE)
  observed.diff <- mean(treatment.weights) - control.weight.avg
  simulate.diff <- function() {
    simulated.weights <- rnorm(length(treatment.weights), mean = control.weight.avg, sd = weight.stdev)
    return(mean(simulated.weights) - control.weight.avg)
  }
  simulated.diffs <- replicate(simulation.rounds, simulate.diff())
  Pvalue <- mean(abs(simulated.diffs) >= abs(observed.diff))
  return(Pvalue)
}
# You are playing an online card game that uses R to simulate shuffling of
# cards. The card game uses 52 cards, ranked 2 to 10 together with Jack (J),
# Queen (Q), King (K) and Ace (A); There are four suits named Clubs (C),
# Diamonds (D), Hearts (H), Spades (S). The cards are represented by strings
# naming the suit first, followed by the rank, e.g. "C4" (Four of Clubs) or "DA"
# (Ace of Diamonds). The unshuffled deck is thus:
cards.ordered <- paste0(
  rep(c("C", "D", "H", "S"), 13),
  rep(c(2:10, "J", "Q", "K", "A"), each = 4)
)
# To get a shuffled deck, the online game uses the following function. The seed
# is based on the time elapsed since 1970-01-01, 00:00:00 in milliseconds;
# however, because the seed is limited to numbers below about 2e9, only the last
# 9 digits of that time are used (i.e. the rest from a division by 1e9 is used).
# The time elapsed since 1970-01-01, 00:00:00 is a very typical unit for time
# measurement in computer systems: `as.numreic(Sys.time())`, for example,
# returns the number of *seconds*. Our demo code here therefore multiplies this
# with 1000.
shuffleCards <- function() {
  milliseconds <- as.numeric(Sys.time()) * 1000
  set.seed(milliseconds %% 1e9)
  cards.ordered[sample.int(52)]
}
# At the beginning of each game, the first five cards of the shuffled deck are
# revealed, e.g.
# > cards.shuffled <- shuffleCards()
# > cards.shuffled[1:5]
#
# Write a function that infers the entire shuffled deck (`cards.shuffled` in the
# example) from the revealed cards and an estimate of when the `shuffleCards()`
# call was executed on the server. You do not know the exact time of the server
# on which the cards are shuffled, and there seems to be some variation caused
# by network delays and caching. However, you are reasonably sure that you can
# estimate the server's clock by plus/minus 2 seconds. You can therefore try out
# different candidate seeds that the server could have used, set it, and do the
# same operation as `shuffleCards()`, to see if you get the same first 5 cards.
#
# Unlike in some other exercises, you explicitly need to call `set.seed()` in
# this exercise.
#
# Inputs:
#  - `cards.shuffled.head` (`character(5)`): A vector of length 5 containing a
#   subset of `cards.ordered`, indicating the first five cards revealed in the
#   game.
#  - `time.estimate` (`POSIXct`): An estimate of the time at which the
#   `shuffleCards()` function was executed. (Do not call `Sys.time()` in your
#   solution, because there could be a delay between when the cards are shuffled
#   and when the first five cards are revealed). The time used is to create
#   `cards.shuffled.head` is off by at most plus or minus 2 seconds (2000
#   milliseconds). You can use `assertPOSIXct()` to check for validity of this
#   argument.
# Return value: A `character(52)` vector containing a guess at the complete
# deck. If no possible seed in the possible time window around `time.estimate`
# could have generated `cards.shuffled.head`, then an error should be thrown.
#
# Your function can make use of the `cards.ordered` variable defined above.
#
# If your function works, then you can try it out by running the following two
# lines in quick succession:
# > cs <- shuffleCards()
# > ex01InferCards(cs[1:5], Sys.time())
# which should return the same vector as `cs`.
ex01InferCards <- function(cards.shuffled.head, time.estimate) {
  assertVector(cards.shuffled.head, len = 5, any.missing = FALSE)
  assertScalar(time.estimate)
  assertCharacter(cards.shuffled.head)
  assertPOSIXct(time.estimate)
  # Generate possible seeds within the time window
  milliseconds <- as.numeric(time.estimate) * 1000
  possible.seeds <- seq(from = milliseconds - 2000, to = milliseconds + 2000, by = 1)
  # Iterate through possible seeds and check if they produce the same shuffled head
  for (seed in possible.seeds) {
    set.seed(seed %% 1e9)
    guessed.cards <- cards.ordered[sample.int(52)]
    if (identical(guessed.cards[1:5], cards.shuffled.head)) {
      return(guessed.cards)
    }
  }
  # If no valid seed is found, throw an error
  stop("Unable to infer the shuffled deck from the provided cards and time estimate.")
}

# Write a function that counts how often the RNG was invoked by another
# function.
#
# Input:
#  - `func` (`function`): A function that takes no arguments and returns nothing.
#   This function will call `runif()` a varying number of times, which your own
#   function should count.
# Return value: An integer `numeric(1)`, indicating how often the RNG was
# invoked by `func`. The return value should be capped at 1000, i.e. if `func()`
# generates more than 1000 random numbers, then your function should return
# 1000. Do this to avoid infinite loops.
#
# Your function can find out how often `func()` called `runif()` by saving the
# RNG state before and after `func()` is called, then resetting the RNG state to
# what it was in the beginning and counting how often `runif()` needs to be
# called to get back to the end state.
#
# Note that `.Random.seed` is a *global* variable, so you will need to use the
# `<<-` operator to assign to it.
#
# Your function should not set a seed or otherwise change the RNG state before
# `func()` is called, and `func()` should be called only once. However, you can
# rely on the fact that `.Random.seed` is set when your function is called.
#
# Example usage:
# > ex02CountRng(function() { runif(1); runif(1) })  # returns 2
# > ex02CountRng(function() { runif(3) })  # returns 3
# > ex02CountRng(function() {})  # returns 0
# > ex02CountRng(function() { runif(2000) })  # returns 1000
ex02CountRng <- function(func) {
  assertFunction(func)
  # Save the initial RNG state
  initial.state <- .Random.seed
  # Call the input function
  func()
  # Save the final RNG state
  final.state <- .Random.seed
  # Reset the RNG state to the initial state
  .Random.seed <<- initial.state
  # Count how many times runif() needs to be called to reach the final state
  count <- 0
  while (!identical(.Random.seed, final.state) && count < 1000) {
    runif(1)
    count <- count + 1
  }
  return(min(count, 1000))  # Cap the count at 1000
}
# In the following tasks you should write functions that handle data in a
# data.table. For all "data.table" inputs, your functions should
# `assertDataTable` the input value, but do not need to make any further
# assertions regarding the format of `data.table` arguments: You may assume
# that all "data.table" inputs have the correct columns in the correct format
# etc.

# Hint:
# For all these functions, you are given example input data that you can work
# with. Note that these are just *exapmles*, the tests will call your functions
# with other tables as well to make sure that your functions work in general,
# not just with the examples. Example tables often have the format
# tablename <- rbindlist(list(
#   list(title1 = NULL, title2 = NULL, ....),
#   list(value,       , value,         ....),
#   ....
# ))
# which you can run in the R console (in RStudio, just press ctrl-return) to get
# the variable `tablename`.
#
# Important:
# You may be writing some functions that modify the input data in-place. This
# means that calling one of your functions may actually cause the input value to
# be different after the call. Be aware of this if you try out your functions on
# the example data. E.g. if you do
# > result <- exXXFunction(input)
# then `input` itself may have changed and could give different results in
# future experiments, depending on what your code does with it.You should
# therefore always run your functions on a `copy` of the input data, like so:
# > result <- exXXFunction(copy(input))
# Alternatively, you can just execute the code that creates the example input
# data (or source this .R-file) again after each experiment.

# This exercise concerns itself with data in a certain format.
#
# A friend asked you to help him with his small corner store for various
# items. Because his shop is both a physical store where customers can
# take items, as well as an online shop that ships items, he has separate
# prices for items with and without delivery to reflect the different
# realities of the two markets.
# Your friend changes his inventory and prices monthly, and keeps a record of all
# sales made in that month. He wants to have a few functions that summarize
# the sales and revenue made in a month.
# His price-list could, for example, look like the following:
itemshop.prices <- rbindlist(list(
  list(item = NULL,             price.onsite = NULL, price.online = NULL),
  list("Healing Potion",        9.99,                12.99),
  list("Staff of Illusion",     18.95,               20.00),
  list("Lesser Stone of Mana",  2.60,                4.00),
  list("Greater Stone of Mana", 7.50,                9.99),
  list("Sword of Clarity +2",   21.50,               22.99)
))
# (the columns are constant, but actual datasets may have more or fewer rows
# with different items).
# Furthermore, your friend keeps a record of items sold in a table.
# The sales record has the following format:
itemshop.sales <- rbindlist(list(
  list(item = NULL,             channel = NULL),
  list("Healing Potion",        "online"),
  list("Sword of Clarity +2",   "onsite"),
  list("Sword of Clarity +2",   "online"),
  list("Sword of Clarity +2",   "onsite"),
  list("Greater Stone of Mana", "onsite")
))
# (Again, actual datasets may have more or fewer rows.)
#
# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.


# Write a function that counts the number of items sold for each item type.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output should be a `data.table` with columns `item` and `count`. Items
# with no sales should appear with a count of 0. The items
# should be in the same order as they appear in the `prices` table. The
# output with the two example datasets would be
itemshop.salescount <- rbindlist(list(
  list(item = NULL,             count = NULL),
  list("Healing Potion",        1),
  list("Staff of Illusion",     0),
  list("Lesser Stone of Mana",  0),
  list("Greater Stone of Mana", 1),
  list("Sword of Clarity +2",   3)
))
# You can use aggregation with `[... by ...]` here, and the special value `.N`
# may be useful. To get the same order as the `prices` table, a join could
# be useful.
ex01CountSales <- function(prices, sales) {
  assertDataTable(prices)
  assertDataTable(sales)
  sales[, .(count = .N), keyby = "item"][prices$item, .(item, count = nafill(count, fill = 0))]
}

# Write a function that counts the number of items sold for each type, and
# for each sales channel.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output should be a `data.table` with columns `item`, `count.onsite`, and
# `count.online`. Items with no sales should appear with a count of 0.
# The items should be in the same order as they appear in the `prices` table.
# The output with the two example datasets would be
itemshop.saleschannel.count <- rbindlist(list(
  list(item = NULL,             count.onsite = NULL, count.online = NULL),
  list("Healing Potion",        0,                   1),
  list("Staff of Illusion",     0,                   0),
  list("Lesser Stone of Mana",  0,                   0),
  list("Greater Stone of Mana", 1,                   0),
  list("Sword of Clarity +2",   2,                   1)
))
# This can be solved with aggregation similar to ex01CountSales, but `dcast()`
# (followed by a join) also works.
ex02CountSalesChannels <- function(prices, sales) {
  assertDataTable(prices)
  assertDataTable(sales)
  linesales <- sales[channel == "online", .(count.online = .N), keyb = "item"]
  sitesales <- sales[channel == "onsite", .(count.onsite = .N), keyby = "item"]
  res <- sitesales[linesales[prices$item]]
  setnafill(res, fill = 0, cols = c("count.online", "count.onsite"))
}

# Write a function that calculates the total revenue received by the shop.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output: a `numeric(1)`, summing up the total revenue as given by the price
# of each sold item for the respective channel.
# For the example dataset, the result would be `86.48`.
ex03Revenue <- function(prices, sales) {
  assertDataTable(prices)
  assertDataTable(sales)
  sum(prices[sales, ifelse(channel == "online", price.online, price.onsite), on = "item"])
}

# This exercise concerns itself with flight data. Every exercise of `data.table`
# has flight data somewhere, so here we go.
# Note that the data.table vignette at
# https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html
# uses very similar data (although we have a reduced version).
#
# The data is in the `flights.zip` file and can be loaded in the following way:
flights.data <- as.data.table(read.csv(unz("tables/flights.zip", "flights.csv"), stringsAsFactors = FALSE))
# (note that data.table offers a more convenient way of loading it, but it does
# not always work reliably on windows if 'unzip' is not in your $PATH:
# > flights.data <- fread("unzip -cq tables/flights.zip")
# )
#
# The data has the following columns:
# - year: year of the flight.
# - month: month of the flight (1-12)
# - day: day of the month of the flight (1-31)
# - dep_delay: delay of departure, in minutes
# - arr_delay: delay of arrival, in minutes
# - carrier: IATA airline designator of operating carrier
# - origin: IATA airport code of departure airport
# - dest: IATA airport code for destination airport
# - hour: planned hour of day of departure
#
# A few comments on the particular dataset:
# - The example data is from 2014, but multiple years in a dataset are possible
# - The example dataset contains only the six most frequent carriers, but the
#   tests could contain more or fewer.
# - The example dataset contains only the busiest airports, but the tests could
#   contain more or fewer.


# Write a function that calculates the median and maximal arrival delay of the
# 3 most frequent carriers.
#
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# Output:
# `data.table` with columns `carrier`, `delay.median`, `delay.max`, indicating
# the minimum and maximum arrival delay experienced by flights of the three
# most represented carriers in the dataset. The median delay should be of the
# flights that were delayed at all, i.e. flights that were not delayed should
# not be counted. (If there were no delays in any flights of a carrier, this
# value should be zero). Only the lines for the three carriers with the most
# flights should be given (in any order). You can rely on there being at least
# three different carriers in the dataset.
# The result with the example dataset could be (up to row order):
flights.delays <- rbindlist(list(
  list(carrier = NULL, delay.median = NULL, delay.max = NULL),
  list("AA",           19,                  1524),
  list("DL",           15,                  1107),
  list("UA",           18,                  668)
))
# (There may be a datatype error when you use `median()` inside a `[ ]`
# aggregation; in that case, use `as.numeric(median())`.)
ex01DelayStats <- function(flights) {
  assertDataTable(flights)
  topcarrier <- flights[, .N, by = "carrier"][head(order(-N), 3)]$carrier
  flights[topcarrier, on = "carrier"][, .(
    delay.median = as.numeric(median(arr_delay[arr_delay != 0])),
    delay.max = max(arr_delay)
  ), by = "carrier"]
}


# Write a function that returns the median delay of flights for each month and
# for each route.
#
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# - `year`: the year for which to aggregate. (Remember that `flights` could
#   contain data from multiple years.)
# Output:
# `data.table` with columns `month`, `delay`, `origin`, `dest`.
# `delay` should indicate the median arrival delay of flights from `origin` to
# `dest` in that month. The median delay should be of the flights that were
# delayed at all, i.e. flights that were not delayed should not be counted. (If
# there were no delays in any flight of given route, the `delay` should be 0.)
# All months of the given `year` should be considered, and the routes from
# and to all airports that are in either `origin` or `dest` of the `flights`
# argument. I.e. if there is only a flight from `"DEN"` to `"ATL"`, but no
# flight from `"ATL"` to `"DEN"`, then the `"ATL"` to `"DEN"` route should
# be *included* in the returned table and listed with a delay of 0.
# This is even true if the airports are listed in different years than
# the `year` given as argument. I.e. `ex02MonthlyDelays(flights.data, 2000)`
# should give a table with many entries that all have `delay` 0.
# A possible return value of this function when called with `flights.data` and
# 2014 is saved in `"ex02MonthlyDelaysResult.csv"`:
flights.monthlydelays <- fread("tables/ex02MonthlyDelaysResult.csv")
# It is a good idea to use the `CJ()` function to generate the table of all
# possible combinations of months, and origin and destination airports. In that
# case you have to take care to remove flights with same origin and destination
# airport, however.
ex02MonthlyDelays <- function(flights, year) {
  assertDataTable(flights)
  assertInt(year)
  allairports <- union(flights$origin, flights$dest)
  index <- CJ(year = year, month = 1:12, origin = allairports, dest = allairports)
  index <- index[origin != dest]
  yr <- year
  delaytbl <- flights[yr == year & arr_delay > 0,
                      .(delay = median(arr_delay)), by = c("month", "origin", "dest")]
  ret <- delaytbl[index, on = c("month", "origin", "dest")]
  setnafill(ret, fill = 0, cols = "delay")[, .(month, delay, origin, dest)]
}
# Write a function that counts the number of flights on each route that departed
# each month until a flight on that route was delayed more than 60 minutes.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# - `year`: the year for which to aggregate. (Remember that `flights` could
#   contain data from multiple years.)
# Output:
# `data.table` with columns `month`, `flights.to.delay`, `origin`, `dest`.
# Similarly to `ex02MonthlyDelays`, this function should aggregate across flight
# routes and months in a given given year.
# Rows should be chronologically ordered.
# For each route and month, the number
# of flights that departed from the start of that month until one flight's
# arrival was delayed more than 60 minutes should be given. The delayed flight
# should not be counted. So if 10 flights departed for a route in a given month
# and the 5th flight was delayed > 60 minutes, the return value would be 4. If
# no flight was delayed more than 60 minutes, the return value should be  the
# total number of flights on that route and month, i.e. 10 in this example. If
# no flight was on the given route during the month, the value is 0. Note that,
# as in `ex02MonthlyDelays`, the year could be different from the years in the
# `flights` dataset, which should result in a table full of `0`s.
#
# A possible return value of this function when called with `flights.data` and
# 2014 is saved in `"ex03FlightsToDelayResult.csv"`:
flights.flightstodelay <- fread("tables/ex03FlightsToDelayResult.csv")
ex03FlightsToDelay <- function(flights, year) {
  assertDataTable(flights)
  assertInt(year)
  allairports <- union(flights$origin, flights$dest)
  index <- CJ(year = year, month = 1:12, origin = allairports, dest = allairports)
  index <- index[origin != dest]
  yr <- year
  delaytbl <- flights[yr == year][order(day, hour),
                                  .(flights.to.delay = c(which(arr_delay > 60) - 1, .N)[[1]]),
                                  by = c("month", "origin", "dest")]
  ret <- delaytbl[index, on = c("month", "origin", "dest")]
  setnafill(ret, fill = 0, cols = "flights.to.delay")
  ret[, .(month, flights.to.delay, origin, dest)]
}
# Write a function that, for each month in a given year, and for each airline,
# calculates (1) the airline's mean flight delay, and (2) the flight delay of
# that airline's competition, i.e. every other airline's mean flight delay.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# - `year`: the year for which to aggregate. (Remember that `flights` could
#   contain data from multiple years.)
# Output:
# `data.table` with columns `month`, `carrier`, `mean.delay`,
# `mean.delay.competition`.
# The table should contain a row for each month and each each carrier, even
# if the given carrier (or its competition) do not have any flights on that
# month (in which case the respective `mean.delay` / `mean.delay.competition`
# should be 0).
# Rows should be chronologically ordered.
# A possible return value of this function when called with `flights.data`
# and 2014 is saved in `ex04CarrierDelayResult.csv`:
flights.carrierdelay <- fread("tables/ex04CarrierDelayResult.csv")
# As in other tasks, the `CJ()` function can be handy here to create a
# table of for all carriers within each month.
ex04CarrierDelay <- function(flights, year) {
  assertDataTable(flights)
  assertInt(year)
  index <- CJ(month = 1:12, carrier = flights$carrier,
              unique = TRUE)
  yr <- year
  allflights <- flights[year == yr][index, on = c("month", "carrier")][, {
    dt <- .SD
    .SD[, .(
      mean.delay = mean(arr_delay),
      mean.delay.competition = dt[!carrier, mean(arr_delay, na.rm = TRUE), on = "carrier"]
    ), by = "carrier"]
  }, by = "month"]
  ret <- allflights[index, on = c("month", "carrier")]
  setnafill(ret, fill = 0, cols = "mean.delay")
}

# This exercise concerns itself with the *same* flight data as
# `03_exercise_flights_i.R`. Read the first paragraph in that R-file for more
# information about the format. Otherwise, these exercises are independent from
# the exercises in `03_exercise_flights_i.R` and can be solved on their own.


# Write a function that returns the rows of maximum delay, for each route.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# Output:
# `data.table`: a subset of the `flights` input data, containing the rows
# that are the flights with the highest arrival delay for each route, in any
# order. If there are multiple rows in a route with delay equal to the maximum
# of that route, any single of these rows should be returned. No rows should be
# returned for a connection that was not serviced in the data.
# An example result for the `flights.data` would be
flights.maxdelays <- rbindlist(list(
  list(year = NULL, month = NULL, day = NULL, dep_delay = NULL, arr_delay = NULL, carrier = NULL, origin = NULL,
       dest = NULL, hour = NULL),
  list(2014,        12,           28,         1165,             1187,             "AA",           "DEN",
       "DFW",       13),
  list(2014,        8,            11,         883,              877,              "F9",           "DFW",
       "DEN",       6),
  list(2014,        10,           20,         1529,             1524,             "AA",           "ATL",
       "DFW",       15),
  list(2014,        6,            12,         855,              842,              "DL",           "DFW",
       "ATL",       7),
  list(2014,        11,           12,         854,              890,              "AA",           "DEN",
       "ORD",       18),
  list(2014,        6,            21,         435,              433,              "AA",           "ORD",
       "DEN",       14),
  list(2014,        8,            1,          710,              712,              "AA",           "DFW",
       "ORD",       22),
  list(2014,        6,            18,         429,              422,              "AA",           "ORD",
       "DFW",       18),
  list(2014,        1,            3,          559,              558,              "DL",           "DEN",
       "ATL",       9),
  list(2014,        8,            23,         508,              501,              "OO",           "ATL",
       "ORD",       16),
  list(2014,        6,            18,         1092,             1107,             "DL",           "ORD",
       "ATL",       19),
  list(2014,        1,            29,         814,              840,              "F9",           "ATL",
       "DEN",       8)
))
# Note how every pair of airports occurs in both orders.
ex01MaxDelay <- function(flights) {
  assertDataTable(flights)
  flights[flights[, .I[which.max(arr_delay)], by = c("origin", "dest")]$V1]
}
# Write a function that returns the rows of *median* delay.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# Output:
# `data.table`: a subset of the `flights` input data, similar to `ex02MaxDelay`.
# However, the rows should be the rows with *median* (arrival) delay for each
# route, with the lower value selected if there is an even number of rows
# for that route. This corresponds to the `quantile(vect, .5, type = 1)`
# In case of ties, the *chronologically earliest* row
# should be returned. Note this exercise also considers 0-delay flight in the
# median.
# E.g. if a route had delays `c(0, 4, 2, 4, 2, 5)` in chronological order, then
# the `median()` would be `3`, but the lower end of the .5-quantile is 2, so the
# 3rd row is returned (tie breaking the two `2`s with earliest).
# If there is no entry for a route in the database, no row for that route should
# be returned.
# An example result for the `flights.data` would be
flights.mediandelays <- rbindlist(list(
  list(year = NULL, month = NULL, day = NULL, dep_delay = NULL, arr_delay = NULL, carrier = NULL, origin = NULL,
       dest = NULL, hour = NULL),
  list(2014,        1,            1,          0,                0,                "DL",           "DEN",
       "ATL",       1),
  list(2014,        1,            1,          4,                2,                "AA",           "DFW",
       "ORD",       9),
  list(2014,        1,            1,          0,                0,                "AA",           "DEN",
       "DFW",       6),
  list(2014,        1,            3,          16,               1,                "AA",           "ORD",
       "DFW",       8),
  list(2014,        1,            3,          0,                0,                "DL",           "ORD",
       "ATL",       6),
  list(2014,        1,            2,          0,                2,                "UA",           "DEN",
       "ORD",       20),
  list(2014,        1,            1,          0,                0,                "AA",           "DFW",
       "DEN",       7),
  list(2014,        1,            1,          0,                0,                "AA",           "ATL",
       "DFW",       9),
  list(2014,        1,            1,          0,                0,                "DL",           "DFW",
       "ATL",       7),
  list(2014,        1,            1,          0,                0,                "DL",           "ATL",
       "ORD",       9),
  list(2014,        1,            5,          1,                3,                "UA",           "ORD",
       "DEN",       12),
  list(2014,        1,            1,          0,                0,                "DL",           "ATL",
       "DEN",       12)
))
# Note how many medians are 0 and because of the chronological tie breaking the
# flights are early in the dataset's range.
ex02MedianDelay <- function(flights) {
  assertDataTable(flights)
  flights <- flights[order(year, month, day, hour)]
  flights[flights[, .I[arr_delay == quantile(arr_delay, .5, type = 1)][[1]],
                  by = c("origin", "dest")]$V1]
}
# You are doing market research on the profitability of restaurants and shops at
# airport departure levels. You assume that the number of sales at a shop is
# proportional to the number of flights leaving from the airport, given that the
# shop is open at that time.
# Write a function that lists, for each month in the flights data in
# in chronological order, the number of flights that departed from a given
# airport during a given set of opening times.
# Input:
# - `flights`: a `data.table` in the format of the example `flights.data` given
# - `airport`: a `character(1)` indicating an airport of departure to consider.
# - `opening.times`: a `data.table` with columns `open`, `close`, with rows
#   indicating periods of time during which a given shop is open. Both columns
#   are integer numerics between 0 and 24 inclusive; a shop is open during
#   midnight if `close` is a number smaller or equal to `open`. An example input
#   for a shop that is open for flights leaving at 8, as well as for flights
#   that leave 14-17 and 20-3 (through midnight) would be (note that the upper
#   bounds are exclusive, since a shop that closes at 9:00 will not catch customers
#   from flights that leave at 9:<something>):
openings <- rbindlist(list(
  list(open = NULL, close = NULL),
  list(20,          5),
  list(14,          19),
  list(8,           9)
))
#   For this example, a flight that departed at `hour == 4` should be counted,
#   just as a flight that left at `hour == 8`, but a flight
#   leaving at `hour == 7` or `hour == 5` does not count.
#   You can rely on opening times never overlapping.
#   Ignore `dep_delay` (since we don't know whether the passengers spent the
#   delay time inside a plane, in a queue in the boarding area, etc.).
# Output:
# `data.table`: a table with columns `year`, `month`, `count`, indicating the
# number of flights that left the given `airport` during the `opening.times`
# at each month in the dataset.
#
# If `airport` is not among the values in the `flights` `origin` column, then
# naturally the number should be 0 for all months occurring in `flights`.
#
# The dataset contains both `hour == 0` and `hour == 24` flights. These should
# be treated the same; i.e. a flight leaving at `hour == 24` should count for
# a shop open with `open == 0`.
#
# The result for the example flights data, as well as the `openings` example
# above for the departure airport `DEN` could be:
shop.activity <- rbindlist(list(
  list(year = NULL, month = NULL, count = NULL),
  list(2014,        1,            437),
  list(2014,        10,           604),
  list(2014,        11,           548),
  list(2014,        12,           584),
  list(2014,        2,            395),
  list(2014,        3,            528),
  list(2014,        4,            460),
  list(2014,        5,            498),
  list(2014,        6,            508),
  list(2014,        7,            554),
  list(2014,        8,            540),
  list(2014,        9,            535)
))
#
# Hint: use the `inrange()` function (or `%inrange%` operator); You will have to
# modify the `opening.times` list to split times that go through midnight
# into two rows, i.e. `open == 20, close == 4` --> `open == 20, close == 24` and
# `open == 0, close == 4`.
ex03ShopActivity <- function(flights, airport, opening.times) {
  assertDataTable(flights)
  assertDataTable(opening.times)
  assertString(airport)
  opening.times[open == 24, open := 0]
  opening.times[close == 0, close := 24]
  flights[hour == 24, hour := 0]
  opening.times <- rbind(opening.times[open < close],
                         opening.times[open >= close, .(open, close = 24)],
                         opening.times[open >= close, .(open = 0, close)])
  opening.times[, close := close - 1]
  flights[, .(count = sum(origin == airport & hour %inrange% opening.times)), by = c("year", "month")]
}

# This exercise concerns itself with bank account transactions.
#
# You are asked to assist a small bank that keeps its record of financial
# transactions (a "ledger") in a tabular format. The table contains one row for
# each transfer of money from a certain account to another account. The table
# has columns for the date of the transfer (`yr`, `mth`, and `day`), the account
# from which a transfer is made (`src`), and where the money is transferred to
# (`dst`). Finally, a column for the transferred amount (`amt`) exists.
#
# Customers may also deposit money to their bank-account, in which case the bank
# records a transaction from the fictional DEPOSIT account. Customers may also
# withdraw money, in which case a transaction to the fictional WITHDRAW account
# is recorded.
#
# The table is not necessarily stored in chronological order.
#
# An example ledger could look like the following:
bank.ledger <- rbindlist(list(
  list(yr = NULL, mth = NULL, day = NULL, src = NULL, dst = NULL, amt = NULL),
  list(2008,      12,         7,          "DEPOSIT",  "Helmut",   19),
  list(2007,      8,          22,         "DEPOSIT",  "Agathe",   21),
  list(2011,      2,          1,          "Helmut",   "Annabel",  4),
  list(2011,      2,          1,          "Helmut",   "Agathe",   5),
  list(2011,      2,          4,          "Agathe",   "Annabel",  3),
  list(2008,      12,         8,          "Helmut",   "WITHDRAW", 10),
  list(2012,      1,          30,         "Agathe",   "Annabel",  23),
  list(2011,      4,          4,          "DEPOSIT",  "Erwin",    10),
  list(2011,      3,          4,          "DEPOSIT",  "Helmut",   1)
))

# Write a function that brings the table in chronological order.
# Input:
# - `ledger`: the bank's ledger in the format described above.
# Output should be a `data.table` with the rows of `ledger` in chronological
# order, starting with the oldest entry. The output with the example above
# should look like the following:
bank.ledger.sorted <- rbindlist(list(
  list(yr = NULL, mth = NULL, day = NULL, src = NULL, dst = NULL, amt = NULL),
  list(2007,      8,          22,         "DEPOSIT",  "Agathe",   21),
  list(2008,      12,         7,          "DEPOSIT",  "Helmut",   19),
  list(2008,      12,         8,          "Helmut",   "WITHDRAW", 10),
  list(2011,      2,          1,          "Helmut",   "Annabel",  4),
  list(2011,      2,          1,          "Helmut",   "Agathe",   5),
  list(2011,      2,          4,          "Agathe",   "Annabel",  3),
  list(2011,      3,          4,          "DEPOSIT",  "Helmut",   1),
  list(2011,      4,          4,          "DEPOSIT",  "Erwin",    10),
  list(2012,      1,          30,         "Agathe",   "Annabel",  23)
))
#
# You could use the `ISOdate()` function, but you may learn more if you try this
# exercise without it.
ex01SortLedger <- function(ledger) {
  assertDataTable(ledger)
  ledger[order(yr, mth, day)]
}
# Write a function that returns all transactions performed before a given date.
# Input:
# - `ledger`: the bank's ledger in the format described above.
# - `year`: integer `numeric(1)`
# - `month`: integer `numeric(1)` between 1 and 12 (inclusive)
# - `day`: integer `numeric(1)` between 1 and 31 (inclusive)
#   (it is not necessary to check whether `day` is valid within `month`. E.g.
#    `month = 2, day = 30` can be ignored, but `month = 2, day = 100` should
#    give an error.)
# Output should be a `data.table` with all rows of `ledger` that occur *before*
# the given date, in any order.
# An example result for the call
# `ex02TrimLedger(bank.ledger, 2008, 12, 8)` would be
bank.ledger.trimmed <- rbindlist(list(
  list(yr = NULL, mth = NULL, day = NULL, src = NULL, dst = NULL, amt = NULL),
  list(2008,      12,         7,          "DEPOSIT",  "Helmut",   19),
  list(2007,      8,          22,         "DEPOSIT",  "Agathe",   21)
))
# (note the entry from 2008-12-08 is *excluded*).
# The result should be an empty `data.table` with appropriate columns and types
# when no entry occurs before the given date.
# An easy solution for this exercise is to use `ISOdate` both for the
# function arguments specifying the time, as well as the date columns in the
# table, and select columns based on this.
#
# (A problem may be that the function argument `day` and the `data.table` column
# `day` have the same name. The easiest workaround here is to use `..day`, or
# to rename the local `day` variable or use it outside the `[ ]` statement. If you
# are interested, you may look at these other solutions:
# https://stackoverflow.com/a/21664128, https://stackoverflow.com/a/58358092 )
ex02TrimLedger <- function(ledger, year, month, day) {
  assertDataTable(ledger)
  assertInt(year)
  assertInt(month, lower = 1, upper = 12)
  assertInt(day, lower = 1, upper = 31)
  cutoff <- ISOdate(year, month, day)
  ledger[cutoff > ISOdate(yr, mth, day)]
}

# You are asked to write a simple account balance reporting interface that
# returns the balance of given accounts at the end of given dates. The function
# will be called with the bank's ledger, as well as a `query` table that details
# which accounts and dates are requested. It could, for example, look like the
# following:
bank.query <- rbindlist(list(
  list(yr = NULL, mth = NULL, day = NULL, account = NULL),
  list(2008,      12,         6,          "Helmut"),
  list(2008,      12,         7,          "Helmut"),
  list(2019,      2,          3,          "Agathe"),
  list(2011,      2,          3,          "Agathe"),
  list(2008,      12,         6,          "Balthasar")
))
# Note that entries are not necessarily in chronological order. Accounts that
# have no transactions at the given time, or that are not mentioned at all in
# the ledger, have a balance of 0.
#
# Input:
# - `ledger`: the bank's ledger in the format described above.
# - `query`: a query in the format shown above.
# Return: return the `query` table with an added column with rows in their
# original order with an added column `balance`. The result for the input
# given above would be
bank.balance <- rbindlist(list(
  list(yr = NULL, mth = NULL, day = NULL, account = NULL, balance = NULL),
  list(2008,      12,         6,          "Helmut",       0),
  list(2008,      12,         7,          "Helmut",       19),
  list(2019,      2,          3,          "Agathe",       0),
  list(2011,      2,          3,          "Agathe",       26),
  list(2008,      12,         6,          "Balthasar",    0)
))
# You could make use of `ex02TrimLedger()` here and do some extra steps to
# calculate the result for each query row in a loop. You are, however,
# encouraged to use merging and aggregation here: Look up "non-equi joins"
# or "inequality joins".
# (You would probably need to use `ISOdate()` for this.)
# A third (and a bit more challenging) way is to create a table of account
# balances after each transaction and use rolling joins:
# https://www.gormanalysis.com/blog/r-data-table-rolling-joins/
ex03Balance <- function(ledger, query) {
  assertDataTable(ledger)
  assertDataTable(query)
  ledger <- copy(ledger)[, `:=`(date = ISOdate(yr, mth, day),
                                yr = NULL, mth = NULL, day = NULL)]
  query <- copy(query)[, date := ISOdate(yr, mth, day)]
  totals <- query[, .(
    recv = ledger[.SD, sum(amt, na.rm = TRUE), on = .(dst = account, date <= date)],
    sent = ledger[.SD, sum(amt, na.rm = TRUE), on = .(src = account, date <= date)]
  ), by = seq_len(nrow(query))]
  cbind(query, balance = totals[, recv - sent])[, date := NULL]
}
# The bank wants to analyse the usage that their system sees over time. Write a
# function that calculates the number of *unique users* of their system, as
# indicated by the transactions, for each month.
# The user who originates a transaction is identified by the `src` column,
# *unless* it is `DEPOSIT`, in which case the `dst` column identifies the user.
#
# Input:
# - `ledger`: the bank's ledger in the format described above.
# - `year`: integer `numeric(1)`. The year for which to generate the analytics.
# Output should be a `data.table` with 12 rows for each month, with the columns
# `month` (counting from 1 to 12), and `users`, indicating the number of unique
# users in that month. An example return for the call
# `ex04MonthlyUsers(bank.ledger, 2011)` could look like this:
bank.users <- rbindlist(list(
  list(month = NULL, users = NULL),
  list(1,            0),
  list(2,            2),
  list(3,            1),
  list(4,            1),
  list(5,            0),
  list(6,            0),
  list(7,            0),
  list(8,            0),
  list(9,            0),
  list(10,           0),
  list(11,           0),
  list(12,           0)
))
# The two unique users in February are `"Helmut"` and `"Agathe"` (both counted
# once even though `"Helmut"` originates two transactions), and the users in
# March and April are `"Helmut"` and `"Erwin"`, who both make a deposit.
# Note that `"Helmut"` is counted as a unique user every month that he makes a
# transaction, but only counted at most once per month.
ex04MonthlyUsers <- function(ledger, year) {
  assertDataTable(ledger)
  assertInt(year, tol = 1e-100)
  ledger[, user := ifelse(src == "DEPOSIT", dst, src)]
  ret <- ledger[yr == year, .(users = length(unique(user))),
                keyby = .(month = mth)][J(1:12)]
  setnafill(ret, fill = 0, cols = "users")[]
}
# Furthering the analysis, the bank wants to measure the daily system
# utilization, i.e. the number of transactions per day. However, to smooth out
# the numbers, a *ten day rolling average* should be applied, i.e. for each
# day, the average number of transactions per day for the last 10 days should
# be reported.
#
# Input:
# - `ledger`: the bank's ledger in the format described above
# - `year`: integer `numeric(1)`. The year for which to generate analytics.
# - `month`: integer `numeric(1)` between 1 and 12 inclusive. The month for
#   which to generate analytics.
# Output should be a `data.table` with columns `yr`, `mth`, `day`,
# `transactions`. `yr` and `mth` should be the `year` and `month` argument
# given to the function, `day` should count up the days of the month, and
# `transactions` should be the 10-day rolling average of the number of
# transactions up to (and including) the day listed. (Note that this average
# should consider the transactions done in the previous month, where applicable,
# i.e. the 1st entry is the average of the 1st of that month and 9 days from
# the preceding month etc.)
# An example result for the call `ex05Transactions(bank.ledger, 2011, 2)`
# would be:
bank.daily.users <- rbindlist(list(
  list(yr = NULL, mth = NULL, day = NULL, transactions = NULL),
  list(2011,      2,          1,          0.2),
  list(2011,      2,          2,          0.2),
  list(2011,      2,          3,          0.2),
  list(2011,      2,          4,          0.3),
  list(2011,      2,          5,          0.3),
  list(2011,      2,          6,          0.3),
  list(2011,      2,          7,          0.3),
  list(2011,      2,          8,          0.3),
  list(2011,      2,          9,          0.3),
  list(2011,      2,          10,         0.3),
  list(2011,      2,          11,         0.1),
  list(2011,      2,          12,         0.1),
  list(2011,      2,          13,         0.1),
  list(2011,      2,          14,         0),
  list(2011,      2,          15,         0),
  list(2011,      2,          16,         0),
  list(2011,      2,          17,         0),
  list(2011,      2,          18,         0),
  list(2011,      2,          19,         0),
  list(2011,      2,          20,         0),
  list(2011,      2,          21,         0),
  list(2011,      2,          22,         0),
  list(2011,      2,          23,         0),
  list(2011,      2,          24,         0),
  list(2011,      2,          25,         0),
  list(2011,      2,          26,         0),
  list(2011,      2,          27,         0),
  list(2011,      2,          28,         0)
))
# It is probably easiest to create a table of daily transactions in the
# interesting window (including 9 days from "last month") and then use
# `frollmean()`.
# You should probably use commands to the effect of
# `date <- as.Date(ISOdate())` on the data to handle dates.
# You can directly subtract days from the date: `date - 9`, and you can
# get the last day of the month by using
# `as.Date(ISOdate(year, month + 1, 1)) - 1`. (Of course there are also
# other ways of handling this, in particular `data.table`'s own `IDate`
# class). Possibly also helpful: `?seq.Date`, and the `data.table::year()`,
# `data.table::month()` and `data.table::mday()` functions that extract parts
# of dates.
ex05Transactions <- function(ledger, year, month) {
  assertDataTable(ledger)
  assertInt(year)
  assertInt(month, lower = 1, upper = 12)
  begin <- as.Date(ISOdate(year, month, 1)) - 9
  end <- if (month == 12) {
    as.Date(ISOdate(year, month, 31))
  } else {
    as.Date(ISOdate(year, month + 1, 1)) - 1
  }
  dseq <- seq(begin, end, by = "day")
  template <- data.table(yr = year(dseq), mth = month(dseq), day = mday(dseq))
  res <- ledger[template, on = c("yr", "mth", "day")]
  res <- res[, .(transactions = sum(!is.na(amt))), by = c("yr", "mth", "day")]
  res[, transactions := frollmean(res$transactions, 10)]
  res[- (1:9)]
}

# This exercise tests your knowledge about *basic features of data.table*
# Basically all of these functions should contain a few checkmate assertions,
# followed by very few lines of code.
#
# Use checkmate `assertDataTable` or `assertDataFrame` to make assertions on
# data.table/data.frame arguments (depending on which one is named in the task
# description). Use `assertNames()` to check column names (as in the example
# to follow).
#
# Example: Write a function that removes rows from a data.table. Your function
# has three inputs:
# - `minuend`: a `data.table` with multiple columns, at least one of which is
#   named "index".
# - `subtrahend`: a `data.frame` or a `data.table` with exactly one column
#   named "index".
# - `do.nothing`: a scalar `logical`.
# If `do.nothing` is `TRUE`, then the `minuend` argument should be returned
# as-is. If `do.nothing` is `FALSE`, then a `data.table` should be returned
# with all rows removed that have a value of `"index"` that occurs in the
# `subtrahend` "index" column. Note that row order and duplicate rows of `minuend`
# (with index that does not occur in `subtrahend`) should be preserved.
#
# This could, for example, be solved with the following code:
demoSubtractRows <- function(minuend, subtrahend, do.nothing) {
  assertFlag(do.nothing)
  assertDataTable(minuend)
  assertNames(colnames(minuend), must.include = "index")
  assertDataFrame(subtrahend, ncol = 1)  # assertDataFrame accepts *both* data.table and data.frame, so it is ideal here
  assertNames(colnames(subtrahend), identical.to = "index")
  if (do.nothing) {
    minuend
  } else {
    minuend[!subtrahend, on = "index"]
  }
}
# If you have looked at some of the data.table material then these functions should
# be a breeze for you.


# Write a function that takes a list of named lists as input and creates a `data.table` as
# output, containing the input lists as rows.
# Inputs:
# - lst: `list` of named `lists` containing numeric scalars.
# Output: `data.table`
# The resulting `data.table` should contain the input rows in order and have columns according
# to the names of the input list (ordering of columns does not matter).
#
# Example:
# ex01List2DT(list(list(a = 1, b = 2), list(b = 3, a = 4)))
# --> data.table(a = c(1, 4), b = c(2, 3))
# Some lists do not contain elements for all columns; in that case, a 0 should be used:
# ex01List2DT(list(list(a = 1, b = 2), list(a = 4, c = 5)))
# --> data.table(a = c(1, 4), b = c(2, 0), c = c(0, 5))
# if there are elements in the lists that are not non-NA scalar numerics, an error should be thrown.
#
# You should probably use `rbindlist` and possibly `nafill` / `setnafill` here.
ex01List2DT <- function(lst) {
  assertList(lst)
  lapply(lst, function(l) {
    assertList(l, names = "unique")
    lapply(l, assertNumber)
  })
  setnafill(rbindlist(lst, use.names = TRUE, fill = TRUE), fill = 0)[]
}

# Write a function that checks whether one data.table is the row-reordered form of another.
# We call this "equivalent" here. All tables must contain an `id` column (absence of which
# is an error), the content of which should be ignored for the purpose of checking equivalence.
#
# Inputs:
# - a, b: `data.table` with atomic (non-list) columns
# Output: `logical(1)` (i.e. `TRUE` or `FALSE`)
#
# Two tables are equivalent whenever one could reorder one table's rows to get the other table,
# ignoreing the content of the `id` column.
#
# The following should be `TRUE`:
# ex02TablesEquivalent(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = c(2, 1, 2), b = c("b", "a", "b"), id = c(4, 5, 6))
# )
#
# The following should be `FALSE`:
# ex02TablesEquivalent(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = c(1, 1, 2), b = c("a", "a", "b"), id = c(4, 5, 6))
# )
#
# Tables having different column names, columns in different order or columns of the
# same name with different types are always not equivalent (even if this concerns the `id` column).
# Tables with different numbers of rows can also not be equivalent.
#
# You can use `fsetequal` or `all.equal` here.
ex02TablesEquivalent <- function(a, b) {
  assertDataTable(a, types = "atomic")
  assertNames(colnames(a), must.include = "id")
  assertDataTable(b, types = "atomic")
  assertNames(colnames(b), must.include = "id")
  isTRUE(all.equal(a[FALSE, ], b[FALSE, ])) && fsetequal(a[, id := NULL], b[, id := NULL])
}

# Write a function that returns all rows in `a` that also occur in `b`, ignoring the `id` column.
#
# Inputs:
# - a, b: `data.table` with atomic (non-list) columns
# Output: `data.table`
#
# Similarly to ex02TablesEquivalent, `a` and `b` contain an `id` column that should be ignored for
# the purpose of checking whether rows are the same (but the `id` of the table `a` should be included
# in the output!).
# Also, just like in ex02TablesEquivalent, different column names, order, or types (even regarding
# the `id` col) mean no rows are equal (and an empty version of `a`, e.g. `a[FALSE, ]` should be returned).
#
# Example:
# ex03TablesIntersect(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = 2, b = "b", id = 10)
# )
# --> data.table(a = c(2, 2), b = c("b", "b"), id = c(2, 3))  # returning rows 2 and 3 of `a`, since they
#                                                             # are also found in `b`
#
# This can be solved using a "join", using `merge()` or `[... on = ...]`. Be careful that, when using
# the letters `a` and `b` within `[ ... ]`, you know whether they reference the function arguments `a`/`b`,
# or the columns within the table that could also be `a` or `b`.
ex03TablesIntersect <- function(a, b) {
  assertDataTable(a, types = "atomic")
  assertNames(colnames(a), must.include = "id")
  assertDataTable(b, types = "atomic")
  assertNames(colnames(b), must.include = "id")
  if (!isTRUE(all.equal(a[FALSE, ], b[FALSE, ]))) {
    return(a[FALSE, ])
  }
  b[, id := NULL]
  b <- unique(b)
  a[b, on = colnames(b)]
}


# Write a function that reads in a `data.table` from a file.
#
# Inputs:
# - `fname`: file name (relative to the repository base directory)
# Output: `data.table`
#
# The file format is: The first three rows of the file contain
# comments and should be skipped. They are followed by a row of
# column names, and then data. Columns are separated by semicolon (`;`) and
# should be read either as `numeric` or `character` columns, as appropriate.
# A string `MISSING` indicates the value should be NA.
# If a column data contains at least one value with at least one comma (`,`), this
# column should be a list column with character vectors of possibly variable
# length as values.
#
# Consider the `example.csv` file, which should give rise to the following
# behaviour:
# ex04ReadTable(file.path("R", "example.csv"))
# -->
# data.table(
#   species = c("cat", "dog", "eldritch"),
#   names = list(c("Felix", "Sir Cattington", "Larry"), c("Maggie", "Snuffles", "Klaus"),
#     c("Yaldabaoth", "Behemoth", "The Other")),
#   ages = list(c("3", "7", "4"), c("1", "4", "3"), c("10234", "5231", "13792168024")),
#   "food, class, if known" = c("Cat Food", "Dog Food", NA),
#   "cost, approximate, if known" = c(100, 250, NA)
# )
#
# Note the `names` and `ages` columns contain lists of characters only, while the `cost` column is numeric.
#
# Here you should use `fread` but you also need to do some post-processing.
ex04ReadTable <- function(fname) {
  assertString(fname)
  dt <- fread(fname, sep = ";", na.string = "MISSING", skip = 3)
  cols <- colnames(dt)[vapply(dt, function(x) any(grepl(",", x)), logical(1))]
  if (length(cols)) {
    dt[, (cols) := lapply(.SD, strsplit, split = ","), .SDcols = cols]
  }
  dt
}
# This exercise concerns itself with data in a certain format.
#
# Setting: You are a data science consultant, hired to help the WidgetCorp (R) corporation
# analyse its production facilities for Widgets (TM). WidgetCorp (R) has a number of
# Widget (TM) production machines that are lovingly called Machine01, Machine02, Machine03, etc.
# Each of these machines has the same set of Sensors, measuring temperature, humidity,
# electricity consumption, water inflow rate, water purity, and various other metrics.
# Because the particulars of the quantities being measured are not relevant, these sensors
# are just enumerated as sensor01, sensor02, sensor03, etc.
# You are given a dataset that records, for each produced Widget (TM), (1) the Machine that
# produced it, (2) the quality of the Widget (TM), on a scale from 0 to 100, as assessed by
# certified Widget (TM) Quality Assessment Professionals, and (3) the values of the various
# sensor readings when the Widget (TM) was produced.
# However, the sensors have a limited measurement range that sometimes gets undercut. E.g.
# if the electricity consumption is below a certain threshold, the sensor is not able to
# record that value and just records NA.
# Example datasets could look like the following.
# (Note the `rbindlist`-layout is only for readability here. You can run the following code and
# will get a proper `data.table` from it.)
widget.corp.data <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine01",    78,             23,              28.6,            -23,             NA),
  list("Machine02",    28,             41,              77.8,            NA,              27),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine03",    80,             NA,              32.3,            NA,              NA),
  list("Machine03",    58,             10,              77.8,            3,               NA),
  list("Machine02",    74,             NA,              24.5,            -18,             3),
  list("Machine01",    46,             81,              NA,              NA,              NA),
  list("Machine01",    24,             43,              13.3,            -22,             NA),
  list("Machine02",    7,              96,              96.0,            0,               NA),
  list("Machine01",    22,             107,             23.5,            7,               8),
  list("Machine03",    98,             NA,              NA,              11,              NA)
))
# In the following tasks you should write functions that handle data just like this. However, you may get
# a dataset with more or fewer machines, and with more or fewer sensors.

# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.

# Hint:
# You will probably write some functions that modify the input data in-place. This means that calling
# one of your functions may actually cause the input value to be different after the call. Be aware of this
# if you try out your functions on the `widget.corp.data` example data. E.g. if you do
# > result <- ex04CleanTable(widget.corp.data)
# then `widget.corp.data` itself may have changed and could give different results in future experiments.
# You should therefore always run your functions on a `copy` of the input data, like so:
# > result <- ex04CleanTable(copy(widget.corp.data))
# Alternatively, you can just execute the above code-snippet (or source this .R-file) again after
# each experiment.

# ** Warm up **
# Write a function that accepts one `data.table` argument `data` and sorts the given data according to
# the number of missing values in the `sensorXX` columns, in descending order. Ties should be broken
# by the `quality`, descending. Your function should return the `data.table` with rows sorted.
# The output for the example data would therefore be:
widget.corp.data.sorted <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine03",    98,             NA,              NA,              11,              NA),
  list("Machine03",    80,             NA,              32.3,            NA,              NA),
  list("Machine01",    46,             81,              NA,              NA,              NA),
  list("Machine01",    78,             23,              28.6,            -23,             NA),
  list("Machine02",    74,             NA,              24.5,            -18,             3),
  list("Machine03",    58,             10,              77.8,            3,               NA),
  list("Machine02",    28,             41,              77.8,            NA,              27),
  list("Machine01",    24,             43,              13.3,            -22,             NA),
  list("Machine02",    7,              96,              96.0,            0,               NA),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine01",    22,             107,             23.5,            7,               8)
))
ex01SortTable <- function(data) {
  assertDataTable(data)
  missings <- data[, rowSums(is.na(.SD)), .SDcols = grep("^sensor", colnames(data), value = TRUE)]
  data[order(missings, quality, decreasing = TRUE)]
}

# ** Warm up **
# Write a function that accepts one `data.table` argument `data` and collects all the sensor data into
# a single list column. This means, the resulting `data.table` should have columns `machine`, `quality`,
# `sensor`, and the `sensor` column should be a `list` column containing a named `numeric` vector
# containing all the non-`NA` sensor values with appropriate names. The order of the table should not be changed.
# The output for the example data would therefore be:
widget.corp.data.list <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor = NULL),
  list("Machine01",    78,             list(c(sensor01 = 23, sensor02 = 28.6, sensor03 = -23))),
  list("Machine02",    28,             list(c(sensor01 = 41, sensor02 = 77.8, sensor04 = 27))),
  list("Machine03",    32,             list(c(sensor01 = 57, sensor02 = 91.6, sensor03 = -29, sensor04 = 10))),
  list("Machine03",    80,             list(c(sensor02 = 32.3))),
  list("Machine03",    58,             list(c(sensor01 = 10, sensor02 = 77.8, sensor03 = 3))),
  list("Machine02",    74,             list(c(sensor02 = 24.5, sensor03 = -18, sensor04 = 3))),
  list("Machine01",    46,             list(c(sensor01 = 81))),
  list("Machine01",    24,             list(c(sensor01 = 43, sensor02 = 13.3, sensor03 = -22))),
  list("Machine02",    7,              list(c(sensor01 = 96, sensor02 = 96.0, sensor03 = 0))),
  list("Machine01",    22,             list(c(sensor01 = 107, sensor02 = 23.5, sensor03 = 7, sensor04 = 8))),
  list("Machine03",    98,             list(c(sensor03 = 11)))
))
# (Just to make clear what a `list` column is -- another way to write this would be the following:)
widget.corp.data.list <- data.table(
  machine = c("Machine01", "Machine02", "Machine03", "Machine03", "Machine03", "Machine02", "Machine01",
              "Machine01", "Machine02", "Machine01", "Machine03"),
  quality = c(78, 28, 32, 80, 58, 74, 46, 24, 7, 22, 98),
  sensor = list(
    c(sensor01 = 23, sensor02 = 28.6, sensor03 = -23),
    c(sensor01 = 41, sensor02 = 77.8, sensor04 = 27),
    c(sensor01 = 57, sensor02 = 91.6, sensor03 = -29, sensor04 = 10),
    c(sensor02 = 32.3),
    c(sensor01 = 10, sensor02 = 77.8, sensor03 = 3),
    c(sensor02 = 24.5, sensor03 = -18, sensor04 = 3),
    c(sensor01 = 81),
    c(sensor01 = 43, sensor02 = 13.3, sensor03 = -22),
    c(sensor01 = 96, sensor02 = 96, sensor03 = 0),
    c(sensor01 = 107, sensor02 = 23.5, sensor03 = 7, sensor04 = 8),
    c(sensor03 = 11)
  )
)
# Be aware that some rows may not have any non-missing sensor data, in which case the `sensor` value for the
# corresponding result row should be an empty `numeric`.
ex02ListTable <- function(data) {
  assertDataTable(data)
  sensorcols <- grep("^sensor[0-9]+", colnames(data), value = TRUE)
  data[, sensor := list(list(c(na.omit(unlist(.SD))))), .SDcols = sensorcols, by = seq_len(nrow(data))]
  data[, c("machine", "quality", "sensor")]
}

# ** Actual analysis **
# You want to analyse the effect of measured sensor data on the quality of widgets, for each machine
# separately. However, you are worried that too much missing data will bring on misleading results.
# Therefore, you plan to remove columns with too much missing data.
#
# Write a function that removes all `sensorXX` columns which have
# 50% or more missing data for at least one machine. Your function should accept one `data.table` argument
# `data` and return the modified `data.table`. For the example `widget.corp.data` dataset,
# that would be `sensor01` (50% missing for "Machine03") and `sensor04` (75% missing for
# both "Machine01" and "Machine03"). The resulting dataset would therefore be
widget.corp.data.fsel <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor02 = NULL, sensor03 = NULL),
  list("Machine01",    78,             28.6,            -23),
  list("Machine02",    28,             77.8,            NA),
  list("Machine03",    32,             91.6,            -29),
  list("Machine03",    80,             32.3,            NA),
  list("Machine03",    58,             77.8,            3),
  list("Machine02",    74,             24.5,            -18),
  list("Machine01",    46,             NA,              NA),
  list("Machine01",    24,             13.3,            -22),
  list("Machine02",    7,              96.0,            0),
  list("Machine01",    22,             23.5,            7),
  list("Machine03",    98,             NA,              11)
))
ex03CleanTable <- function(data) {
  assertDataTable(data)
  sensorcols <- grep("^sensor[0-9]+", colnames(data), value = TRUE)
  sensor.nas <- data[, as.list(colMeans(is.na(.SD))), .SDcols = sensorcols, by = machine][, machine := NULL]
  keep <- names(sensor.nas)[vapply(sensor.nas, max, numeric(1)) < 0.5]
  data[, c("machine", "quality", keep), with = FALSE]
}

# ** Actual analysis II **
# Even after removing columns with 50% or more missing data for any machine, you are still left with
# a dataset with missing values. You don't want to remove all columns with missing data (which would,
# in most cases, leave you with no data at all), so instead you choose to "impute" missing values.
# You know that values are missing because they are below the detection threshold of a sensor.
# You don't know the detection thesholds, but you know that for each sensor they are the same on
# different machines. Therefore, you *estimate* the threshold as the *minimum non-missing value* for each sensor.
#
# Write a function that imputes all missing values for sensor columns as the minimum of that column.
# The function takes one `data.table` argument `data` and should return the modified `data.table`.
# You can rely on at least one non-missing value being present in each sensor-column. The result
# for the `widget.corp.data` example data would be
widget.corp.data.imputed <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine01",    78,             23,              28.6,            -23,             3),
  list("Machine02",    28,             41,              77.8,            -29,              27),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine03",    80,             10,              32.3,            -29,             3),
  list("Machine03",    58,             10,              77.8,            3,               3),
  list("Machine02",    74,             10,              24.5,            -18,             3),
  list("Machine01",    46,             81,              13.3,            -29,             3),
  list("Machine01",    24,             43,              13.3,            -22,             3),
  list("Machine02",    7,              96,              96.0,            0,               3),
  list("Machine01",    22,             107,             23.5,            7,               8),
  list("Machine03",    98,             10,              13.3,            11,              3)
))
ex04ImputeTable <- function(data) {
  assertDataTable(data)
  sensorcols <- grep("^sensor[0-9]+", colnames(data), value = TRUE)
  data[, (sensorcols) := lapply(.SD, function(x) nafill(x, fill = min(x, na.rm = TRUE))), .SDcols = sensorcols][]
}

# Write a function that `cbind`s two data.tables, avoiding duplicate column
# names in a certain way.
# Input:
# - table.a: `data.table`
# - table.b: `data.table`
# Output: Return a `data.table` that contains all the columns of `table.a`,
# followed by the columns of `table.b`, in order. However, should a column
# name occur in `table.b` that is already in `table.a`, then the column
# should be suffixed by `_copy`. If that *also* leads to a name collision,
# it should be suffixed instead by `_copy.1` (or `_copy.2` etc.)
# You can rely on `table.a` and `table.b` for themselves having unique column
# names, but some of their column names may *already* end on `_copy` or
# `_copy.#`.
#
# If `table.a`. and `table.b` have a different number of rows, an error
# should be thrown.
# Example input:
table.a.example <- data.table(x = 1, y = 2, y_copy = 3, y_copy.1 = 4)
table.b.example <- data.table(z = 100, x = 200, y = 300, y_copy.1 = 400, y_copy.2 = 500)
# This should give the output
table.ab.example <- data.table(x = 1, y = 2, y_copy = 3, y_copy.1 = 4,
                               z = 100, x_copy = 200, y_copy.3 = 300, y_copy.1_copy = 400, y_copy.2 = 500)
# (note that `y` gets renamed to `y_copy.3` because `_copy`, `_copy.1` and
# `_copy.2` are already taken; `y_copy.1` of `table.b` turns into
# `y_copy.1_copy`, because `y_copy.1` is already present in `table.a`.)
ex01CbindNameClash <- function(table.a, table.b) {
  assertDataTable(table.a)
  assertDataTable(table.b)
  if (nrow(table.a) != nrow(table.b)) {
    stop("Different number of rows")
  }
  allnames <- union(colnames(table.a), colnames(table.b))
  renames <- vapply(colnames(table.b), function(name) {
    if (!name %in% colnames(table.a)) {
      return(name)
    }
    basename <- name <- paste0(name, "_copy")
    counter <- 1
    while (name %in% allnames) {
      name <- paste0(basename, ".", counter)
      counter <- counter + 1
    }
    name
  }, character(1))
  colnames(table.b) <- renames  # could also use setnames() here
  cbind(table.a, table.b)
}
# Write a function that removes duplicate entries according to some columns.
# Input:
# - `table`: a `data.table` with the columns `year`, `month`, `day`, and
#   arbitrarily many more columns additionally to that.
# Output: The input `data.table` where duplicate entries according to the
# `year`, `month`, `day` columns are removed. I.e. for each such indicated date,
# the resulting table should only contain the *last* line with that date.
# The lines that are not removed should remain in order.
dup.table <- rbindlist(list(
  list(year = NULL, month = NULL, day = NULL, reference = NULL, id = NULL),
  list(2009,        4,            13,         "ae8f43b4b8",     6054),
  list(2009,        4,            14,         "e0e57942dd",     3453),
  list(2009,        4,            13,         "d63a61d9fc",     1470)
))
# Here the first line should be skipped because the third line has an identical
# date:
deduped.table <- rbindlist(list(
  list(year = NULL, month = NULL, day = NULL, reference = NULL, id = NULL),
  list(2009,        4,            14,         "e0e57942dd",     3453),
  list(2009,        4,            13,         "d63a61d9fc",     1470)
))

ex02DedupTable <- function(table) {
  assertDataTable(table)
  unique(table, by = c("year", "month", "day"), fromLast = TRUE)
}


# You are looking for a new flat. For this, you query a database for monthly
# rent prices of available flats, which returns data in the following format:
flat.prices <- rbindlist(list(
  list(address = NULL, prices = NULL),
  list("134 Charles St", list(list(c(2340, 2193), NULL, 4023, NULL, NULL, c(10234, 9203)))),
  list("12 East 69th St", list(list(2493, NULL, NULL, NULL))),
  list("2 Park Pl", list(list(NULL, NULL, 1924, 3921))),
  list("172 Madison Ave", list(list(NULL, NULL))),
  list("25 Columbus Circle", list(list(10234)))
))
# This lists the adresses where the agency manages flats, and for each address
# the column `prices` lists the flat prices. This column contains a list for
# each address, where the first entry lists all prices of all available flats
# on the first floor, the second entry lists all prices of available flats on
# the second floor etc. In this example, the `"134 Charles St"` building has
# two flats available on the ground floor (one for 2340, one for 2193), one flat
# for 4023 on the second floor, and two flats (10234 and 9203) on the fifth
# floor.
#
# You plan to look at all available addresses individually, but within each
# address you only consider the *cheapest flat that is not on the ground floor*.
# You therefore need to write a function that lists the address, the price of
# the cheapest apartment that is not on the ground floor, and the floor of that
# aparment. If there are no apartments available that are not on the ground
# floor, the address should be absent. The result for the input above should
# therefore be
flat.choices <- rbindlist(list(
  list(address = NULL, price = NULL, floor = NULL),
  list("134 Charles St", 4023, 2),
  list("2 Park Pl", 1924, 2)
))
# Input:
# - `prices`: a `data.table` with columns `address`, `prices`, as described
#   above
# Output:
# A `data.table with columns `address`, `price`, `floor`, as shown above,
# with arbitrary order of rows.
ex03FlatPrices <- function(prices) {
  assertDataTable(prices)
  suppressWarnings(prices[, floormins := lapply(prices, function(x) vapply(x, min, numeric(1)))])
  prices <- suppressWarnings(prices[, .(
    address,
    price = vapply(floormins, function(x) min(x[-1]), numeric(1)),
    floor = vapply(floormins, function(x) which.min(c(x[-1], Inf)), numeric(1)))])
  prices[is.finite(price)]
}



# You are organising a small hackathon for R developers working on a specific
# project. Everyone should come to a place over the weekend to code. You want
# to order food and drinks for everyone, as well as reserve a hotel for those
# who don't live nearby or sleep over at friends' places. For this you have
# set up a Google Forms Survey, where those interested in coming should answer
# questions, for example about their dietary needs and whether they need a
# hotel. However, some participants may forget to input some information, or
# may change their mind about certain things later. In that case, they just
# submit a new response in the survey, updating the old information. The data
# you get could, for example, have the following format:
participants.response <- rbindlist(list(
  list(name = NULL,        coming = NULL, hotel = NULL, dietary.reqs = NULL, submission.date = NULL),
  list("Donald Knuth",     TRUE,          TRUE,         "dessert first",     3),
  list("Ross Ihaka",       NA,            TRUE,         NA,                  6),
  list("Ross Ihaka",       TRUE,          FALSE,        NA,                  4),
  list("Vladimir Vapnik",  TRUE,          FALSE,        "",                  5),
  list("Donald Knuth",     FALSE,         NA,           NA,                  5),
  list("Robert Gentleman", TRUE,          TRUE,         NA,                  3),
  list("Ross Ihaka",       NA,            NA,           "vegetarian",        5)
))
# Here the `submission.date` column indicates the number of days since the form
# was set up. In this example, Donald Knuth at first signed up to come but then
# later had to cancel. Ross Ihaka at first did not provide information
# about his dietary requirements, which he then set to `"vegetarian"`. He
# furthermore updated that he did want to stay at a hotel after all.
# Robert Gentleman and Vladimir Vapnik both indicated they would be coming and
# provided information, although Robert Gentleman's dietary requirements stay
# unknown.
# (*This data is made up. Please do not use the dietary requirement data if you
# are actually hosting someone from this list. Please email me if you happen
# to know the dietary requirements of someone on this list)
#
# Write a function that takes data of this form and constructs the final
# response for each participant, by taking the last value, according to the
# submission date that is not missing.
# Input: `response`, a `data.table` with the columns `name`, `submission.date`,
# and multiple other columns of any type. The order of the columns, as well as
# the other columns present could be arbitrary.
# Return: The result `data.table` should contain all columns except the
# `submission.date` column, and the columns should be in the order they were
# given. There should be one row for each participant (according to `name`)
# with the most recently given response for each other column.
# The response for the data above could, for example, be:
participants.response.final <- rbindlist(list(
  list(name = NULL,        coming = NULL, hotel = NULL, dietary.reqs = NULL),
  list("Donald Knuth",     FALSE,         TRUE,         "dessert first"),
  list("Ross Ihaka",       TRUE,          TRUE,         "vegetarian"),
  list("Vladimir Vapnik",  TRUE,          FALSE,        ""),
  list("Robert Gentleman", TRUE,          TRUE,         NA)
))
ex04LastResponse <- function(response) {
  assertDataTable(response)
  response[order(submission.date),
           lapply(.SD, function(x) last(na.omit(x))), by = name][,
                                                                 submission.date := NULL][]
}

# You are given data about the organizational hierarchy of a small company. It
# tracks, for every employee, who their supervisor / immediate superior is.
# An example of this data would be
org.table <- rbindlist(list(
  list(name = NULL,               superior = NULL),
  list("Gottfried Ofers",         ""),
  list("Kunigunde von Lauerberg", "Gottfried Ofers"),
  list("Agata Weyen",             "Kaspar Vinken"),
  list("Kaspar Vinken",           "Gottfried Ofers")
))
# (The CEO has his superior listed as `""`)
# The company needs to track which employee is supervising other employees for
# accounting purposes.
# Write a function that adds a `logical` column `has.inferior` which is `TRUE`
# for every employee that has inferiors.
# Input: `org`, a `data.table` as in the format above
# Output: The `data.table` with an additional column `has.inferior`. The example
# above should return
org.table.augmented <- rbindlist(list(
  list(name = NULL,               superior = NULL,   has.inferior = NULL),
  list("Gottfried Ofers",         "",                TRUE),
  list("Kunigunde von Lauerberg", "Gottfried Ofers", FALSE),
  list("Kaspar Vinken",           "Gottfried Ofers", TRUE),
  list("Agata Weyen",             "Kaspar Vinken",   FALSE)
))
ex05OrgHasInf <- function(org) {
  assertDataTable(org)
  org[, has.inferior := name %in% org$superior][]
}
# This exercise concerns itself with data in a certain format.
#
# You are advising the education board of a city on grading outcomes. They collect data
# on student's grades and need to put the data into different formats.
#
# The data they collect is a listing of students. For each student, they have a unique `student.id` (a random
# but unique character string which anonymizes the student's name), the student's `school`, and
# a `list` column `grades` containing named `numeric` elements indicating the grades which the student
# got during the last semester. The names are the subjects, and the values the grades ranging
# from 1 ("Very Good") to 6 ("Insufficient").
#
# An example dataset could look like the following. (See the notes in 02_exercise_sensordata.R
# on the format and other hints).
student.grade.data <- rbindlist(list(
  list(student.id = NULL, school = NULL,       grades = NULL),
  list("xzqk",            "North High School", list(c(maths = 2.0, physics = 1.3, chemistry = 1.7))),
  list("hffi",            "South Academy",     list(c(maths = 2.3, physics = 1.0, biology = 2.0, compsci = 1.0))),
  list("hwdp",            "South Academy",     list(c(maths = 3.3, chemistry = 3.0, biology = 2.7, compsci = 1.7))),
  list("flzb",            "North High School", list(c(maths = 1.7, physics = 2.0, chemistry = 2.0, biology = 2.0))),
  list("txyb",            "North High School", list(c(chemistry = 1.0, biology = 1.3))),
  list("fjea",            "South Academy",     list(c(chemistry = 2.0, physics = 2.3, compsci = 1.0)))
))

# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.


# To be able to analyze the data better you should start by bringing them into "long format".
# You want a table that has the columns `student.id`, `school`, `subject`, and `grade`,
# where there are multiple lines for each student (one for each subject in which he or she was
# graded).
#
# Write a function that brings the input data into "long format". The function should have
# an argument `data` (a `data.table`) and return a `data.table` in long format.
# A possible result of the data above would be:
student.grade.data.long <- rbindlist(list(
  list(student.id = NULL, school = NULL,       subject = NULL, grade = NULL),
  list("xzqk",            "North High School", "maths",        2.0),
  list("xzqk",            "North High School", "physics",      1.3),
  list("xzqk",            "North High School", "chemistry",    1.7),
  list("hffi",            "South Academy",     "maths",        2.3),
  list("hffi",            "South Academy",     "physics",      1.0),
  list("hffi",            "South Academy",     "biology",      2.0),
  list("hffi",            "South Academy",     "compsci",      1.0),
  list("hwdp",            "South Academy",     "maths",        3.3),
  list("hwdp",            "South Academy",     "chemistry",    3.0),
  list("hwdp",            "South Academy",     "biology",      2.7),
  list("hwdp",            "South Academy",     "compsci",      1.7),
  list("flzb",            "North High School", "maths",        1.7),
  list("flzb",            "North High School", "physics",      2.0),
  list("flzb",            "North High School", "chemistry",    2.0),
  list("flzb",            "North High School", "biology",      2.0),
  list("txyb",            "North High School", "chemistry",    1.0),
  list("txyb",            "North High School", "biology",      1.3),
  list("fjea",            "South Academy",     "chemistry",    2.0),
  list("fjea",            "South Academy",     "physics",      2.3),
  list("fjea",            "South Academy",     "compsci",      1.0)
))
# The ordering of rows does not matter.
ex01DataToLong <- function(data) {
  assertDataTable(data)
  data[, .(school = school, subject = names(grades[[1]]), grade = grades[[1]]), by = student.id]
}

# You want to present some statistics on the grade distribution.
# In particular you want to give the `mean()`, as well as the `sd()` of grades
# - within schools
# - within subjects
# - within both schools *and* subjects
#
# Write a function that accepts two arguments
# - `data`: a `data.table` in the format of `student.grade.data` at the top
# - `group.by`: a `character(1)` being either `"subject"`, `"school"`, or `"both"`.
# The return value should be a `data.table` with the columns
# - `school` (if `group.by` is either `"school"` or `"both"`)
# - `subject` (if `group.by` is either `"subject"` or `"both"`)
# - `mean`
# - `sd` (note: just like the `sd()` function, this value should be `NA` if only
#   one datapoint fro a grade is avilable.)
# The result of that function with `group.by = "school"` would be (up to 6 digits)
student.grade.data.byschool <- rbindlist(list(
  list(school = NULL,        mean = NULL, sd = NULL),
  list("North High School", 1.6666667,    0.3807887),
  list("South Academy",     2.027273,     0.8026094)
))
# The result with `group.by = "subject"` would be
student.grade.data.bysubject <- rbindlist(list(
  list(subject = NULL, mean = NULL, sd = NULL),
  list("maths",        2.325,       0.6946222),
  list("physics",      1.65,        0.6027714),
  list("chemistry",    1.94,        0.7197222),
  list("biology",      2.0,         0.5715476),
  list("compsci",      1.233333,    0.4041452)
))
# The result with `group.by = "both"` would be
student.grade.data.byboth <- rbindlist(list(
  list(school = NULL,       subject = NULL, mean = NULL, sd = NULL),
  list("North High School", "maths",        1.85,        0.2121320),
  list("North High School", "physics",      1.65,        0.4949747),
  list("North High School", "chemistry",    1.566667,    0.5131601),
  list("North High School", "biology",      1.65,        0.4949747),
  list("South Academy",     "maths",        2.8,         0.7071068),
  list("South Academy",     "physics",      1.65,        0.9192388),
  list("South Academy",     "chemistry",    2.5,         0.7071068),
  list("South Academy",     "biology",      2.35,        0.4949747),
  list("South Academy",     "compsci",      1.233333,    0.4041452)
))
# (where the ordering of rows does not matter)
#
# It is highly recommended that you make use of `ex01DataToLong()` here and then do
# aggregation with `[ ... by = ... ]`
ex02DataStats <- function(data, group.by) {
  assertDataTable(data)
  assertChoice(group.by, c("school", "subject", "both"))
  group.by <- c(if (group.by != "subject") "school", if (group.by != "school") "subject")
  ex01DataToLong(data)[, .(mean = mean(grade), sd = sd(grade)), by = group.by]
}

# You want to look at the number of students taught by each school in each subject.
#
# Write a function that takes a `data.table` argument `data` and returns a `data.table`
# with the columns `school`, `subject`, `students`, where `students` is a `numeric` column
# listing the number of students in the `subject` at that `school`. Not all subjects get
# taught at all schools, in which case the table should contain a row for the
# respective `school` / `subject` where `students` is `0`. (`compsci` is not taught at
# `"North High School"`, in the example)
#
# The result for the example dataset above would be (with row order irrelevant)
student.grade.data.count <- rbindlist(list(
  list(school = NULL,       subject = NULL, students = NULL),
  list("North High School", "maths",        2),
  list("North High School", "physics",      2),
  list("North High School", "chemistry",    3),
  list("North High School", "biology",      2),
  list("North High School", "compsci",      0),
  list("South Academy",     "maths",        2),
  list("South Academy",     "physics",      2),
  list("South Academy",     "chemistry",    2),
  list("South Academy",     "biology",      2),
  list("South Academy",     "compsci",      3)
))
# It is pobably easiest to use `ex01DataToLong()` to get the table into long format first.
# You can use the `CJ()` function to get the first two columns, and
# joins and aggregation to get the third column.
ex03DataCount <- function(data) {
  assertDataTable(data)
  data <- ex01DataToLong(data)
  aggr <- data[, .(students = length(student.id)), by = c("school", "subject")]
  aggr <- aggr[CJ(school, subject, unique = TRUE), on = c("school", "subject")]
  aggr$students <- nafill(aggr$students, fill = 0)
  aggr
}

# Write a function that gets the data into "wide" format: The input `data` is a `data.table`
# in the format of `student.grade.data`, which should be turned into a `data.table` with
# one row per student, and columns `student.id` and `school`, followed (for example) by columns
# `grade.maths`, `grade.chemistry`, `grade.XXX` -- i.e. one of the subject names each (in any order) prefixed
# with `grade.`.
# The result of the example dataset above could be
student.grade.data.wide <- rbindlist(list(
  list(student.id = NULL, school = NULL,
       grade.maths = NULL, grade.physics = NULL, grade.chemistry = NULL, grade.biology = NULL, grade.compsci = NULL),
  list("xzqk",            "North High School",
       2.0,                1.3,                  1.7,                    NA,                   NA),
  list("hffi",            "South Academy",
       2.3,                1.0,                  NA,                     2.0,                  1.0),
  list("hwdp",            "South Academy",
       3.3,                NA,                   3.0,                    2.7,                  1.7),
  list("flzb",            "North High School",
       1.7,                2.0,                  2.0,                    2.0,                  NA),
  list("txyb",            "North High School",
       NA,                 NA,                   1.0,                    1.3,                  NA),
  list("fjea",            "South Academy",
       NA,                 2.3,                  2.0,                    NA,                   1.0)
))
# (but the `grade.maths` could also come after the `grade.physics` column for example). Note the `NA`s in places where
# a grade is not given. The ordering of rows should be the same as in the input data.
#
# This can be solved by using `ex01DataToLong()` followed by the `dcast()` method.
ex04DataWide <- function(data) {
  assertDataTable(data)
  dcast(ex01DataToLong(data)[, subject := paste0("grade.", subject)],
        student.id + school ~ subject, value.var = "grade")[data$student.id, on = "student.id"]
}
