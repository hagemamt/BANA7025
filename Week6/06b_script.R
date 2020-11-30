###############################################################################
# 06b_script.R
# Script for writing functions
###############################################################################



# When to write functions -------------------------------------------------

df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Can you spot the error?
df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$b, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))



# Creating your own function ----------------------------------------------

# Define function
pv <- function(FV, r, n) {
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}

# Look at function parts
formals(pv)
body(pv)
environment(pv)

# Function output
pv(FV = 1000, r = .08, n = 5)

# More function output
pv2 <- function(FV, r, n) {
  present_value <- FV / (1 + r)^n
  return(present_value)
  round(present_value, 2)
}
pv2(1000, .08, 5)



# Your turn ---------------------------------------------------------------
ratio <- function(x, y) x / y
ratio(5, 10)


# Handling arguments ------------------------------------------------------

# Calling arguments in different ways
pv(FV = 1000, r = .08, n = 5)
pv(1000, .08, 5)
pv(r = .08, FV = 1000, n = 5)
pv(.08, 1000, 5)
pv(1000, .08)

# Setting default arguments
pv <- function(FV, r, n = 5) {
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}
pv(1000, .08)
PV(1000, .08, n = 3)

# Ordering arguments
top_n <- function(x, n, wt) {
  body
}
top_n(df, 5)
df %>% top_n(5)
df %>% top_n(5, var2)



# Your turn ---------------------------------------------------------------
rescale <- function(x) {
  rng <- range(x, na.rm = T)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale(df$a)

# Your turn ---------------------------------------------------------------
rescale <- function(x, digits = 2) {
  rng <- range(x, na.rm = T)
  rescaled_result <- (x - rng[1]) / (rng[2] - rng[1])
  round(rescaled_result, digits)
}
rescale(df$a, 3)


# Your turn ---------------------------------------------------------------
rescale <- function(x, digits = 2, na.rm = F) {
  rng <- range(x, na.rm = na.rm)
  rescaled_result <- (x - rng[1]) / (rng[2] - rng[1])
  round(rescaled_result, digits)
}
rescale(df$a, 3, T)

rescale <- function(x, digits = 2, na.rm = F) {
  if (isTRUE(na.rm)) ? x <- x[!is.na(x)]
  rng <- range(x)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits)
}
rescale(df$a, 3, T)

rescale <- function(x, digits = 2, na.rm = F) {
  if (na.rm) ? x <- x[!is.na(x)]
  rng <- range(x)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits)
}
rescale(df$a, 3, T)


# Your turn ---------------------------------------------------------------
mtcars %>% 
  map_df(rescale)


# Invalid parameters ------------------------------------------------------

# Check class of FV
pv <- function(FV, r, n = 5) {
  
  if(!is.atomic(FV)) {
    stop('FV must be an atomic vector')
  }
  
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}


# Test error
fv_l <- list(fv1 = 800, 
             fv2 = 900, 
             fv3 = 1100)
pv(fv_l, 0.08)


# Add tests for class input
pv <- function(FV, r, n = 5) {
  
  if(!is.atomic(FV)) {
    stop('FV must be an atomic vector')
  }
  
  if(!is.numeric(FV) | !is.numeric(r) | !is.numeric(n)){
    stop('This function only works for numeric inputs!\n', 
         'You have provided objects of the following classes:\n', 
         'FV: ', class(FV), '\n',
         'r: ', class(r), '\n',
         'n: ', class(n))
  }
  
  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}


# Test errors
pv(FV = "1000", .08, n = 5)



# Your turn ---------------------------------------------------------------
rescale <- function(x, digits = 2, na.rm = F) {
  if(!is.numeric(x)) {
    stop('x must be an atomic vector')
  }
  if (!is.numeric(digits) | length(digits) > 1) {
    stop('digits must be a numeric vector of one element')
  }
  if (!is.logical(na.rm)) {
    stop('na.rm must be logical')
  }
  if (na.rm) ? x <- x[!is.na(x)]
  rng <- range(x)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits)
}
rescale('blah', 2, T)
rescale(df$a, '22', T)
rescale(df$a, 2, 'blah')
rescale(df$a, 2, T)

# Other notes -------------------------------------------------------------

# Lazy evaluation
lazy <- function(x, y = NULL) {
  if(!is.null(y)) {
    return(x * 2 + y)
  }
  x * 2
}
lazy(4)
lazy(4, 1)


# Lexical scoping rules
y <- 2
scoping <- function(x) {
  if(!is.null(y)) {
    return(x * 2 + y)
  }
  x * 2
}
scoping(4)



# Your turn: Practice writing functions -----------------------------------



# Your turn: Practice applying functions ----------------------------------
source("Week6/06-stat-functions.R")
mtcars %>% map_dbl(skewness)
