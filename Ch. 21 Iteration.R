setwd("C:/Users/Kim F/Desktop/Semester 1/R&SQL_for_Business_Analytics/3. Zusammenfassungen & Guidelines/Ch._21_Iteration")
getwd()


##CHT 21 - Iteration (Session 8)

library(tidyverse)
library(dplyr)

#Functions reduces duplication by identifying repeated patterns of code and extract them 
#out into independent pieces that can be easily reused and updated.
#Iteration helps if you need to do the same thing to multiple inputs: 
#repeating the same operation on different columns, or on different datasets.

## Loops with base R --------------------------------------
## For Loops ----------------------------------------------

# imagine we have following tibble:
df <- tibble(
  a=rnorm(10),
  b=rnorm(10),
  c=rnorm(10),
  d=rnorm(10)
)

df
# imagine we want to compute the median for each column of the tibble: 
median(df$a)
median(df$b)
median(df$c)
median(df$d)
# that breaks rule of thumb: "never copy and paste more than twice"
# instead we could use a loop: 

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output
?vector

# 1. Output: Allocate sufficient space for the output for the purpose of obtaining efficiency.
# For example use the vector() function. It has two arguments: type (âlogicalâ, 
# âintegerâ, âdoubleâ, âcharacterâ, etc) and the length of the vector.

# 2. Sequence: This determines what to loop over: each run of the for loop will assign i to a 
# different value from seq_along(df)

# 3. Body: This is the code. output[[1]] <- median(df[[1]], output[[2]] <- median(df[[2]],
# output[[3]] <- median(df[[3]], and output[[4]] <- median(df[[4]]

# Examples For Loops: 
# a very simple loop with numeric vector: 

for(i in 1:5) {
  cat("The number i is", i, "in the vector", "\n")
}

#simple loop with non-numeric vector: 

for(word in c("hello", "new", "world")) {
  cat("The current word is", word)
}

# simple loop with list: 

list <- list(
  a = c(1,2,3),
  b = c("ab", "a", "c")
)

for(element in list) {
  cat("element:\n length:", length(element), 
      "\n class:", class(element), "\n")
}

# simple loop with a data frame 
df <- data.frame(
  a = c(1,2,3),
  b = c("ab", "a", "c"), 
  stringsAsFactors = FALSE)
df

for(element in dataframe) {
  cat("element:\n length:", length(element), 
      "\n class:", class(element), "\n")
}

# row-wise loop with a data frame
for(i in 1:nrow(df)) {
  row <- df[i,]
  cat("row", i, "\n")
  str(row)
  cat("\n")
}

# a more complex loop with creating(!) a data frame: 
for (i in 1:6) {
  id <- ((i - 1L) * chunk_size):(i * chunk_size - 1L) # in each iteration 
  # a chunk of 9 IDs are created 
  chunk[seq_along(id),] <- data.frame(id = id, # IDs are assigned to be one column in df
  # we have to provide as many rows as there are IDs
  # note: we CANNOT use length(id) here: we start at ID 0, and length(0) = 0 instead of 1,
  # hence we would have one row too few
                      type = LETTERS[[i]],
                      score = rbinom(chunk_size, 10, (10 - i) / 10),
                      stringsAsFactors = FALSE)
}
chunk

## Exercises 21.2.1
# 1. Write for loops to..
# a) Compute the mean of every column in mtcars

?mean
mean_mtcars <- vector("double", ncol(mtcars))

for(i in 1:ncol(mtcars)) {
  mean_mtcars[i] <- mean(mtcars [[i]])
}
mean_mtcars

# b) Determine the type of each column in nycflights13::flights

coltype_nycflights <- vector("character", ncol(nycflights13::flights))
for(i in 1:ncol(nycflights13::flights)) {
  coltype_nycflights[i] <- typeof(nycflights13::flights [[i]])
}

# note: if we do not include index [i] for target vector as well, only one value
# gets assigned to the vector, that will get overwritten each iteration
coltype_nycflights
View(nycflights13::flights)

#3. Compute the number of unique values in each column of iris

unique.values_iris <- vector("double", ncol(iris))
names(unique.values_iris) <- names(iris) # take col names for vector as well 

for(i in names(iris)) {
  unique.values_iris[i] <- length(unique(iris[[i]])) # ! include [[]] directly after df
}
unique.values_iris

# 4. Generate 10 random normals for each of m�h = -10, 0, 10 und 100

mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(10, mean = mu[i])
}
# note: most of the time iterator variable will be an Index

normals

#solutions from lecture for task 1: 
# Compute the mean of every column in mtcars:
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}
output

output <- vector("double", ncol(mtcars))
names(output) <- names(mtcars)
for (i in names(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}
output

#output <- vector("double", ncol(mtcars))
#names(output) <- names(mtcars)
#for (i in seq_along(mtcars)) {
#  output[i] <- mean(mtcars[[i]])
#}
#output


# Determine the type of each column in nycflights13::flights
output <- vector("list", ncol(flights))
names(output) <- names(flights)
for (i in names(flights)) {
  output[[i]] <- class(flights[[i]])
}
output


# Compute the number of unique values in each column of iris

iris_uniq <- vector("double", ncol(iris))
names(iris_uniq) <- names(iris)
for (i in names(iris)) {
  iris_uniq[i] <- length(unique(iris[[i]]))
}
iris_uniq


# Generate 10 random normals for each of Î¼=â10, 0, 10, and 100

n <- 10
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mean = mu[i])
}
normals


#However, we dont need a for loop for this since rnorm recycles means.
matrix(rnorm(n * length(mu), mean = mu), ncol = n)


# 2. Eliminate the for loop in each of the following examples by taking advantage of an
# existing function that works with vectors:

# 2.1
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}

out
out <-  stringr::str_c(letters, sep = " ", collapse = "")
out

# 2.2
x <- sample(100) # creates a random sample of 100 values
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
(sd <- sqrt(sd / (length(x) - 1)))

?sd
(x <- sample(100))
(sumse <- sum((x - mean(x))^2))
(samplesd <- (sumse/(length(x)-1)))^0.5

#3. 1
# Write a for loop that prints() the lyrics to the children's song "Alice the camel"




# 3.2 Convert the nursery rhyme "ten in the bed" to a function. Generalise it to any
# number of people in any sleeping structure


# solutions from lecture:
#The lyrics for Ten in the Bed:

numbers <- c("ten", "nine", "eight", "seven", "six", "five",
             "four", "three", "two", "one")
for (i in numbers) {
  cat(str_c("There were ", i, " in the bed\n"))
  cat("and the little one said\n")
  if (i == "one") {
    cat("I'm lonely...")
  } else {
    cat("Roll over, roll over\n")
    cat("So they all rolled over and one fell out.\n")
  }
  cat("\n")
}

# 3.3 Convert the song "99 bottles of beer on the wall" to a function. Generalise to any
# number of any vessel containing any liquid on any surface


# Intervening a loop with break and next
# break-argument: 

for(i in 1000:1100){
  if((i^2) %% 11 == (i^3) %% 17) break
}
i
# next-argument: 

for(i in 50:60) {
  if(i == 55) next 
  cat("message ", i, "\n") 
  }
i

## Nested For Loops

# input vector:
x <- c("a", "b")

# output vector:
?character
combinations1_x <- character()
combinations2_x <- vector("character")
# character() and vector("character") both define a character vector of undefined length

# loop: 
for (l1 in x) {
  for (l2 in x)
    combinations1_x <- c(combinations1_x, str_c(l1, l2, sep = ","))
} 
combinations1_x

for (l1 in x) {
  for (l2 in x)
    combinations2_x <- c(combinations2_x, str_c(l1, l2, sep = ","))
} 
combinations2_x

# nested loop and next-argument

# input vector:
x <- c("a", "b")

# output vector:
combinations2_x <- vector("character")

for (l1 in x) {
  for (l2 in x)
    if (l1 == l2) next
    combinations2_x <- c(combinations2_x, str_c(l1, l2, sep = ","))
} 
combinations2_x

# While Loop 
x <- 1
while (x <= 4){
  cat(x, " ", sep = "")
  x <- x + 1
}
x
?cat

# While Loop and next-argument

x <- 1
while (x <= 4){
  x <- x 
  if (x == 2) next
  cat(x, " ", sep = "")
  x <- x + 1
}
#  ------------------------------------------------------------------------
#Modifying an existing object

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
df$a
df$b
df$c
df$d

# Now with For Loop:
for(i in seq_along(df)) {
  rng <- range(df[[i]], na.rm = TRUE)
  df[[i]] <- (df[[i]] - rng[1])/(rng[2] - rng[1])
}
df

# Solution from lecture: 

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
seq_along(df)

df

#iterate over each column with seq_along(df)
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
df


# JUMP
# Looping patterns
# Three basic ways to loop over a vector:

#1 - Loop over the elements: for (x in xs)

#2 - Loop over the names: for (nm in names(xs))
results <- vector("list", length(x))
names(results) <- names(x)

#3 - Iteration over the numeric indices is the most general form, because given the 
#position you can extract both the name and the value:

for (i in seq_along(x)) {
  name <- names(x)[[i]]
  value <- x[[i]]
}

?seq_along
#  ------------------------------------------------------------------------

# Unknown output length 

#Itâs common to see for loops that donât preallocate the output and instead 
#increase the length of a vector at each step; this, however is inefficient

add_to_vector <- function(n) {
  output <- vector("integer", 0)
  for (i in seq_len(n)) {
    output <- c(output, i)
  }
  output  
}
add_to_vector(10000)
install.packages("microbenchmark")
library(microbenchmark)

microbenchmark(add_to_vector(10000), times = 10)

#Unit: milliseconds (A millisecond is a thousandth of a second)
#         expr          min     lq       mean    median     uq      max     neval
#add_to_vector(10000) 108.6927 109.3212 119.0582 115.3984 127.6597 142.2584    10
  
add_to_vector_2 <- function(n) {
  output <- vector("integer", n)
  for (i in seq_len(n)) {
    output[[i]] <- i
  }
  output
}
microbenchmark(add_to_vector_2(10000), times = 10)
?map

#Unit: microseconds (A microsecond is a millionth of a second)
#         expr            min     lq      mean    median     uq    max     neval
#add_to_vector_2(10000)  612.26 623.707 1053.236 633.5765 643.05 4728.724    10


means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

#EX 3 - A better solution to save the results in a list, and 
#   then combine into a single vector after the loop is done:
means <- c(0, 1, 2)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
  nn <- length(out)
}
out
out[1]
out[[1]]
out[[2]]
out[[3]]
str(out)
str(unlist(out))
purrr::flatten_dbl(out)


#Unknown (input) sequence length -  while loop - see below: no of flips varies

# for (i in seq_along(x)) {
#   # body
# }
# 
#  # Equivalent to
# i <- 1
#while (i <= length(x)) {
# # body
#i <- i + 1 
#}

flip <- function() sample(c("T", "H"), 1)
flips <- 0
nheads <- 0

while (nheads < 10) { #Dont try more than say 20! Unless you are bored. no of flips=1.232.731
  if (flip() == "H") { #Here you run the flip function
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips
#Often used in connection with simulation.


# For loops vs. functionals - R is a functional language - It is possible to wrap up
# for loops in a function which can be called....

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

#make a function instead:

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}
col_mean(df)

#Make more functions:
col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}
col_median(df)

col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}
col_sd(df)


#Make it even more simple
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
#The idea of passing a function to another function is extremely powerful 
#idea, and it is one of the behaviours that makes R a functional programming 
#language. 
col_summary(df, median)
col_summary(df, mean)
col_summary(df, sd)





##############################################################################
#purrr package, which provides functions that eliminate the need for many 
#common for loops.

#The apply family of functions in base R (apply(), lapply(), tapply(), etc) 
#solve a similar problem, but purrr is more consistent and thus is easier 
#to learn.

library(purrr) 

#A functional programming tool - instead of 'for loops'

#map() makes a list.
#map_lgl() makes a logical vector.
#map_int() makes an integer vector.
#map_dbl() makes a double vector.
#map_chr() makes a character vector.

#The chief benefits of using functions like map() is not speed, but clarity: they 
#make your code easier to write and to read.


df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
  # e = "Not a Number" # Check what happens if e is part of df
)

# The summary function (col_summary) returned doubles, so we use map_dbl():

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

#Similar but more convenient:
df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)

#Compared to col_summary() (see above) map_*() is a little bit faster 

map_dbl(df, mean, trim = 0.1)
?mean


#Shortcuts
# fitting a linear model to each group in the mtcars dataset:
models <- mtcars %>% 
  split(.$cyl) %>%  #the split fct makes the grouping.
  #the dot refers to mtcars
  purrr::map(function(df) lm(mpg ~ wt, data = df))
models

class(models)


models <- mtcars %>% 
  split(.$cyl) %>% 
  purrr::map(~lm(mpg ~ wt, data = .))
#the dot refers to models <- mtcars %>% 
models

models %>% 
  purrr::map(summary) %>% 
  purrr::map_dbl(~.$r.squared) #Still making summary by groups - contained in models.

#purrr provides an even shorter shortcut: you can use a string.
models %>% 
  purrr::map(summary) %>% 
  purrr::map_dbl("r.squared")

#You can also use an integer to select elements by position:
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)


#Base R
#lapply(), vapply(), sapply() etc.


#Dealing with failure
safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% purrr::map(safely(log))
str(y)


#Nicer: This would be easier to work with if we had two lists: one of all 
#the errors and one of all the output 
?purrr::transpose()
y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()

?possibly() 
#is simpler than safely(), because you give it a default value to return 
#when there is an error.
x <- list(1, 10, "a")
x %>% purrr::map_dbl(possibly(log, NA_real_))

quietly()
#performs a similar role to safely(), but instead of capturing errors, 
#it captures printed output, messages, and warnings:
x <- list(2, 1, -1)
x %>% purrr::map(quietly(log)) %>% 
  str()  



#often you have multiple related inputs that you need iterate along in parallel.

mu <- list(5, 10, -3)
mu %>% 
  purrr::map(rnorm, n = 5) %>% 
  str()

set.seed(117)
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  purrr::map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

# But that obfuscates the intent of the code. Instead we could use map2() 
#which iterates over two vectors in parallel:

set.seed(117)
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)
map2(mu, sigma, rnorm, n = 5) %>% str()

#Like map(), map2() is just a wrapper around a for loop:
#  map2 <- function(x, y, f, ...) {
#    out <- vector("list", length(x))
#    for (i in seq_along(x)) {
#      out[[i]] <- f(x[[i]], y[[i]], ...)
#    }
#    out
#  }

?pmap() 
#takes a list of arguments.

set.seed(117)
n <- list(1, 3, 5)
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)

args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()
#its better to name the arguments:

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()

#Since they (mean, sd and n) has the same lenght, they can be stored in a data frame
#Just give the variables the right names, i.e. the right input for rnorm
params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)
params %>% 
  pmap(rnorm)

?invoke_map()
#step up in complexity - as well as varying the arguments to the function 
#you might also vary the function itself:

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)
purrr::invoke_map(f, param, n = 5) %>% str()




#sim <- tribble(
#  ~f,      ~params,
#  "runif", list(min = -1, max = 1),
#  "rnorm", list(sd = 5),
#  "rpois", list(lambda = 10)
#)
#sim %>% 
#  mutate(sim = purrr::invoke_map(f, params, n = 10))



?walk
#Walk is an alternative to map that you use when you want to call a function 
#for its side effects, rather than for its return value.

x <- list(1, "a", 3)
x
x %>% 
  walk(print)

#here we just want to save the plots
plots <- mtcars %>% 
  split(.$cyl) %>% 
  purrr::map(~ggplot(., aes(mpg, wt)) + geom_point())
plots
paths <- stringr::str_c(names(plots), ".pdf")
paths
purrr::pwalk(list(paths, plots), ggsave, path = tempdir())

#C:\Users\au16000\AppData\Local\Temp\Rtmpe8vVba

#Predicate functions
#keep() and discard() keep elements of the input where the predicate is TRUE 
#or FALSE respectively

iris
View(iris)


iris %>% 
  keep(is.factor) %>% 
  str()


iris %>% 
  discard(is.factor) %>% 
  str()

#some() and every() determine if the predicate is true for any or for all of 
#the elements.

x <- list(1:5, letters, list(10))
x %>% 
  some(is_character)

x %>% 
  every(is_vector)

#detect() finds the first element where the predicate is true; 
#detect_index() returns its position.
x <- sample(10)
x

x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)


#head_while() and tail_while() take elements from the start or end 
#of a vector while a predicate is true:

x

x %>% 
  head_while(~ . > 5)

x %>% 
  tail_while(~ . > 5)


#For example, you might have a list of data frames, and you want to 
#reduce to a single data frame by joining the elements together:


dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)
dfs
dfs01 <- dfs %>% reduce(full_join)
dfs01


vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)
vs %>% reduce(intersect)


#use it to implement a cumulative sum:
x <- sample(10)
x
x %>% accumulate(`+`)


