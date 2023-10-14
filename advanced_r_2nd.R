
?Memory
?gc
# install.packages("lobstr")
library(devtools)
# install_github("r-lib/lobstr")
library(lobstr)
library(tidyverse)

x <- c(1,2,3)

# the object, or value, doesn't have a name; it's actually the name that has a value.

y <- x

# obj_addr() gives the address of the value that x points to;
# obj_addrs() gives the address of the components the list, environment, and character vector x point to.

# You can access an object's identifier with lobstr::obj_addr(). Doing so allows you to see that both x and y point to the same identifier:

globalenv()
?globalenv
?obj_addr

obj_addr(x)
obj_addrs(globalenv())
obj_addr(y)




# 2.2.2 Exercises ======


# 1. Explain the relationship between a, b, c and d in the following code:

a <- 1:10
b <- a
c <- b
d <- 1:10

# A: a, b, c point to the same object (with the same address in memory). This object has the value 1:10. d points to a different object with the same value.

list_of_names <- list(a, b, c, d)
obj_addrs(list_of_names)


# The following code accesses the mean function in multiple ways. Do they all point to the same underlying function object? Verify this with lobstr::obj_addr().

mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")


# A: Yes, they point to the same object. We confirm this by inspecting the address of the underlying function object.

mean_functions <- list(mean,
                       base::mean,
                       get("mean"),
                       evalq(mean),
                       match.fun("mean"))

unique(obj_addrs(mean_functions))





# 2.3 Copy-on-modify ==========================================================

# Consider the following code. It binds x and y to the same underlying value, then modifies y3.

x <- c(1, 2, 3)

y <- x

y[[3]] <- 4

# object is copied on modification:

obj_addr(x)
obj_addr(y)



# 2.3.1 tracemem() ============================================================

?tracemem
x <- c(1, 2, 3)
cat(tracemem(x), "\n")
y <- x
obj_addr(x)
obj_addr(y)
y[[3]] <- 4L
y[[3]] <- 5L
untracemem(y)
obj_addr(x)
obj_addr(y)

# This behaviour is called copy-on-modify. Understanding it will radically improve your intuition about the performance of R code. A related way to describe this behaviour is to say that R objects are unchangeable, or immutable.




# 2.3.2 Function calls ========================================================

# The same rules for copying also apply to function calls.
f <- function(a) {
  a
}

x <- c(1, 2, 3)

cat(tracemem(x), "\n")

z <- f(x)  # there's no copy here!

untracemem(x)




# 2.3.3 Lists =================================================================

l1 <- list(1, 2, 3)

l2 <- l1

?ref

ref(l1, l2)

l2[[3]] <- 4

ref(l1, l2)



# 2.3.4 Data frames ===========================================================

# Data frames are lists of vectors, so copy-on-modify has important consequences when you modify a data frame. Take this data frame as an example:

# If you modify a column, only that column needs to be modified; the others will still point to their original references:

# However, if you modify a row, every column is modified, which means every column must be copied:




# 2.3.5 Character vectors =====================================================

x <- c("a", "a", "abc", "d")

ref(x, character = TRUE)




# 2.4 Object size ===============================

# You can find out how much memory an object takes with lobstr::obj_size():

obj_size(letters)
obj_size(ggplot2::diamonds)

# Since the elements of lists are references to values, the size of a list might be much smaller than you expect:

x <- runif(1e6)
obj_size(x)
y <- list(x, x, x)
obj_size(y)

# y is only 80 bytes bigger than x. That's the size of an empty list with three elements:

obj_size(list(NULL, NULL, NULL))

# Similarly, because R uses a global string pool character vectors take up less memory than you might expect: repeating a string 1000 times does not make it take up 1000 times as much memory.

banana <- "bananas bananas bananas"
obj_size(banana)
obj_size(rep(banana, 100))

# References also make it challenging to think about the size of individual objects. obj_size(x) + obj_size(y) will only equal obj_size(x, y) if there are no shared values. Here, the combined size of x and y is the same as the size of y:

obj_size(x, y)

obj_size(1:3)
obj_size(1:1e3)
obj_size(1:1e6)
obj_size(1:1e9)



# 2.5 Modify-in-place =========================================================

# 2.5.1 Objects with a single binding =========================================

v <- c(1, 2, 3)
ref(v)
v[[3]] <- 4
ref(v)

v <- c(1, 2, 3)
ref(v)
tracemem(v)
v[[3]] <- 4
ref(v)
untracemem(v)

runif(10)
x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
x
?vapply
medians <- vapply(x, median, numeric(1))

for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}

cat(tracemem(x), "\n")
for (i in 1:5) {
  x[[i]] <- x[[i]] - medians[[i]]
}
untracemem(x)

# In fact, each iteration copies the data frame not once, not twice, but three times! Two copies are made by [[.data.frame, and a further copy is made because [[.data.frame is a regular function that increments the reference count of x.

# We can reduce the number of copies by using a list instead of a data frame. Modifying a list uses internal C code, so the references are not incremented and only a single copy is made:

y <- as.list(x)
cat(tracemem(y), "\n")
for (i in 1:5) {
   y[[i]] <- y[[i]] - medians[[i]]
 }
untracemem(y)




# 2.5.2 Environments ==========================================================

e1 <- rlang::env(a = 1, b = 2, c = 3)

e2 <- e1


is.environment(e1)
e1$c
e1$c <- 4
e2$c      # there is no copy-on-modify, e2 still points to the same object, i.e. env

e <- rlang::env()
e$self <- e
ref(e)



# 2.6 Unbinding and the garbage collector =====================================

gc()
mem_used()

# library(swirl)

# _____________________________ ===============================================
# 3 Vectors ===================================================================
#
# 3.1 Introduction ============================================================

# 3.2 Atomic vectors ==========================================================

# 3.2.1 Scalars ===============================================================

# 3.2.2 Making longer vectors with c() ========================================

lgl_var <- c(TRUE, FALSE)
int_var <- c(1L, 6L, 10L)
dbl_var <- c(1, 2.5, 4.5)
chr_var <- c("these are", "some strings")

c(c(1, 2), c(3, 4))

typeof(lgl_var)
typeof(int_var)
typeof(dbl_var)
typeof(chr_var)


# 3.2.3 Missing values ==============================================


NA > 5
10 * NA
!NA

NA ^ 0
NA | TRUE
NA & FALSE




# 3.2.4 Testing and coercion ========================================

str(c("a", 1))
x <- c(FALSE, FALSE, TRUE)
as.numeric(x)
sum(x)
mean(x)
as.integer(c("1", "1.5", "a"))





# 3.3 Attributes ==============================================================



# 3.3.1 Getting and setting ===================================================

a <- 1:3
attr(a, "x") <- "abcdef"
attr(a, "x")
str(a)
attr(a, "y") <- 4:6
str(attributes(a))

# Or equivalently
a <- structure(1:3,
               x = "abcdef",
               y = 4:6)
str(attributes(a))

attributes(a)
attributes(a[1])
attributes(sum(a))




# 3.3.2 Names =======================================================

x <- c(a = 1, b = 2, c = 3)
x
x <- 1:3
names(x) <- c("a", "b", "c")
x <- setNames(1:3, c("a", "b", "c"))



# 3.3.3 Dimensions ==================================================

a <- matrix(1:6, nrow = 2, ncol = 3)
a
b <- array(1:12, c(2, 3, 2))
b
c <- 1:6
dim(c) <- c(3, 2)
c

str(1:3)      # 1d vector
str(matrix(1:3, ncol = 1)) # column vector
str(matrix(1:3, nrow = 1)) # row vector
str(array(1:3, 3)) # "array" vector




# 3.3.4 Exercises ===================================================
#
# How is setNames() implemented? How is unname() implemented? Read the source code.

setNames
unname

# https://stackoverflow.com/questions/19226816/how-can-i-view-the-source-code-for-a-function

# List Methods for S3 Generic Functions or Classes

methods(generic.function = "summary")
methods(class = "aov")                          # S3 class

## The same, with more details and more difficult to read:
print(methods(class = "aov"), byclass = FALSE)
methods("[[")                 # uses C-internal dispatching
methods("$")
methods("$<-")                # replacement function
methods("+")                  # binary operator
methods("Math")               # group generic
require(graphics)
methods("axis")               # looks like a generic, but is not

if (require(Matrix)) {
  print(methods(class = "Matrix"))  # S4 class
  m <- methods("dim")       # S3 and S4 methods
  print(m)
  print(attr(m, "info"))    # more extensive information
  ## --> help(showMethods) for related examples
}

stats:::t.ts
getAnywhere(t.ts)           # the same

library(Matrix)
standardGeneric("chol2inv")
chol2inv
getMethod("chol2inv", "diagonalMatrix")
install.packages("raster")
require(raster)
showMethods(extract)
?vector


x1 <- array(1:5, c(1, 1, 5))
x1
x2 <- array(1:5, c(1, 5, 1))
x2
x3 <- array(1:5, c(5, 1, 1))
x3




# 3.4 S3 atomic vectors ===================================



# 3.4.1 Factors =====================================================

x <- factor(c("a", "b", "b", "a"))
x
typeof(x)
attributes(x)

sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))
?table
table(sex_char)
table(sex_factor)

grade <- ordered(c("b", "b", "a", "c","c","c"),
                 levels = c("c", "b", "a"))
grade
table(grade)

# While factors look like (and often behave like) character vectors, they are built on top of integers. So be careful when treating them like strings.

# For this reason, it's usually best to explicitly convert factors to character vectors if you need string-like behaviour.




# 3.4.2 Dates =======================================================
#
# Date vectors are built on top of double vectors. They have class "Date" and no other attributes:
today <- Sys.Date()
today
typeof(today)
attributes(today)
date <- as.Date("1970-02-01")
unclass(date)




# 3.4.3 Date-times ==============================
#
# Base R19 provides two ways of storing date-time information, POSIXct, and POSIXlt.
#
# These are admittedly odd names: "POSIX" is short for Portable Operating System Interface, which is a family of cross-platform standards. "ct" standards for calendar time (the time_t type in C), and "lt" for local time (the struct tm type in C).
#
# Here we'll focus on POSIXct, because it's the simplest, is built on top of an atomic vector, and is most appropriate for use in data frames.
#
# POSIXct vectors are built on top of double vectors, where the value represents the number of seconds since 1970-01-01.

now_ct <- as.POSIXct("2018-08-01 22:00", tz = "UTC")
now_ct
#> [1] "2018-08-01 22:00:00 UTC"

typeof(now_ct)
#> [1] "double"

attributes(now_ct)
#> $class
#> [1] "POSIXct" "POSIXt"
#>
#> $tzone
#> [1] "UTC"
#
# The tzone attribute controls only how the date-time is formatted; it does not control the instant of time represented by the vector. Note that the time is not printed if it is midnight.

structure(now_ct, tzone = "Asia/Tokyo")
#> [1] "2018-08-02 07:00:00 JST"
structure(now_ct, tzone = "America/New_York")
#> [1] "2018-08-01 18:00:00 EDT"
structure(now_ct, tzone = "Australia/Lord_Howe")
#> [1] "2018-08-02 08:30:00 +1030"
structure(now_ct, tzone = "Europe/Paris")
#> [1] "2018-08-02 CEST"




# 3.4.4 Durations ===============================
#
# Durations, which represent the amount of time between pairs of dates or date-times, are stored in difftimes. Difftimes are built on top of doubles, and have a units attribute that determines how the integer should be interpreted:

one_week_1 <- as.difftime(1, units = "weeks")
one_week_1
typeof(one_week_1)
attributes(one_week_1)

one_week_2 <- as.difftime(7, units = "days")
one_week_2
typeof(one_week_2)
attributes(one_week_2)



# 3.4.5 Exercises ===============================
#
# What sort of object does table() return? What is its type? What attributes does it have? How does the dimensionality change as you tabulate more variables?

# A: table() returns a contingency table of its input variables, which has the class "table".
#
# Internally it is represented as an array (implicit class) of integers (type) with the attributes dim (dimension of the underlying array) and dimnames (one name for each input column).
#
# The dimensions correspond to the number of unique values (factor levels) in each input variable.

str(mtcars)
table(mtcars[c("vs", "cyl", "am")])
x <- table(mtcars[c("vs", "cyl", "am")])

typeof(x)
#> [1] "integer"
attributes(x)





# _____________________________ ===============================================
# 3.5 Lists ===================================================================

# Lists are a step up in complexity from atomic vectors: each element can be any type, not just vectors. Technically speaking, each element of a list is actually the same type because, as you saw in Section 2.3.3, each element is really a reference to another object, which can be any type.


# 3.5.1 Creating ====================================================
#
# You construct lists with list():

l1 <- list(1:3,
           "a",
           c(TRUE, FALSE, TRUE),
           c(2.3, 5.9))

l1[1]
l1[[1]]
l1[[1]][2]
l1[[4]][2]

typeof(l1)
typeof(l1[1])
typeof(l1[[2]])
str(l1)

# Because the elements of a list are references, creating a list does not involve copying the components into the list. For this reason, the total size of a list might be smaller than you might expect.

lobstr::obj_size(mtcars)
#> 7,208 B

l2 <- list(mtcars, mtcars, mtcars, mtcars)
lobstr::obj_size(l2)
#> 7,288 B
#
# Lists are sometimes called recursive vectors because a list can contain other lists. This makes them fundamentally different from atomic vectors.

l3 <- list(list(list(1)))
str(l3)
#> List of 1
#>  $ :List of 1
#>   ..$ :List of 1
#>   .. ..$ : num 1


# c() will combine several lists into one. If given a combination of atomic vectors and lists, c() will coerce the vectors to lists before combining them. Compare the results of list() and c():

l4 <- list(list(1, 2), c(3, 4))
l5 <- c(list(1, 2), c(3, 4))
str(l4)
str(l5)


# 3.5.2 Testing and coercion ========================================

# The typeof() a list is list. You can test for a list with is.list(), and coerce to a list with as.list().

list(1:3)
as.list(1:3)

# You can turn a list into an atomic vector with unlist(). The rules for the resulting type are complex, not well documented, and not always equivalent to what you'd get with c().



# 3.5.3 Matrices and arrays =========================================
#
# With atomic vectors, the dimension attribute is commonly used to create matrices. With lists, the dimension attribute can be used to create list-matrices or list-arrays:

l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)
l

l[[1, 1]]
l[[1]]
l[[1, 2]]
l[[2, 1]]

# These data structures are relatively esoteric but they can be useful if you want to arrange objects in a grid-like structure. For example, if you're running models on a spatio-temporal grid, it might be more intuitive to store the models in a 3D array that matches the grid structure.
#
# 3.5.4 Exercises ===================================================
#
# List all the ways that a list differs from an atomic vector.

# A: To summarise:
#   Atomic vectors are always homogeneous (all elements must be of the same type).
#   Lists may be heterogeneous (the elements can be of different types).
#   Atomic vectors point to one address in memory, while
#   lists contain separate references for each element.

lobstr::ref(1:2)
lobstr::ref(list(1:2, 2))

# Subsetting with out of bound values or NAs leads to NAs for atomics and NULL values for lists.

# Subsetting atomic vectors
(1:2)[3]
(1:2)[NA]

# Subsetting lists
as.list(1:2)[3]
as.list(1:2)[NA]

# Why do you need to use unlist() to convert a list to an atomic vector? Why doesn't as.vector() work?

# A: A list is already a vector, though not an atomic one!

  # Note that as.vector() and is.vector() use different defintions of "vector"!

  is.vector(as.vector(mtcars))

# Compare and contrast c() and unlist() when combining a date and date-time into a single vector.

# A: Date and date-time objects are built upon doubles. Dates are represented as days, while date-time-objects (POSIXct) represent seconds (counted in respect to the reference date 1970-01-01, also known as "The Epoch").

# Combining these objects leads to surprising output because c() does not consider the class of both inputs:

date    <- as.Date("1970-01-02")
dttm_ct <- as.POSIXct("1970-01-01 01:00", tz = "UTC")

c(date, dttm_ct)  # equal to c.Date(date, dttm_ct)
#> [1] "1970-01-02" "1979-11-10"
c(dttm_ct, date)  # equal to c.POSIXct(date, dttm_ct)
#> [1] "1970-01-01 01:00:00 UTC" "1970-01-01 00:00:01 UTC"
# The generic function dispatches based on the class of its first argument. When c.Date() is executed, dttm_ct is converted to a date, but the 3600 seconds are mistaken for 3600 days! When c.POSIXct() is called on date, one day counts as one second only, as illustrated by the following line:

  unclass(c(date, dttm_ct))  # internal representation
#> [1]    1 3600
date + 3599
#> [1] "1979-11-10"
# Some of these problems may be avoided via explicit conversion of the classes:

  c(as.Date(dttm_ct, tz = "UTC"), date)
#> [1] "1970-01-01" "1970-01-02"
# Let's look at unlist(), which operates on list input.

# attributes are stripped
unlist(list(date, dttm_ct))
#> [1]    1 3600
# We see that internally dates(-times) are stored as doubles. Unfortunately this is all we are left with, when unlist strips the attributes of the list.
#
# To summarise: c() coerces types and errors may occur because of inappropriate method dispatch. unlist() strips attributes.



# _____________________________ ===============================================
# 3.6 Data frames and tibbles ===================
#
# The two most important S3 vectors built on top of lists are data frames and tibbles.
#
# If you do data analysis in R, you're going to be using data frames. A data frame is a named list of vectors with attributes for (column) names, row.names20, and its class, "data.frame":

df1 <- data.frame(x = 1:3, y = letters[1:3])
typeof(df1)
attributes(df1)

df1[1]
df1[[1]]
df1[[2]]
rownames(df1)
colnames(df1)
names(df1)

# In contrast to a regular list, a data frame has an additional constraint: the length of each of its vectors must be the same. This gives data frames their rectangular structure and explains why they share the properties of both matrices and lists:
#
# A data frame has rownames() and colnames(). The names() of a data frame are the column names.
#
# A data frame has nrow() rows and ncol() columns. The length() of a data frame gives the number of columns.
#
# Data frames are one of the biggest and most important ideas in R, and one of the things that makes R different from other programming languages. However, in the over 20 years since their creation, the ways that people use R have changed, and some of the design decisions that made sense at the time data frames were created now cause frustration.
#
# This frustration lead to the creation of the tibble (Müller and Wickham 2018), a modern reimagining of the data frame. Tibbles are designed to be (as much as possible) drop-in replacements for data frames that fix those frustrations. A concise, and fun, way to summarise the main differences is that tibbles are lazy and surly: they do less and complain more. You'll see what that means as you work through this section.
#
# Tibbles are provided by the tibble package and share the same structure as data frames. The only difference is that the class vector is longer, and includes tbl_df. This allows tibbles to behave differently in the key ways which we'll discuss below.

library(tibble)

df2 <- tibble(x = 1:3, y = letters[1:3])
typeof(df2)
attributes(df2)




# 3.6.1 Creating ====================================================
#
# You create a data frame by supplying name-vector pairs to data.frame():
#
df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c")
)
str(df)

# Beware of the default conversion of strings to factors. Use stringsAsFactors = FALSE to suppress this and keep character vectors as character vectors:

df1 <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
str(df1)

# Creating a tibble is similar to creating a data frame. The difference between the two is that tibbles never coerce their input (this is one feature that makes them lazy):

df2 <- tibble(
  x = 1:3,
  y = c("a", "b", "c")
)
str(df2)

# Additionally, while data frames automatically transform non-syntactic names (unless check.names = FALSE), tibbles do not (although they do print non-syntactic names surrounded by `).

names(data.frame(`1` = 1))
names(tibble(`1` = 1))

# While every element of a data frame (or tibble) must have the same length, both data.frame() and tibble() will recycle shorter inputs. However, while data frames automatically recycle columns that are an integer multiple of the longest column, tibbles will only recycle vectors of length one.

data.frame(x = 1:4, y = 1:2)
data.frame(x = 1:4, y = 1:3)
tibble(x = 1:4, y = 1)
tibble(x = 1:4, y = 1:2)

# There is one final difference: tibble() allows you to refer to variables created during construction:

tibble(
  x = 1:3,
  y = x * 2
)



# 3.6.2 Row names ===================================================
#
# Data frames allow you to label each row with a name, a character vector containing only unique values:

df3 <- data.frame(
  age = c(35, 27, 18),
  hair = c("blond", "brown", "black"),
  row.names = c("Bob", "Susan", "Sam")
)
df3

# You can get and set row names with rownames(), and you can use them to subset rows:

rownames(df3)

df3["Bob", ]

# Row names arise naturally if you think of data frames as 2D structures like matrices: columns (variables) have names so rows (observations) should too. Most matrices are numeric, so having a place to store character labels is important. But this analogy to matrices is misleading because matrices possess an important property that data frames do not: they are transposable. In matrices the rows and columns are interchangeable, and transposing a matrix gives you another matrix (transposing again gives you the original matrix). With data frames, however, the rows and columns are not interchangeable: the transpose of a data frame is not a data frame.
#
# There are three reasons why row names are undesirable:
#
# Metadata is data, so storing it in a different way to the rest of the data is fundamentally a bad idea. It also means that you need to learn a new set of tools to work with row names; you can't use what you already know about manipulating columns.
#
# Row names are a poor abstraction for labelling rows because they only work when a row can be identified by a single string. This fails in many cases, for example when you want to identify a row by a non-character vector (e.g. a time point), or with multiple vectors (e.g. position, encoded by latitude and longitude).
#
# Row names must be unique, so any duplication of rows (e.g. from bootstrapping) will create new row names. If you want to match rows from before and after the transformation, you'll need to perform complicated string surgery.

df3[c(1, 1, 1), ]

# For these reasons, tibbles do not support row names. Instead the tibble package provides tools to easily convert row names into a regular column with either rownames_to_column(), or the rownames argument in as_tibble():

as_tibble(df3, rownames = "name")



# 3.6.3 Printing ====================================================
#
# One of the most obvious differences between tibbles and data frames is how they print. I assume that you're already familiar with how data frames are printed, so here I'll highlight some of the biggest differences using an example dataset included in the dplyr package:

dplyr::starwars

# Tibbles only show the first 10 rows and all the columns that will fit on screen. Additional columns are shown at the bottom.
#
# Each column is labelled with its type, abbreviated to three or four letters.
#
# Wide columns are truncated to avoid having a single long string occupy an entire row. (This is still a work in progress: it's a tricky tradeoff between showing as many columns as possible and showing columns in their entirety.)
#
# When used in console environments that support it, colour is used judiciously to highlight important information, and de-emphasise supplemental details.



# 3.6.4 Subsetting ==================================================
#
# As you will learn in Chapter 4, you can subset a data frame or a tibble like a 1D structure (where it behaves like a list), or a 2D structure (where it behaves like a matrix).
#
# In my opinion, data frames have two undesirable subsetting behaviours:
#
# When you subset columns with df[, vars], you will get a vector if vars selects one variable, otherwise you'll get a data frame. This is a frequent source of bugs when using [ in a function, unless you always remember to use df[, vars, drop = FALSE].
#
# When you attempt to extract a single column with df$x and there is no column x, a data frame will instead select any variable that starts with x. If no variable starts with x, df$x will return NULL. This makes it easy to select the wrong variable or to select a variable that doesn't exist.
#
# Tibbles tweak these behaviours so that a [ always returns a tibble, and a $ doesn't do partial matching and warns if it can't find a variable (this is what makes tibbles surly).

df1 <- data.frame(xyz = "a")
df2 <- tibble(xyz = "a")

str(df1$x)
str(df2$x)

# A tibble's insistence on returning a data frame from [ can cause problems with legacy code, which often uses df[, "col"] to extract a single column. If you want a single column, I recommend using df[["col"]]. This clearly communicates your intent, and works with both data frames and tibbles.



# 3.6.5 Testing and coercing ========================================
#
# To check if an object is a data frame or tibble, use is.data.frame():

is.data.frame(df1)
is.data.frame(df2)

# Typically, it should not matter if you have a tibble or data frame, but if you need to be certain, use is_tibble():

is_tibble(df1)
is_tibble(df2)

# You can coerce an object to a data frame with as.data.frame() or to a tibble with as_tibble().



# 3.6.6 List columns ================================================
#
# Since a data frame is a list of vectors, it is possible for a data frame to have a column that is a list. This is very useful because a list can contain any other object: this means you can put any object in a data frame. This allows you to keep related objects together in a row, no matter how complex the individual objects are. You can see an application of this in the "Many Models" chapter of R for Data Science, http://r4ds.had.co.nz/many-models.html.
#
# List-columns are allowed in data frames but you have to do a little extra work by either adding the list-column after creation or wrapping the list in I()22.

df <- data.frame(x = 1:3)
df
df$y <- list(1:2, 1:3, 1:4)
df
data.frame(
  x = 1:3,
  y = I(list(1:2, 1:3, 1:4))
)

# List columns are easier to use with tibbles because they can be directly included inside tibble() and they will be printed tidily:

tibble(
  x = 1:3,
  y = list(1:2, 1:3, 1:4)
)


# 3.6.7 Matrix and data frame columns ===============================
#
# As long as the number of rows matches the data frame, it's also possible to have a matrix or array as a column of a data frame. (This requires a slight extension to our definition of a data frame: it's not the length() of each column that must be equal, but the NROW().) As for list-columns, you must either add it after creation, or wrap it in I().

dfm <- data.frame(
  x = 1:3 * 10
)
dfm
dfm$y <- matrix(1:9, nrow = 3)
dfm
dfm$z <- data.frame(a = 3:1, b = letters[1:3], stringsAsFactors = FALSE)
dfm
str(dfm)

# Matrix and data frame columns require a little caution. Many functions that work with data frames assume that all columns are vectors. Also, the printed display can be confusing.

dfm[1, ]
#>    x y.1 y.2 y.3 z.a z.b
#> 1 10   1   4   7   3   a


# 3.6.8 Exercises ===================================================
#
# Can you have a data frame with zero rows? What about zero columns?
#
# What happens if you attempt to set rownames that are not unique?
#
# If df is a data frame, what can you say about t(df), and t(t(df))? Perform some experiments, making sure to try different column types.
#
# What does as.matrix() do when applied to a data frame with columns of different types? How does it differ from data.matrix()?
#
# 3.7 NULL ==========================================================
#
# To finish up this chapter, I want to talk about one final important data structure that's closely related to vectors: NULL. NULL is special because it has a unique type, is always length zero, and can't have any attributes:

typeof(NULL)
length(NULL)
x <- NULL
attr(x, "y") <- 1

# You can test for NULLs with is.null():
is.null(NULL)

# There are two common uses of NULL:
#
# To represent an empty vector (a vector of length zero) of arbitrary type. For example, if you use c() but don't include any arguments, you get NULL, and concatenating NULL to a vector will leave it unchanged:

c()

# To represent an absent vector. For example, NULL is often used as a default function argument, when the argument is optional but the default value requires some computation (see Section 6.5.3 for more on this). Contrast this with NA which is used to indicate that an element of a vector is absent.
#
# If you're familiar with SQL, you'll know about relational NULL and might expect it to be the same as R's. However, the database NULL is actually equivalent to R's NA.



# 3.8 Quiz answers ==================================================

# The four common types of atomic vector are logical, integer, double and character. The two rarer types are complex and raw.

# Attributes allow you to associate arbitrary additional metadata to any object. You can get and set individual attributes with attr(x, "y") and attr(x, "y") <- value; or you can get and set all attributes at once with attributes().

# The elements of a list can be any type (even a list); the elements of an atomic vector are all of the same type. Similarly, every element of a matrix must be the same type; in a data frame, different columns can have different types.

# You can make a list-array by assigning dimensions to a list. You can make a matrix a column of a data frame with df$x <- matrix(), or by using I() when creating a new data frame data.frame(x = I(matrix())).

# Tibbles have an enhanced print method, never coerce strings to factors, and provide stricter subsetting methods.
#
#
#
# _____________________________ ===============================================
# 4 Subsetting ================================================================

# 4.1 Introduction ======

# 4.2 Selecting multiple elements =====

# 4.2.1 Atomic vectors ========

x <- c(2.1, 4.2, 3.3, 5.4)
x
x[c(3, 1)]
x[order(x)]
x[c(1, 1)]
# Real numbers are silently truncated to integers
x[c(2.1, 2.9)]
x[-c(3, 1)]
x[c(-1, 2)]
x[c(TRUE, TRUE, FALSE, FALSE)]
x[x > 3]
x[c(TRUE, FALSE)]
x[c(TRUE, FALSE, TRUE, FALSE)]
x[c(TRUE, TRUE, NA, FALSE)]
x[]
x[0]
(y <- setNames(x, letters[1:4]))
y[c("d", "c", "a")]
y[c("a", "a", "a")]
z <- c(abc = 1, def = 2)
z
str(z)
z[c("a", "d")]
y[factor("b")]


# 4.2.2 Lists ========

# 4.2.3 Matrices and arrays =======
a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a
a[1:2, ]
a[c(TRUE, FALSE, TRUE), c("B", "A")]
a[0, -2]
a[1, ]
a[1, 1]
vals <- outer(1:5,
              1:5,
              FUN = "paste",
              sep = ",")
vals
vals[c(4, 15)]
select <- matrix(ncol = 2,
                 byrow = TRUE,
                 c(1, 1,
                   3, 1,
                   2, 4))
select
vals
vals[select]



# 4.2.4 Data frames and tibbles ============

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df
df[df$x == 2, ]
df[c(1, 3), ]
df[c("x", "z")]
df[, c("x", "z")]
# There's an important difference if you select a single
# column: matrix subsetting simplifies by default, list
# subsetting does not.
str(df["x"])
str(df[, "x"])
df <- tibble::tibble(x = 1:3, y = 3:1, z = letters[1:3])
str(df["x"])
str(df[, "x"])


# 4.2.5 Preserving dimensionality ===========
a <- matrix(1:4, nrow = 2)
a
str(a[1, ])
str(a[1, , drop = FALSE])
df <- data.frame(a = 1:2, b = 1:2)
str(df[, "a"])
str(df[, "a", drop = FALSE])
z <- factor(c("a", "b"))
z[1]
z
z[1, drop = TRUE]


# 4.2.6 Exercises ======

mtcars[mtcars$cyl == 4, ]

mtcars[1:4, ]

mtcars[mtcars$cyl <= 5,]

mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]

x <- 1:5

x[NA]

x <- outer(1:5, 1:5, FUN = "*")
x
x[upper.tri(x)]
# A: upper.tri() returns a logical matrix containing TRUE for all upper diagonal elements and FALSE otherwise. The implementation of upper.tri() is straightforward, but quite interesting as it uses .row(dim(x)) <= .col(dim(x)) to create the logical matrix. Its subsetting-behaviour will be identical to subsetting with logical matrices, where all elements that correspond to TRUE will be selected. We don't need to treat this form of subsetting in a special way.

diag2 <- function(x){
  n <- min(nrow(x), ncol(x))
  idx <- cbind(seq_len(n), seq_len(n))
  x[idx]
}

# Let's check if it works
(x <- matrix(1:30, 5))
x
diag(x)
diag2(x)
diag


# 4.3 Selecting a single element ====================================

# 4.3.1 [[ ==========================================================

# [[ is most important when working with lists because subsetting a list with [ always returns a smaller list. To help make this easier to nderstand we can use a metaphor:

# If list x is a train carrying objects, then x[[5]] is the object in ar 5; x[4:6] is a train of cars 4-6.

x <- list(1:3, "a", 4:6)
x[[c(1, 2)]]
x[[1]]
x[[1]][[2]]
x
length(x)



# 4.3.2 $

# $ is a shorthand operator: x$y is roughly equivalent to x[["y"]]. It's ften used to access variables in a data frame, as in mtcars$cyl or iamonds$carat. One common mistake with $ is to use it when you have he name of a column stored in a variable:

var <- "cyl"
# Doesn't work - mtcars$var translated to mtcars[["var"]]
mtcars$var
#> NULL

# Instead use [[
mtcars[[var]]

x <- list(abc = 1)
x
x$a

x[["a"]]

# To help avoid this behaviour I highly recommend setting the global ption warnPartialMatchDollar to TRUE:

options(warnPartialMatchDollar = TRUE)
x$a

# (For data frames, you can also avoid this problem by using tibbles, hich never do partial matching.)

# 4.3.3 Missing and out-of-bounds indices =====================================

x <- list(
a = list(1, 2, 3),
b = list(3, 4, 5)
)

purrr::pluck(x, "a", 1)

purrr::pluck(x, "c", 1)

purrr::pluck(x, "c", 1, .default = NA)

# 4.3.4 @ and slot() ==========================================================

# 4.3.5 Exercises =============================================================

# _____________________________ ===============================================

# 4.4 Subsetting and assignment ===============================================

x <- 1:5
x[c(1, 2)] <- c(101, 102)
x

x <- list(a = 1, b = 2)
x[["b"]] <- NULL
str(x)

y <- list(a = 1, b = 2)
y["b"] <- list(NULL)
str(y)

str(mtcars)
mtcars[] <- lapply(mtcars, as.integer)
is.data.frame(mtcars)

mtcars <- lapply(mtcars, as.integer)
is.data.frame(mtcars)
str(mtcars)

# 4.5 Applications ============================================================

# 4.5.1 Lookup tables (character subsetting) ==================================
# Character matching is a powerful way to create lookup tables. Say you want to convert abbreviations:
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup
lookup[x]

unname(lookup[x])


# 4.5.2 Matching and merging by hand (integer subsetting) =====================

# You can also have more complicated lookup tables with multiple columns of information. For example, suppose we have a vector of integer grades, and a table that describes their properties:
grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)

grades
info

# Then, let's say we want to duplicate the info table so that we have a row for each value in grades. An elegant way to do this is by combining match() and integer subsetting (match(needles, haystack) returns the position where each needle is found in the haystack).

id <- match(grades, info$grade)

id

info[id, ]


# 4.5.3 Random samples and bootstraps (integer subsetting) ====================

# You can use integer indices to randomly sample or bootstrap a vector or data frame. Just use sample(n) to generate a random permutation of 1:n, and then use the results to subset the values:
df <- data.frame(x = c(1, 2, 3, 1, 2),
    y = 5:1,
    z = letters[1:5])

df
# Randomly reorder
df[sample(nrow(df)), ]

# Select 3 random rows
df[sample(nrow(df), 3), ]

# Select 6 bootstrap replicates
df[sample(nrow(df), 6, replace = TRUE), ]


# 4.5.4 Ordering (integer subsetting) =========================================

x <- c("b", "c", "a")
order(x)

x[order(x)]

# Randomly reorder df
df2 <- df[sample(nrow(df)), 3:1]
df2

df2[order(df2$x), ]

df2[, order(names(df2))]


# 4.5.5 Expanding aggregated counts (integer subsetting) ======================

df <- data.frame(x = c(2, 4, 1),
    y = c(9, 11, 6),
    n = c(3, 5, 1))

rep(1:nrow(df), df$n)

df[rep(1:nrow(df), df$n), ]



# 4.5.6 Removing columns from data frames (character ) ========================

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

df$z <- NULL

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

df[c("x", "y")]

df[setdiff(names(df), "z")]



# 4.5.7 Selecting rows based on a condition (logical subsetting) ==============

mtcars[mtcars$gear == 5, ]
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]

# !(X & Y) is the same as !X | !Y

# !(X | Y) is the same as !X & !Y

# For example, !(X & !(Y | Z)) simplifies to
# !X | !!(Y|Z), and then to
# X | Y | Z.


# 4.5.8 Boolean algebra versus sets (logical and integer ) ====================

# It's useful to be aware of the natural equivalence between set operations (integer subsetting) and Boolean algebra (logical subsetting). Using set operations is more effective when:
#
#   You want to find the first (or last) TRUE.

# You have very few TRUEs and very many FALSEs; a set representation may be faster and require less storage.

# which() allows you to convert a Boolean representation to an integer representation. There's no reverse operation in base R but we can easily create one:
x <- sample(10) < 4
x
which(x)

unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}

unwhich(which(x), 10)

(x1 <- 1:10 %% 2 == 0)

(x2 <- which(x1))

(y1 <- 1:10 %% 5 == 0)

(y2 <- which(y1))

x1 & y1

intersect(x2, y2)

x1 | y1

union(x2, y2)

x1 & !y1

setdiff(x2, y2)

xor(x1, y1)

setdiff(union(x2, y2), intersect(x2, y2))

# _____________________________ ===============================================
# 5 Control flow ====================================================
#
# 5.1 Introduction ==================================================
#
# There are two primary tools of control flow: choices and loops. Choices, like if statements and switch() calls, allow you to run different code depending on the input. Loops, like for and while, allow you to repeatedly run code, typically with changing options. I'd expect that you're already familiar with the basics of these functions so I'll briefly cover some technical details and then introduce some useful, but lesser known, features.

# The condition system (messages, warnings, and errors), which you'll learn about in Chapter 8, also provides non-local control flow.

# Quiz
#
# Want to skip this chapter? Go for it, if you can answer the questions below. Find the answers at the end of the chapter in Section 5.4.
#
# What is the different between if and ifelse()?
#
#   In the following code, what will the value of y be if x is TRUE? What if x is FALSE? What if x is NA?
#
#   y <- if (x) 3
# What does switch("x", x = , y = 2, z = 3) return?


# Outline ===========================================================

# Section 5.2 dives into the details of if, then discusses the close relatives ifelse() and switch().

# Section 5.3 starts off by reminding you of the basic structure of the for loop in R, discusses some common pitfalls, and then talks about the related while and repeat statements.



# 5.2 Choices =======================================================
#
# The basic form of an if statement in R is as follows:
#
#   if (condition) true_action
# if (condition) true_action else false_action
# If condition is TRUE, true_action is evaluated; if condition is FALSE, the optional false_action is evaluated.
#
# Typically the actions are compound statements contained within {:

grade <- function(x) {
  if (x > 90) {
    "A"
  } else if (x > 80) {
    "B"
  } else if (x > 50) {
    "C"
  } else {
    "F"
  }
}

# if returns a value so that you can assign the results:
x1 <- if (TRUE) 1 else 2
x2 <- if (FALSE) 1 else 2
c(x1, x2)

# I recommend assigning the results of an if statement only when the entire expression fits on one lineotherwise it tends to be hard to read.

# When you use the single argument form without an else statement, if invisibly (Section 6.7.2) returns NULL if the condition is FALSE.
# Since functions like c() and paste() drop NULL inputs, this allows for a compact expression of certain idioms:
greet <- function(name, birthday = FALSE) {
  paste0("Hi ", name,
         if (birthday)
           " and HAPPY BIRTHDAY")
}

greet("Maria", FALSE)

greet("Jaime", TRUE)



# 5.2.1 Invalid inputs ==============================================

# The condition should evaluate to a single TRUE or FALSE. Most other inputs will generate an error:

if ("x") 1
#> Error in if ("x") 1: argument is not interpretable as logical

if (logical()) 1
#> Error in if (logical()) 1: argument is of length zero

if (NA) 1
#> Error in if (NA) 1: missing value where TRUE/FALSE needed

# The exception is a logical vector of length greater than 1, which generates a warning:

if (c(TRUE, FALSE)) 1

#> Warning in if (c(TRUE, FALSE)) 1: the condition has length > 1 and only the first element will be used
#> [1] 1

# In R 3.5.0 and greater, thanks to Henrik Bengtsson, you can turn this into an error by setting an environment variable:

Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")

if (c(TRUE, FALSE)) 1

#> Error in if (c(TRUE, FALSE)) 1: the condition has length > 1

# I think this is good practice as it reveals a clear mistake that you might otherwise miss if it were only shown as a warning.



# 5.2.2 Vectorised if ===============================================

# Given that if only works with a single TRUE or FALSE, you might wonder what to do if you have a vector of logical values. Handling vectors of values is the job of ifelse():a vectorised function with test, yes, and no vectors (that will be recycled to the same length):

x <- 1:10
ifelse(x %% 5 == 0, "XXX", as.character(x))

#>  [1] "1"   "2"   "3"   "4"   "XXX" "6"   "7"   "8"   "9"   "XXX"

ifelse(x %% 2 == 0, "even", "odd")

#>  [1] "odd"  "even" "odd"  "even" "odd"  "even" "odd"  "even" "odd"  "even"

# Note that missing values will be propagated into the output.

# I recommend using ifelse() only when the yes and no vectors are the same type as it is otherwise hard to predict the output type.

# Another vectorised equivalent is the more general dplyr::case_when(). It uses a special syntax to allow any number of condition - vector pairs:

dplyr::case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "???",
  TRUE ~ as.character(x)
)

#>  [1] "1""2""3""4""fizz" "6""buzz" "8""9""fizz"


# 5.2.3 switch() statement ==========================================

# Closely related to if is the switch() - statement. It's a compact, special purpose equivalent that lets you replace code like:

x_option <- function(x) {
  if (x == "a") {
    "option 1"
  } else if (x == "b") {
    "option 2"
  } else if (x == "c") {
    "option 3"
  } else {
    stop("Invalid `x` value")
  }
}

# with the more succinct:
x_option <- function(x) {
  switch(x,
         a = "option 1",
         b = "option 2",
         c = "option 3",
         stop("Invalid `x` value"))
}

# The last component of a switch() should always throw an error, otherwise unmatched inputs will invisibly return NULL:

(switch("c", a = 1, b = 2))

#> NULL

# If multiple inputs have the same output, you can leave the right hand side of = empty and the input will "fall through" to the next value. This mimics the behaviour of C's switch statement:

legs <- function(x) {
    switch(
      x,
      cow = ,
      horse = ,
      dog = 4,
      human = ,
      chicken = 2,
      plant = 0,
      stop("Unknown input")
    )
  }

legs("cow")
legs("dog")
legs(c("cow","dog"))    # not vectorised, only for single evaluation

# It is also possible to use switch() with a numeric x, but is harder to read, and has undesirable failure modes if x is a not a whole number. I recommend using switch() only with character inputs.




# 5.3 Loops =========================================================

# For loops are used to iterate over items in a vector. They have the following basic form:for (item in vector)
#   perform_action
# For each item in vector, perform_action is called once
# updating the value of item each time.

for (i in 1:3) {
  print(i)
}

# (When iterating over a vector of indices, it's conventional to use very short variable names like i, j, or k.)

# N.B.:for assigns the item to the current environment, overwriting any existing variable with the same name:

i <- 100

for (i in 1:3) {

}
i
#> [1] 3

# There are two ways to terminate a for loop early:next exits the current iteration.
# break exits the entire for loop.
for (i in 1:10) {
  if (i < 3)
    next
  print(i)
  if (i >= 5)
    break
}


# 5.3.1 Common pitfalls =============================================

# There are three common pitfalls to watch out for when using for. First, if you're generating data, make sure to preallocate the output container. Otherwise the loop will be very slow
# see Sections 23.2.2 and 24.6 for more details. The vector() function is helpful here.

means <- c(1, 50, 20)
out <- vector("list", length(means))
for (i in 1:length(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}

str(out)
out

# Next, beware of iterating over 1:length(x), which will fail in unhelpful ways if x has length 0:

means <- c()
out <- vector("list", length(means))
for (i in 1:length(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}

#> Error in rnorm(10, means[[i]]): invalid arguments
#
# This occurs because:works with both increasing and decreasing sequences:1:length(means)
#> [1] 1 0

# Use seq_along(x) instead. It always returns a value the same length as x:seq_along(means)
#> integer(0)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}
# Finally, you might encounter problems when iterating over S3 vectors, as loops typically strip the attributes:

xs <- as.Date(c("2020-01-01", "2010-01-01"))
for (x in xs) {
  print(x)
}

# Work around this by calling [[yourself:
for (i in seq_along(xs)) {
  print(xs[[i]])
}




# 5.3.2 Related tools ===============================================

# for loops are useful if you know in advance the set of values that you want to iterate over. If you don't know, there are two related tools with more flexible specifications:while (condition)
  # action:performs action while condition is TRUE.

repeat (action)
# : repeats action forever (i.e. until it encounters break).

# R does not have an equivalent to the do {
  action
} while (condition)
  syntax found in other languages.

# You can rewrite any for loop to use while instead, and you can rewrite any while loop to use repeat , but the converses are not true. That means while is more flexible than for , and repeat is more flexible than while. It's good practice, however, to use the least -
#   flexible solution to a problem, so you should use for wherever possible.
#
# Generally speaking you shouldn't need to use for loops for data analysis tasks, as map() and apply() already provide less flexible solutions to most problems. You'll learn more in Chapter 9.

# 5.3.3 Exercises ===================================================
#
# Why does this code succeed without errors or warnings ?

  x <- numeric()
out <- vector("list", length(x))
for (i in 1:length(x)) {
  out[i] <- x[i] ^ 2
}
out
# When the following code is evaluated, what can you say about the vector being iterated ?

xs <- c(1, 2, 3)
for (x in xs) {
  xs <- c(xs, x * 2)
}
xs
#> [1] 1 2 3 2 4 6
# What does the following code tell you about when the index is updated ?

  for (i in 1:3) {
    i <- i * 2
    print(i)
  }
#> [1] 2
#> [1] 4
#> [1] 6
#
# 5.4 Quiz answers ==================================================
#
# if works with scalars
# ifelse() works with vectors.
#
# When x is TRUE, y will be 3
# when FALSE, y will be NULL
# when NA the if statement will throw an error.
#
# This switch() statement makes use of fall - through so it will return 2. See details in Section 5.2.3.
# ________________ ========
# 6. Functions ================================================================

# _6.2.1 Function components ====

f02 <- function(x, y) {
  # a comment
  x + y
  }

formals(f02)

body(f02)

environment(f02)

attr(f02, "srcref")                                     # also shows comments

# 6.2.2 Primitive functions ===================================================

# sum() and [ call C-code directly

typeof(sum)
typeof(`[`)

# 6.2.3 First-class functions =================================================

# R functions are objects in their own right
#
# anonymous functions:
lapply(mtcars, function(x) length(unique(x)))

Filter(function(x) !is.numeric(x), mtcars)

integrate(function(x) sin(x) ^ 2, 0, pi)


# 6.2.4 Invoking a function ===================================================

# You normally call a function by placing with arguments wrapped in parentheses:

# mean(1:10, na.rm = TRUE).

# But what happens if you have the arguments  already in a data structure?

args <- list(1:10, na.rm = TRUE)

# You can instead use do.call(): it has two arguments. The function to call, and a list containing the function arguments:

do.call(mean, args)




# 6.3 Function composition ====================================================

square <- function(x) x^2
deviation <- function(x) x - mean(x)

# option 1: nest the function calls
x <- runif(100)
sqrt(mean(square(deviation(x))))    # population standard deviation

# Or you save the intermediate results as variables:
out <- deviation(x)
out <- square(out)
out <- mean(out)
out <- sqrt(out)
out

# or use magrittr package
library(magrittr)
x %>%
  deviation() %>%
  square() %>%
  mean() %>%
  sqrt()


# Lexical scoping =============================================================

# scoping, the act of finding the value associated with a  name.

x <- 10

g01 <- function() {
  x <- 20
  x
}

g01()


# 6.4.1 Name masking ==========================================================

# The basic principle of lexical scoping is that names defined inside a function mask names defined outside a function. This is illustrated in the following example.

x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
g02()


# If a name isn't defined inside a function, R looks one level up.
x <- 2
g03 <- function() {
  y <- 1
  c(x, y)
}
g03()

# And this doesn't change the previous value of y
y


# The same rules apply if a function is defined inside another function. First, R looks inside the current function. Then, it looks where that function was defined (and so on, all the way up to the global environment). Finally, it looks in other loaded packages.

x <- 1

g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}

g04()


# 6.4.2 Functions versus variables ============================================

# In R, functions are ordinary objects.
# This means the scoping rules described above also apply to functions:

g07 <- function(x) x + 1

g08 <- function() {
  g07 <- function(x) x + 100
  g07(10)
}

g08()




# 6.4.3 A fresh start =========================================================

# What happens to values between invocations of a function?

# Consider the example below. What will happen the first time you run this function?
# What will happen the second time?

g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g11()

g11()

# You might be surprised that g11() always returns the same value. This happens because every time a function is called a new environment is created to host its execution.
#
# This means that a function has no way to tell what happened the last time it was run;
# each invocation is completely independent.




# 6.4.4 Dynamic lookup ========================================================

# Lexical scoping determines where, but not when to look for values.
#
# R looks for values when the function is run, not when the function is created.
#
# Together, these two properties tell us that the output of a function can differ depending on the objects outside the function's environment:

g12 <- function() x + 1
x <- 15
g12()
x <- 20
g12()


# This behaviour can be quite annoying. If you make a spelling mistake in your code, you won't get an error message when you create the function. And depending on the variables defined in the global environment, you might not even get an error message when you run the function.

# To detect this problem, use codetools::findGlobals().
# This function lists all the external dependencies (unbound symbols) within a function:

codetools::findGlobals(g12)

# To solve this problem, you can manually change the function's environment to the emptyenv(), an environment which contains nothing:

environment(g12) <- emptyenv()

g12()

# The problem and its solution reveal why this seemingly undesirable behaviour exists: R relies on lexical scoping to find everything, from the obvious, like mean(), to the less obvious, like + or even {. This gives R's scoping rules a rather beautiful simplicity.




# 6.5 Lazy evaluation =========================================================

# In R, function arguments are lazily evaluated: they're only evaluated if accessed. For example, this code doesn't generate an error because x is never used:

h01 <- function(x) {
    10
  }

h01(stop("This is an error!"))

# This is an important feature because it allows you to do things like include potentially expensive computations in function arguments that will only be evaluated if needed.




# 6.5.1 Promises ==============================================================

# Lazy evaluation is powered by a data structure called a promise, or (less commonly) a thunk. It's one of the features that makes R such an interesting programming language (we'll return to promises again in Section 20.3).

# A promise has three components:
# A) An expression, like x + y, which gives rise to the delayed computation.
# B) An environment where the expression should be evaluated, i.e. the environment where the function is called. This makes sure that the following function returns 11, not 101:

y <- 10
h02 <- function(x) {
  y <- 100
  x + 1
}

h02(y)


# This also means that when you do assignment inside a call to a function, the variable is bound outside of the function, not inside of it.

h02(y <- 1000)

y


# C) A value, which is computed and cached the first time a promise is accessed when the expression is evaluated in the specified environment. This ensures that the promise is evaluated at most once, and is why you only see "Calculating." printed once in the following example.

x <- 20

double <- function(x) {
  message("Calculating...")
  x * 2
}

h03 <- function(x) {
  c(x, x)
}

h03(double(x))


# You cannot manipulate promises with R code. Promises are like a quantum state: any attempt to inspect them with R code will force an immediate evaluation, making the promise disappear. Later, in Section 20.3, you'll learn about quosures, which convert promises into an R object where you can easily inspect the expression and the environment.



# 6.5.2 Default arguments =====================================================

# Thanks to lazy evaluation, default values can be defined in terms of other arguments, or even in terms of variables defined later in the function:

h04 <- function(x = 1, y = x * 2, z = a + b) {
    a <- 10
    b <- 100
    c(x, y, z)
}


h04()


# Many base R functions use this technique, but I don't recommend it.
#
# It makes the code harder to understand: to predict what will be returned, you need to know the exact order in which default arguments are evaluated.
#
# The evaluation environment is slightly different for default and user supplied arguments, as default arguments are evaluated inside the function.
#
# This means that seemingly identical calls can yield different results. It's easiest to see this with an extreme example:

h05 <- function(x = ls()) {
    a <- 1
    x
  }


# ls() evaluated inside h05:
h05()

# ls() evaluated in global environment:
h05(ls())




# 6.6... (dot-dot-dot)  =======================================================
#
# Functions can have a special argument... (pronounced dot-dot-dot).
#
# With  it, a function can take any number of additional arguments.
#
# You can also use... to pass those additional arguments on to another function.

i01 <- function(y, z) {
  list(y = y, z = z)
}


i02 <- function(x, ...) {
  i01(...)
}


str(i02(x = 1, y = 2, z = 3))


# More useful is list(...), which evaluates the arguments and stores them in a list:

i04 <- function(...) {
    list(...)
  }

str(i04(a = 1, b = 2))

# (See also rlang::list2() to support splicing and to silently ignore trailing commas, and rlang::enquos() to capture unevaluated arguments, the topic of quasiquotation.)



# There are two primary uses of ..., both of which we'll come back to later in the book:
# If your function takes a function as an argument, you want some way to pass additional arguments to that function.
#
# In this example, lapply() uses ... to pass na.rm on to mean():

x <- list(c(1, 3, NA),
          c(4, NA, 6))

str(lapply(x, mean, na.rm = TRUE))



# If your function is an S3 generic, you need some way to allow methods to  take arbitrary extra arguments.
#
# For example, take the print() function.
#
# Because there are different options for printing depending on the type of  object, there's no way to pre-specify every possible argument and... allows  individual methods to have different arguments:

print(factor(letters), max.levels = 4)

print(y ~ x, showEnv = TRUE)



# 6.7 Exiting a function ======================================================
#
# Most functions exit in one of two ways29: they either return a value, indicating success, or they throw an error, indicating failure.
#
# This section describes return values (implicit versus explicit; visible versus invisible), briefly discusses errors, and introduces exit handlers, which allow you to run code when a function exits.



# 6.7.1 Implicit versus explicit returns ======================================

# There are two ways that a function can return a value:

# Implicitly, where the last evaluated expression is the return value:

j01 <- function(x) {
  if (x < 10) {
    0
  } else {
    10
  }
}
j01(5)

j01(15)

# Explicitly, by calling return():
j02 <- function(x) {
  if (x < 10) {
    return(0)
  } else {
    return(10)
  }
}




# 6.7.2 Invisible values ======================================================
#
# Most functions return visibly: calling the function in an interactive context prints the result.

j03 <- function() 1

j03()

# However, you can prevent automatic printing by applying invisible() to the last value:

j04 <- function() invisible(1)

j04()

# To verify that this value does indeed exist, you can explicitly print it or wrap it in parentheses:

print(j04())

(j04())


# Alternatively, you can use withVisible() to return the value and a visibility flag:

str(withVisible(j04()))

# The most common function that returns invisibly is <-:

a <- 2
(a <- 2)

# This is what makes it possible to chain assignments:

a <- b <- c <- d <- 2


# In general, any function called primarily for a side effect (like <-, print(), or plot()) should return an invisible value (typically the value of the first argument).




# 6.7.3 Errors ================================================================

# If a function cannot complete its assigned task, it should throw an error with stop(), which immediately terminates the execution of the function.

j05 <- function() {
  stop("I'm an error")
  return(10)
}

j05()


# An error indicates that something has gone wrong, and forces the user to deal with the problem. Some languages (like C, Go, and Rust) rely on special return values to indicate problems, but in R you should always throw an error. You'll learn more about errors, and how to handle them, in Chapter 8.



# 6.7.4 Exit handlers =========================================================

# Sometimes a function needs to make temporary changes to the global state.
#
# But having to cleanup those changes can be painful (what happens if there's an error?).
#
# To ensure that these changes are undone and that the global state is restored no matter how a function exits, use on.exit() to set up an exit handler.
#
# The following simple example shows that the exit handler is run regardless of whether the function exits normally or with an error.

j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)

  if (x) {
    return(10)
  } else {
    stop("Error")
  }
}


j06(TRUE)

j06(FALSE)


# Always set add = TRUE when using on.exit().
#
# If you don't, each call to on.exit() will overwrite the previous exit handler.
#
# Even when only registering a single handler, it's good practice to set add = TRUE so that you won't get any unpleasant surprises if you later add more exit handlers.
#
# on.exit() is useful because it allows you to place clean-up code directly next to the code that requires clean-up:

cleanup <- function(dir, code) {
  old_dir <- setwd(dir)
  on.exit(setwd(old_dir), add = TRUE)
  old_opt <- options(stringsAsFactors = FALSE)
  on.exit(options(old_opt), add = TRUE)
}


# Coupled with lazy evaluation, this creates a very useful pattern for running a block of code in an altered environment:

with_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  force(code)
}


getwd()

with_dir("~", getwd())

# The use of force() isn't strictly necessary here as simply referring to code will force its evaluation.
#
# However, using force() makes it very clear that we are deliberately forcing the execution. You'll learn other uses of force() in Chapter 10.


j08 <- function() {
  on.exit(message("a"), add = TRUE)
  on.exit(message("b"), add = TRUE)
}

j08()


# This can make cleanup a little tricky if some actions need to happen in a specific order; typically you want the most recent added expression to be run first.

j09 <- function() {
 on.exit(message("a"), add = TRUE, after = FALSE)
 on.exit(message("b"), add = TRUE, after = FALSE)
}

j09()




#  6.8 Function forms =========================================================
#
# To understand computations in R, two slogans are helpful:

# Everything that exists is an object.

# Everything that happens is a function call.


# While everything that happens in R is a result of a function call, not all calls look the same. Function calls come in four varieties:

#  prefix: the function name comes before its arguments, like foofy(a, b, c). These constitute of the majority of function calls in R.

# infix: the function name comes in between its arguments, like x + y. Infix forms are used for many mathematical operators, and for user-defined functions that begin and end with %.

# replacement: functions that replace values by assignment, like names(df) <- c("a", "b", "c"). They actually look like prefix functions.

# special: functions like [[, if, and for. While they don't have a consistent structure, they play important roles in R's syntax.


#  While there are four forms, you actually only need one because any call can be written in prefix form. I'll demonstrate this property, and then you'll learn about each of the forms in turn.




#  6.8.1 Rewriting to prefix form =============================================

# An interesting property of R is that every infix, replacement, or special form can be rewritten in prefix form.

# Doing so is useful because it helps you better understand the structure of the language, it gives you the real name of every function, and it allows you to modify those functions for fun and profit.

# The following example shows three pairs of equivalent calls, rewriting an infix form, replacement form, and a special form into prefix form.

# x + y
# `+`(x, y)

# names(df) <- c("x", "y", "z")
# `names<-`(df, c("x", "y", "z"))

# for(i in 1:10) print(i)
# `for`(i, 1:10, print(i))






# This provides a clean and elegant approach to writing domain specific languages and translators to other languages.

# A more useful application comes up when using functional programming tools. For example, you could use lapply() to add 3 to every element of a list by first defining a function add():

add <- function(x, y) x + y

lapply(list(1:3, 4:5), add, 3)


# But we can also get the same result simply by relying on the existing + function:
lapply(list(1:3, 4:5), `+`, 3)



# 6.8.2 Prefix form ===========================================================

# The prefix form is the most common form in R code, and indeed in the majority of programming languages. Prefix calls in R are a little special because you can specify arguments in three ways:

# By position, like help(mean).

# Using partial matching, like help(top = mean).

# By name, like help(topic = mean).

# As illustrated by the following chunk, arguments are matched by exact name, then with unique prefixes, and finally by position.

k01 <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}

str(k01(1, 2, 3))

str(k01(2, 3, abcdef = 1))

# Can abbreviate long argument names:
str(k01(2, 3, a = 1))

# But this doesn't work because abbreviation is ambiguous
str(k01(1, 3, b = 1))

#> Error in k01(1, 3, b = 1): argument 3 matches multiple formal arguments

# In general, use positional matching only for the first one or two arguments; they will be the most commonly used, and most readers will know what they are.
#
# Avoid using positional matching for less commonly used arguments, and never use partial matching. Unfortunately you can't disable partial matching, but you can turn it into a warning with the warnPartialMatchArgs option:

options(warnPartialMatchArgs = TRUE)

x <- k01(a = 1, 2, 3)




# 6.8.3 Infix functions =======================================================

# Infix functions get their name from the fact the function name comes inbetween its arguments, and hence have two arguments.

# R comes with a number of built-in infix operators: :, ::, :::, $, @, ^, *, /, +, -, >, >=, <, <=, ==, !=, !, &, &&, |, ||, ~, <-, and <<-.

# You can also create your own infix functions that start and end with %.

# Base R uses this pattern to define %%, %*%, %/%, %in%, %o%, and %x%.

# Defining your own infix function is simple. You create a two argument function and bind it to a name that starts and ends with %:

`%+%` <- function(a, b) paste0(a, b)

"new " %+% "string"


# You will need to escape any special characters in the string used to define the function, but not when you call it:

`% %` <- function(a, b) paste(a, b)
`%/\\%` <- function(a, b) paste(a, b)

"a" % % "b"

"a" %/\% "b"


# R's default precedence rules mean that infix operators are composed left to right:

`%-%` <- function(a, b) paste0("(", a, " %-% ", b, ")")

"a" %-% "b" %-% "c"

# There are two special infix functions that can be called with a single argument: + and -.

-1
+10



# 6.8.4 Replacement functions ================================================

# Replacement functions act like they modify their arguments in place, and have the special name xxx<-.

# They must have arguments named x and value, and must return the modified object. For example, the following function modifies the second element of a vector:

`second<-` <- function(x, value) {
  x[2] <- value
  x
}

# Replacement functions are used by placing the function call on the left side of <-:

x <- 1:10
second(x) <- 5L
x

# I say they act like they modify their arguments in place, because, as explained in Section 2.5, they actually create a modified copy. We can see that by using tracemem():

x <- 1:10

tracemem(x)

second(x) <- 6L

# If your replacement function needs additional arguments, place them between x and value, and call the replacement function with additional arguments on the left:

`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}

modify(x, 1) <- 10

x

# When you write modify(x, 1) <- 10, behind the scenes R turns it into:
x <- `modify<-`(x, 1, 10)

# Combining replacement with other functions requires more complex translation. For example:
x <- c(a = 1, b = 2, c = 3)
names(x)


names(x)[2] <- "two"

names(x)

# is translated into:

`*tmp*` <- x

x <- `names<-`(`*tmp*`, `[<-`(names(`*tmp*`), 2, "two"))

rm(`*tmp*`)

# (Yes, it really does create a local variable named *tmp*, which is removed afterwards.)



# 6.8.5 Special forms =========================================================

# Finally, there are a bunch of language features that are usually written in special ways, but also have prefix forms. These include parentheses:
# (x) (`(`(x))
# {x} (`{`(x)).
# The subsetting operators:
# x[i] (`[`(x, i))
# x[[i]] (`[[`(x, i))
# And the tools of control flow:
if (cond) true (`if`(cond, true))
if (cond) true else false (`if`(cond, true, false))
for(var in seq) action (`for`(var, seq, action))
while(cond) action (`while`(cond, action))
repeat expr (`repeat`(expr))
next (`next`())
break (`break`())
# Finally, the most complex is the function function:
function(arg1, arg2) {body} (`function`(alist(arg1, arg2), body, env))
# Knowing the name of the function that underlies a special form is useful for getting documentation: ?( is a syntax error; ?`(` will give you the documentation for parentheses.
 # All special forms are implemented as primitive functions (i.e. in C); this means printing these functions is not informative:
 `for`
 #> .Primitive("for")



# 7 Environments ==============================================================
# 7.1 Introduction ============================================================

library(rlang)


# 7.2.1 Basics

# To create an environment, use rlang::env(). It works like list(), taking a set of name-value pairs:

 e1 <- env(a = FALSE,
           b = "a",
           c = 2.3,
           d = 1:3)

# In base R: use new.env() to create a new environment.

e1
env_print(e1)
env_names(e1)
ls(e1)
ls(e1, all.names = TRUE)


# 7.2.3 Parents ===============================================================

e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)

env_parent(e2b)
env_parent(e2a)


e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)


# The ancestors of every environment eventually terminate with the empty environment.
# You can see all ancestors with env_parents():

env_parents(e2b)
env_parents(e2d)
env_parents(e2b, last = empty_env())


# In Base R use parent.env() to find the parent of an environment.
# No base function returns all ancestors.
parent.env(e2b)


# 7.2.4 Super assignment, <<- =================================================

# The ancestors of an environment have an important relationship to <<-.
#
# Regular assignment, <-, always creates a variable in the current environment.
#
# Super assignment, <<-, never creates a variable in the current environment,
# but instead modifies an existing variable found in a parent environment.


x <- 0

f <- function() {
  x <<- 1
}

f()

x

# If <<- doesn't find an existing variable, it will create one in the global environment.


# 7.3 Recursing over environments =============================================

# Let's create a function that returns its current environment and
# its caller environment:
fn <- function() list(current = current_env(), caller = caller_env())

# The current environment is an unique execution environment
# created when `fn()` was called. The caller environment is the
# global env because that's where we called `fn()`.
fn()

# Let's call `fn()` again but this time within a function:
g <- function() fn()

# Now the caller environment is also an unique execution environment.
# This is the exec env created by R for our call to g():
g()


where <- function(name, env = caller_env()) {

  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)

  } else if (env_has(env, name)) {
    # Success case
    env

  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}

where("yyy")
x <- 5
where("x")
where("mean")
e4a <- env(empty_env(), a = 1, b = 2)
e4b <- env(e4a, x = 10, a = 11)
where("a", e4b)
where("b", e4b)
where("c", e4b)



# 7.4 Special environments ====================================================
library(rlang)
# 7.4.1 Package environments and the search path ==============================

# Each package attached by library() or require() becomes one of the parents of the global environment.

search()

rlang::search_envs()


# 7.4.2 The function environment ==============================================

# A function binds the current environment when it is created.
#
# This is called the function environment, and is used for lexical scoping.
#
# Across computer languages, functions that capture (or enclose) their environments are called closures, which is why this term is often used interchangeably with function in R's documentation.
#
# You can get the function environment with fn_env():

y <- 1
f <- function(x) x + y
rlang::fn_env(f)

# Use environment(f) to access the environment of function f.

environment(f)

e <- rlang::env()
e$g <- function() 1
ls(e)



# 7.4.3 Namespaces ============================================================

# Every function in a package is associated with a pair of environments:
#   - the package environment, and
#   - the namespace environment.
#
# The package environment is the external interface to the package.
#
# It's how you, the R user, find a function in an attached package or with ::.
#
# Its parent is determined by search path, i.e. the order in which packages have been attached.
#
# The namespace environment is the internal interface to the package.
#
# The package environment controls how we find the function; the namespace controls how the function finds its variables.




# 7.4.4 Execution environments ================================================

# What will the following function return the first time it's run? What about the second?

g <- function(x) {
  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

# Think about it for a moment before you read on.

g(10)

# This function returns the same value every time because of the fresh start principle. Each time a function is called, a new environment is created to host execution.
#
# This is called the execution environment, and its parent is the function environment.

h <- function(x) {
  a <- 2
  x + a
}

y <- h(1)


# An execution environment is usually ephemeral; once the function has completed, the environment will be garbage collected. There are several ways to make it stay around for longer. The first is to explicitly return it:

h2 <- function(x) {
  a <- x * 2
  current_env()
}


e <- h2(x = 10)
env_print(e)
fn_env(h2)
current_env()



# 7.5 Call stacks
#
# The caller environment is accessed with rlang::caller_env().
#
# This provides the environment from which the function was called, and hence varies based on how the function is called, not how the function was created.
#
# As we saw above this is a useful default whenever you write a function that takes an environment as an argument.

# use in Base R: parent.frame()


# 7.5.1 Simple call stacks
# Let's illustrate this with a simple sequence of calls: f() calls g() calls h().

f <- function(x) {
  g(x = 2)
}

g <- function(x) {
  h(x = 3)
}

h <- function(x) {
  stop()
}

# The way you most commonly see a call stack in R is by looking at the traceback() after an error has occurred:

f(x = 1)

traceback()

# Instead of stop() + traceback() to understand the call stack, we're going to use lobstr::cst() to print out the call stack tree:

h <- function(x) {
  lobstr::cst()
}

f(x = 1)

#> ???
#> ??????f(x = 1)
#>   ??????g(x = 2)
#>     ??????h(x = 3)
#>       ??????lobstr::cst()
# This shows us that cst() was called from h(), which was called from g(), which was called from f().



# 7.5.2 Lazy evaluation =======================================================
#
# The call stack above is simple: while you get a hint that there's some tree-like structure involved, everything happens on a single branch.
#
# This is typical of a call stack when all arguments are eagerly evaluated.
#
# Let's create a more complicated example that involves some lazy evaluation.
#
# We'll create a sequence of functions, a(), b(), c(), that pass along an argument x.

a <- function(x) b(x)
b <- function(x) c(x)
c <- function(x) x

a(f())

# x is lazily evaluated so this tree gets two branches.
#
# In the first branch a() calls b(), then b() calls c().
#
# The second branch starts when c() evaluates its argument x.
#
# This argument is evaluated in a new branch because the environment in which it is evaluated is the global environment, not the environment of c().





# 8 Conditions ================================================================

library(rlang)


# 8.2.1 Errors ================================================================

# In base R, errors are signalled, or thrown, by stop():

f <- function() g()
g <- function() h()
h <- function() stop("This is an error!")

f()

# By default, the error message includes the call, but this is typically not useful (and recapitulates information that you can easily get from traceback()), so I think it's good practice to use call. = FALSE:

h <- function() stop("This is an error!", call. = FALSE)

f()
traceback()

# The rlang equivalent to stop(), rlang::abort(), does this automatically. We'll use abort() throughout this chapter, but we won't get to its most compelling feature, the ability to add additional metadata to the condition object, until we're near the end of the chapter.

h <- function() abort("This is an error!")
f()



# 8.2.2 Warnings ==============================================================

# Warnings, signalled by warning(), are weaker than errors: they signal that something has gone wrong, but the code has been able to recover and continue. Unlike errors, you can have multiple warnings from a single function call:

fw <- function() {
  cat("1\n")
  warning("W1")
  cat("2\n")
  warning("W2")
  cat("3\n")
  warning("W3")
}

# By default, warnings are cached and printed only when control returns to the top level:

fw()



# 8.2.3 Messages

# Messages, signalled by message(), are informational; use them to tell the user that you've done something on their behalf. Good messages are a balancing act: you want to provide just enough information so the user knows what's going on, but not so much that they're overwhelmed.
#
# message()s are displayed immediately and do not have a call. argument:

fm <- function() {
    cat("1\n")
    message("M1")
    cat("2\n")
    message("M2")
    cat("3\n")
    message("M3")
  }


fm()




# 8.3 Ignoring conditions =====================================================

f1 <- function(x) {
  log(x)
  10
}

f1("x")

# However, if you wrap the statement that creates the error in try(), the error message will be displayed35 but execution will continue:

f2 <- function(x) {
    try(log(x))
    10
  }

f2("a")




default <- NULL
try(default <- read.csv("possibly-bad-input.csv"), silent = TRUE)


suppressWarnings({
  warning("Uhoh!")
  warning("Another warning")
  1
})

suppressMessages({
  message("Hello there")
  2
})

suppressWarnings({
  message("You can still see me")
  3
})



# 8.4 Handling conditions


# tryCatch(
#
#   error = function(cnd) {
#     # code to run when error is thrown
#   },
#
#   code_to_run_while_handlers_are_active
#
# )
#
#
# withCallingHandlers(
#
#   warning = function(cnd) {
#     # code to run when warning is signalled
#   },
#
#   message = function(cnd) {
#     # code to run when message is signalled
#   },
#
#   code_to_run_while_handlers_are_active
#
# )



# 8.4.1 Condition objects =====================================================

library(rlang)
cnd <- catch_cnd(stop("An error"))
str(cnd)
conditionCall(cnd)




# 8.4.2 Exiting handlers ======================================================

# tryCatch() registers exiting handlers, and is typically used to handle error conditions.
#
# It allows you to override the default error behaviour.
#
# For example, the following code will return NA instead of throwing an error:

f3 <- function(x) {
  tryCatch(
    error = function(cnd) NA,
    log(x)
  )
}


f3("x")

# If no conditions are signalled, or the class of the signalled condition does not match the handler name, the code executes normally:

tryCatch(
  error = function(cnd)
    10,
  1 + 1
)


tryCatch(
  error = function(cnd)
    10,
  {
    message("Hi!")
    1 + 1
  }
)



# The handlers set up by tryCatch() are called exiting handlers because after the condition is signalled, control passes to the handler and never returns to the original code, effectively meaning that the code exits:

tryCatch(
  message = function(cnd)
    "There",
  {
    message("Here")
    stop("This code is never run!")
  }
)


# The protected code is evaluated in the environment of tryCatch(), but the handler code is not, because the handlers are functions. This is important to remember if you're trying to modify objects in the parent environment.

# The handler functions are called with a single argument, the condition object. I call this argument cnd, by convention. This value is only moderately useful for the base conditions because they contain relatively little data. It's more useful when you make your own custom conditions, as you'll see shortly.

tryCatch(
  error = function(cnd) {
    paste0("--", conditionMessage(cnd), "--")
  },
  stop("This is an error")
)


# tryCatch() has one other argument: finally.
#
# It specifies a block of code (not a function) to run regardless of whether the initial expression succeeds or fails. This can be useful for clean up, like deleting files, or closing connections.
#
# This is functionally equivalent to using on.exit() (and indeed that's how it's implemented) but it can wrap smaller chunks of code than an entire function.

path <- tempfile()

tryCatch(
  {
    writeLines("Hi!", path)
    # ...
  },
  finally = {
    # always run
    unlink(path)
  }
)



# 8.4.3 Calling handlers

tryCatch(
  message = function(cnd)
    cat("Caught a message!\n"),
  {
    message("Someone there?")
    message("Why, yes!")
  }
)

# the code is terminated once the exiting handler completes

withCallingHandlers(
  message = function(cnd)
    cat("Caught a message!\n"),
  {
    message("Someone there?")
    message("Why, yes!")
  }
)



# 8.6 Applications ============================================================

# The goal of this section is not to show every possible usage of tryCatch() and withCallingHandlers() but to illustrate some  common patterns that frequently crop up.




# 8.6.1 Failure value
# There are a few simple, but useful, tryCatch() patterns based on returning a value from the error handler. The simplest case is a wrapper to return a default value if an error occurs:


fail_with <- function(expr, value = NULL) {
  tryCatch(
    error = function(cnd)
      value,
    expr
  )
}


fail_with(log(10), NA_real_)

fail_with(log("x"), NA_real_)



# 9 Functionals ===============================================================

# 9.1 Introduction ============================================================

# A functional is a function that takes a function as an input and returns a vector as output
randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(sum)

# A common use of functionals is as an alternative to for loops.


# 9.2 My first functional: map() ==============================================
library(purrr)

triple <- function(x) x * 3

map(1:3, triple)



# 9.2.1 Producing atomic vectors ==============================================

# map_chr() always returns a character vector
map_chr(mtcars, typeof)

# map_lgl() always returns a logical vector
map_lgl(mtcars, is.double)

# map_int() always returns a integer vector
n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique)

# map_dbl() always returns a double vector
map_dbl(mtcars, mean)



# 9.2.2 Anonymous functions and shortcuts =====================================

map_dbl(mtcars, function(x) length(unique(x)))

# Anonymous functions are very useful, but the syntax is verbose. So purrr supports a special shortcut:
map_dbl(mtcars, ~ length(unique(.x)))

# This works because all purrr functions translate formulas, created by ~ (pronounced “twiddle”), into functions. You can see what’s happening behind the scenes by calling as_mapper():
as_mapper(~ length(unique(.x)))

# This shortcut is particularly useful for generating random data:
x <- map(1:3, ~ runif(2))
str(x)

x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

x
# Select by name
map_dbl(x, "x")

# Or by position
map_dbl(x, 1)

# Or by both
map_dbl(x, list("y", 1))

# You'll get an error if a component doesn't exist:
map_chr(x, "z")

# Unless you supply a .default value
map_chr(x, "z", .default = NA)



# 9.2.3 Passing arguments with ... ============================================











