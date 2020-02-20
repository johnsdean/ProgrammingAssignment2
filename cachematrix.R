# This function creates a list that holds functions that
# cache a matrix.

# Using tidyverse style guide, except for the functions
# whose names were specified in the assignment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse)
} # end makeCacheMatrix

## This function returns the inverse of the given matrix.
## If the matrix's inverse has already been cached, return it.
## Otherwise, calculate the inverse, cache it, and return it.

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
} # end cacheSolve

# Test the functions, including an inverse of an inverse

cm1 <- makeCacheMatrix()
cm1$set(matrix(c(4,2,7,6), 2, 2))
print(cm1$get())
cm1_inverse <- cacheSolve(cm1)
cm1_inverse <- cacheSolve(cm1) # prints cached message
print(cm1_inverse)

cm2 <- makeCacheMatrix()
cm2$set(matrix(c(1,0,2,2), 2, 2))
print("cm2 = ")
print(cm2$get())
cm2_inverse <- cacheSolve(cm2)
cm2_inverse <- cacheSolve(cm2) # prints cached message
print("Inverse of cm2 = ")
print(cm2_inverse)

cm3 <- makeCacheMatrix()
cm3$set(cm2_inverse)
print("Inverse of the inverse of cm2 =")
print(cacheSolve(cm3))