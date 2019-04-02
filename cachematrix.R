## Below are two functions that are used to create a special object that stores matrix
# and cache's iverse of it.

##  "makeCacheMatrix" function creates a special matrix, which is really a list containing a function to
# set the value of the Matrix
# get the value of the Matrix
# set the value of the inverse of a Matrix
# get the value of the inverse of a Matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  set_inv_m <- function(inv_m) inv_m <<- inv_m
  get_inv_m <- function() inv_m
  list(set = set, get = get,
       set_inv_m = set_inv_m,
       get_inv_m = get_inv_m)
}

## "cacheSolve" function calculates the inverse of the matrix created with the "makeCacheMatrix" function. 
# If inverse of a matrix was alredy calculated function will return it from cache.
cacheSolve <- function(x, ...) {
  inv_m <- x$get_inv_m()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$set_inv_m(inv_m)
  inv_m
}
