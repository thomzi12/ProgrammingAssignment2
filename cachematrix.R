## Write the following functions:
## 
## This is a straightforward transformation of the given mean function in the assignment 
## description to handle matrices and their inverses using the cache.
{##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
}

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    get_mat <- function() x 
    set_inv <- function(inv) i <<- inv
    get_inv <- function() i
    list(set = set, get_mat = get_mat, set_inv = set_inv, get_inv = get_inv)
}

cacheSolve <- function (x, ...){
  i <- x$get_inv()
  if (!is.null(i)){
      message("Getting cached data")
      return(i)
  }
  data <- x$get_mat()
  i = solve(data, ...)
  x$set_inv(i)
  i 
}