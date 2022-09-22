## In this file, I am going to write two functions, one named
##  makeCacheMatrix that will create a matrix that caches its inverse
## and one named cacheSolve that will compute the inverse of this matrix but
## it will only retrieve it if it's already been calculated


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
get <- function() x
setsolve <- function(solve) inv <<- solve
getsolve <- function() inv
list(set = set, get = get,
     setsolve = setsolve, 
     getsolve = getsolve) ##now our matrix is there
}


## Next, the cacheSolve function that will compute the inverse of the matrix
## but will check if it's already been calculated, in which case it will retrieve it from the cache


cacheSolve <- function(x, ...) {
  inv <- x$getsolve() 
  if(!is.null(inv)) { ##here the function checks whether inv is NULL
    message("getting inversed matrix") ##in which case it gets the inverse of the matrix
    return(inv)  ##and returns it
  }
mat <- x$get()
inv <- solve(mat, ...)
x$setsolve(inv)
inv
}

