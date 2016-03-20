## These functions (1) create an object for handling a matrix with a cached 
## inverse (with functions and associated environment for creating the matrix, 
## returning the matrix, getting the matrix's inverse, and setting the matrix's 
## inverse) and (2) getting the inverse of a 'cached' matrix (i.e. checking if
## inverse has already been cached and, if not, calculating it)
## 

## Sets up a set of functions to handle creating and storing and cached matrix, 
## as well as getting and storing its inverse. Also provides environmental in 
## which the matrix and its inverse are stored.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(matrixToCreate) {
          x <<- matrixToCreate
          inv <<- NULL
        }
        get <- function() x
        setinv <- function(invToCache) inv <<- invToCache
        getinv <- function() inv
        list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
  
}


## Returns inverse of matrix by first looking to see if the inverse is already
## defined (will be in the environment created by the call to makeCacheMatrix
## that created x) and, if not, computing the inverse.

cacheSolve <- function(x, ...) {
  
    # Grab x's inverse from the envirment getinv was created
    inv <- x$getinv()
    
    # if inverse exists, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, grab x, compute its inverse, and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    return(inv)
}