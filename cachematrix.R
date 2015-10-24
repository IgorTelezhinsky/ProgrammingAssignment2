## The code implements the Programming Assignment 2, i.e.,
## a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                           ## sets the inverse to NULL upon creation of a new object
    
    ## this function sets new x if we want to change our matrix
    set <- function(y) {                           
        x <<- y                         ## the value of input y is assigned to x of makeCacheMatrix
        i <<- NULL                      ## the inverse is reset because we have new x
    }
    
    ## this function gets x (the matrix)
    get <- function() x
    
    ## this function sets i (the inverse of matrix x)
    setinverse <- function(inverse) i <<- inverse    ## the value of input inverse is assigned to i of makeCacheMatrix
    
    ## this function gets i (the inverse of matrix x)
    getinverse <- function() i
    
    list(set = set, get = get,          ## creates the list of functions within makeCacheMatrix
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()                 ## gets the inverse of x from the cache "special matrix" object x
    if(!is.null(i)) {                   ## checks if the inverse is not NULL (is already computed)
        message("getting cached data")  
        return(i)                       ## if true returns the cached inverse and exits cacheSolve function
    }
    data <- x$get()                     ## else gets the data of x,
    i <- solve(data, ...)               ## computes its inverse 
    x$setinverse(i)                     ## and stores the inverse in the cache of x
    i                                   ## returns computed inverse and exits cacheSolve function
}

