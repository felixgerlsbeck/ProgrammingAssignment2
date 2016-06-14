## makeCacheMatrix() and cacheSolve() take a matrix as input, 
## calculate its inverse, and store the result in the cache. If
## cacheSolve() is called again, it takes the value out of the cache
## rather than calculating it again.

## makeCacheMatrix creates four functions that (1) set the value
## of x, (2) return the value of x, (3) set the values of the inverse 
## of x, and (4) return the value of the inverse of x


source("cachematrix.R")
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inv <<- i
        getinverse <- function() inv
        list (set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse)
}

## cacheSolve checks whether the inverse of x has been set to a value.
## that is not NULL. If not, it calculates the value of inverse of x 
## and sets it to that value. If it has been already set, cacheSolve 
## gets and returns that value directly (printing 'getting cached data').

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                inv
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
