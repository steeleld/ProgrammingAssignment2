## Coursera Programming Assignment 2
## Course identifier: rprog-035
## Overall assignment is to provide a program that can cache
## time-consuming computations as an example of R lexical scoping.

## Program will cache the inverse of a matrix to parent environment and cache. 

## subprogram 1: makeCacheMatrix
## makeCacheMatrix creates a special "matrix" object than can cache its inverse
        
makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
        
## subprogram 2: cachesolve
## cachesolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.  If inverse already calculated (and matrix not
## changed since then), then cacheSolve retrieves the already calculated value.
        
cachesolve <- function(m, ...) {
        inv <- m$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        datasolve <- m$get()
        inv <- solve(datasolve, ...)
        m$setsolve(inv)
        inv
}
