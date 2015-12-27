## Coursera Programming Assignment 2
## Course identifier: rprog-035
## Overall assignment is to provide a program that can cache
## time-consuming computations as an example of R lexical scoping.

## Program will cache the inverse of a matrix. 
cachematrix <- function() {

        ## makeCacheMatrix creates a special "matrix" object than can cache its inverse
        
        makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- mean
                getsolve <- function() m
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
        }
        
        ## cacheSolve computes the inverse of the special "matrix" returned by 
        ## makeCacheMatrix above.  If inverse already calculated (and matrix not
        ## changed since then), then cacheSolve retrieves the already calculated value.
        
        cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                m <- x$getsolve()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
                m
        }
}