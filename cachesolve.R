# Subprogram to solve (find inverse of) matrix, separate program for testing
cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <<- getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        dataMatrix <- getMatrix()
        m <- solve(dataMatrix, ...)
        m
}