## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly.  We will
## write a pair of functions that cache the inverse of a 
## matrix.
##   
## Take a square matrix: matrix(c(3,1,4,2),nrow=2,ncol=2)
##
##      [,1] [,2]
## [1,]    3    1
## [2,]    4    2
##
## Create a function to cache the inverse of the matrix 
## ie. makeCacheMatrix
## Create a function to retrieve the inverse of the matrix
## if it has already been calculated, else calculate 
## the inverse.
##
## TEST Example
##
## > mymatrix<-matrix(c(3,1,4,2),nrow=2,ncol=2)
## > mysolve<-makeCacheMatrix(mymatrix)
## > cacheSolve(mysolve)
## [,1] [,2]
## [1,]  1.0 -2.0
## [2,] -0.5  1.5


## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
