## 1. makeCacheMatrix: The benefit of this function is that it can compute from 
## the cashe instead of doing it repeatedly (which might not always be possible 
## whith large amounts of data). 

## This function creates a special “matrix” object that 
## can cache its inverse. I also assumes that the matrix 
## supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrixInverse) inv <<- solveMatrixInverse
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2. cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message( "Your humble servant is now getting the cached data, Sir.")
                return(inv)
        }
        theMatrix <- x$get()
        inv <- solve(theMatrix, ...)
        x$setInverse(inv)
        inv
}