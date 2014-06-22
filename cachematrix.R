## These functions cache the inverse of the matrix and compute
## the matrix inverse by first checking if it is cached 
## and solving for the inverse only if not cached

## This is the first function which creates the cache of the matrix inverse
## It is actually a list of 4 functions which set and retrieve matrix 
## to be inversed and set and retrieve the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
    
        get <- function() x
    
        setinv <- function(solve) inverse <<- solve
        getinv <- function() inverse
        list(set = set, get = get,
         setinv = setinv, 
         getinv = getinv)

}

## This function computes the inverse of the matrix
## It first checks the cache for the inverse if it exists (using is.null)
## If not, then it solves for inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)){
                message("Getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse    
}
