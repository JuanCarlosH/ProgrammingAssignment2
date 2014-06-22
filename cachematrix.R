## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#==============================================================================#
#------------------------ Function 1 makeCacheMatrix---------------------------#
#------- Creates a special "matrix" object that can cache its inverse ---------#
#==============================================================================#

makeCacheMatrix <- function(x = matrix()) {
        k  <- NULL
        set  <- function(y){
                x <<- y
                k <<- NULL
        }
        get  <- function() x
        setinverse  <- function(inverse) k  <<- inverse
        getinverse  <- function() k
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
#==============================================================================#
#------------------------ Function 2 cacheSolve -------------------------------#
## Computes the inverse. If the inverse has already been calculated (and the   #
## matrix has not changed), then the cachesolve should retrieve the inverse    #
## from the cach --------------------------------------------------------------#
#==============================================================================#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        k  <- x$getinverse()
        if (!is.null(k)){
                message("getting cached data")
                return(k)
        }
        data  <- x$get()
        k  <- solve(data, ...)
        x$setinverse(k)
        k
}
