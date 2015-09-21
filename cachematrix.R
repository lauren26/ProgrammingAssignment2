## Matrix inversion is usually a costly computation and there may be some benefit  
## to caching the inverse of a matrix rather than compute it repeatedly.  
## The following functions will creates a special "matrix" object that can cache its inverse. 



## This function returns a list containing a function to  
## set the value of the matrix 
## get the value of the matrix 
## set the value of the inverse of the matrix 
## get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
## Write a short comment describing this function

## cacheSolve: The function computes the inverse of the special "matrix"  
## returned by makeCacheMatrix above. If the inverse has already been calculated  
## (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
    

