## Matrix inversion is usually a costly computation and there may be some benefit  
## to caching the inverse of a matrix rather than compute it repeatedly.  
## The following functions will creates a special "matrix" object that can cache its inverse. 



## This function returns a list containing a function to  
## set the value of the matrix 
## get the value of the matrix 
## set the value of the inverse of the matrix 
## get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) 
{
# initialize the stored inverse value to NULL
    i <- NULL
    
# set value of the matrix
    set <- function(y) {
       x <<- y
       i <<- NULL
    } 
    
# get value of matrix
    get <- function() {
    x
    }
   
# set inverse of matrix 
    setInverse <- function(inv) i <<- inv
    
# get inverse of matrix
    getInverse <- function() i
  
# return a list containing all functions defined above  
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve: The function computes the inverse of the special "matrix"  
## returned by makeCacheMatrix above. If the inverse has already been calculated  
## (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
#   get inverse 
    i <- x$getInverse()
    
# if inverse exists, check if already cached
# if yes, return cached inverse
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    
# if not, get matrix
    m <- x$get()
    
# compute inverse of matrix
    i <- solve(m, ...)
    
# cache inverse of matrix
    x$setInverse(i)
    
# return inverse
    i
}
    

