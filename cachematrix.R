## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 
## The following pair of functions cache the inverse of a matrix.

# makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
 ## This function creates a special "matrix" object that can cache its inverse.
    
  
    i <- NULL ## Initialize the inverse property
 
    set <- function(y) { ## Method to set the matrix
        x <<- y
        i <<- NULL
        }
 
    get <- function() x  ## Method to get the the matrix
 
    setinv <- function(solve) i <<- solve ## Method to set the inverse of the matrix
 
    getinv <- function() i ## Method to get the inverse of the matrix
 
    ## Return a list of the methods
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve function

cacheSolve <- function(x, ...) {
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve will retrieve the inverse from the 
## cache.
    
    i <- x$getinv() ## Return a matrix that is the inverse of 'x'
 
    if(!is.null(i)) {  ## Just return the inverse if its already set
        message("getting cached data")
        return(i)
    }
 
    data <- x$get() ## Get the matrix from our object
 
    i <- solve(data, ...) ## Calculate the inverse 
 
    x$setinv(i) ## Set the inverse to the object
    print(i)
}
