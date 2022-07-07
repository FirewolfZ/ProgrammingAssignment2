## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 
## The following pair of functions cache the inverse of a matrix.

# makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
 ## This function creates a special "matrix" object that can cache its inverse.
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
        }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
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
    
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    print(i)
}
