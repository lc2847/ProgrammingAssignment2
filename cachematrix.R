## makeCacheMatrix creates a special matrix, which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse of the Matrix
## get the value of the Inverse of the Matrix 
 
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinv<- function(inverse) inv_x <<-inverse
    getinv <- function() inv_x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
 
## The function cacheSolve calculates the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve skips the calculation and return the cashed one.
##If not, it calculates, caches, and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinv()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinv(inv_x)
        return(inv_x)
    }
}