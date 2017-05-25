## makeCacheMatrix
##   Creates a special "matrix" object that can cache its inverse.
## cacheSolve
##   Computes the inverse of the special "matrix" returned by
##   makeCacheMatrix above.
##   Returns the cached inverse rather than recomputing it
##   if the inverse has already been calculated
##   and the matrix has not changed. 

## Receive matrix argument.
## Initialize inverse as null.
## Create four functions and return as named list:
##   set:    receives a matrix argument and assigns to x
##   get:    returns the current matrix x when called
##   setinv: receives inverted matrix and assigns to inv
##   getinv: returns the inverted matrix inv when called

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(
        setit = set,
        getit = get,
        setinvit = setinv,
        getinvit = getinv)
}

## Receive object defined previously by makeCacheMatrix.
## Look for presence of cached value and return it if there.
## Only if no cached value, calculate and return inverse.

cacheSolve <- function(x, ...) {
    inv2 <- x$getinvit()
    if (!is.null(inv2)) {
        message("Retreiving cached matrix...")
        return(inv2)
    }
    mat <- x$getit()   #get the matrix
    inv2 <- solve(mat) #solve the matrix for inverse
    x$setinvit(inv2)   #cache the inverse
    inv2               #return the inverse
}
