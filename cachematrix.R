## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invMatx <- NULL
    set <- function(y) {
        x <<- y
        invMatx <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMatx <<- inverse
    getInverse <- function() invMatx
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

##ss
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatx <- x$getInverse()
    if (!is.null(invMatx)) {
        message("getting cached data")
        return(invMatx)
    }
    mat <- x$get()
    invMatx <- solve(mat, ...)
    x$setInverse(invMatx)
    invMatx
}
