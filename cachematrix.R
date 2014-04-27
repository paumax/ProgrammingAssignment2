## These functions act in conjuction to cache 
## the inverse of a matrix

## makeCacheMatrix takes a matrix as argument and
## returns 4 functions wrapped in a list.
## The list acts as a repository for the matrix
## and its inverse.
## The function caches the matrix and makes space to
## cache its inverse (it does not calculate the inverse).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y){
        x <<- y
        m <<-NULL        
    }
    getMatrix <- function() x 
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = setMatrix, get = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)    
}


## cacheInverse takes a makeCachematrix object (a list) and
## returns the inverse of the matrix cached inside the list:
## - if the inverse is already cached, it returns it.
## - otrhewise, it calculates the inverse, sets it, and
##   returns it.

cacheInverse <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
