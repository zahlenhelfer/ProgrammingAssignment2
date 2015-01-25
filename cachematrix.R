## Put comments here that give an overall description of what your
## functions do

## This function create a special "matrix" object
## it can cache it´s inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special "matrix"
## it took the return by 'makeCacheMatrix' above.
## When inverse has already been calculated
## then 'cacheSolve' will retrieve the inverse from cache.
## PS: cache only when the matrix has not changed!

cacheSolve <- function(x, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("get the cached data")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse) ## Return a matrix whic is the inverse of 'x'
}
