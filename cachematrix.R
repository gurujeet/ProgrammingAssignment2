## These functions work together to cache the inverse of a matrix so that the inverse need only be calculated once.
## The solve function is used to calculate the matrix inverse.

## Usage:
## For an invertable matrix, m, the following call will return a list to x:
## x <- makeCacheMatrix(m)
## The inverse of m, mi, can then be calculated with this call:
## mi <- cacheSolve(x)
## The inverse is calculated on the first call to cacheSolve, and also cached in x. Subsequent calls to cacheSolve(x)
## will return the cached value.

## makeCacheMatrix takes an invertable matrix as an argument. It returns a list that can be used to calculate the matrix inverse and
## cache it. The first time the list is used, the inverse is calculated using the solve function. Subsequent usage returns the
## cached calculation.

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) mi <<- solve
    getinverse <- function() mi
    list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve takes a list as an argument that was created by a call to makeCacheMatrix. It checks to see if the matrix
## inverse has already been cached using the getinverse function stored in the list. If so, it returns the cached value. 
## If not, it retrieves the data for the matrix using the get function stored in the list. It calculates the inverse and 
## caches it using the setinverse function in the list, and then returns the inverse.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- x$getinverse()
    if (!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setinverse(mi)
    mi
}
