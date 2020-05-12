## Johan Vásquez Mazo
## Universidad Nacional de Colombia - Sede Medellín


## makeCacheMatrix takes a matrix as input and outputs a list of four functions:
# makeCacheMatrix$set, which lets the user change the matrix, thus deleting the cache, if any, of its inverse.
# makeCacheMatrix$get, which retrieves the matrix.
# makeCacheMatrix$setinverse, which is to be used by cacheSolve and set the inverse of the matrix.
# makeCacheMatrix$getinverse, which retrieves the inverse of the matrix.
## Additionally, two values are defined:
# x, the matrix.
# inv, the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(A) {
        x <<- A
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes an object of the type returned by makeCacheMatrix as input and returns the inverse of the matrix,
# either by getting its cached data or by calculating it if the stored inverse is null. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
