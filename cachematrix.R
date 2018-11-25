## makeCacheMatrix created a special matrix object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invx <<- inverse
        getinverse <- function() invx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the incerse of the special matrix returned by makeCacheMatrix above. If the incerse has already been calculated (and the matrix has not changes), then the cachesolve should retriece the incerse from the cache

cacheSolve <- function(x, ...) {
        invx <- x$getinverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinverse(invx)
        invx
}
