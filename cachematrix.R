## these functions invert a square matrix and cache the results. The cached 
## results are returned until the matrix is reset, at which point the matrix
## is inverted and the results re-cached.


## makeCacheMatrix caches and retrieves matrix x via set and get, and caches and retrieves
## the inverted matrix via setsolve and getsolve. x$set(x) resets matrix x.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns the previously inverted and cached matrix if m is not null. 
## Otherwise the matrix to be inverted is retrieved and the inverted value is 
## calculated and cached. This inverted value is then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
