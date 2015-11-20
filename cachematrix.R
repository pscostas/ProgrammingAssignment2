##Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly This is a pair of unctions 
##that cache the inverse of a matrix.


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() {s}
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix, previously defined. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data for inverse of matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
        s
}

