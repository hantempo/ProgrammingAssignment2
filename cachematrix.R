## The following pair of functions are used to cache caculated inverses for matrix,
## since matrix inversion is usually a costly computation.

## Creates a special "matrix" object that can cache its inverse.
## Return a list containing functions:
## 1. set the value of the matrix;
## 2. get the value of the matrix;
## 3. set the value of the cached inverse;
## 4. get the value of the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the unsolved cached inversion to be NULL
        inverse <- NULL
        set <- function(y) {
                ## Set the matrix and the cached inverse in the parent environment
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        ## Set the cached inverse in the parent environment
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        ## Retrieve the matrix value and calculate its inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        ## Save up the inverse
        x$setinverse(inv)
        inv
}
