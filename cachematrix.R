## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. Hereafter, you can find a pair
## of functions that cache the inverse of a given matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (x = matrix()) {
        
        inv_mat <- NULL
        
        set <- function (y) {
          x <<- y
          inv_mat <<- NULL
        }
        
        get <- function () x
        setinversemat <- function (inversematrix) inv_mat <<- inversematrix
        getinversemat <- function () inv_mat
        list (set = set, get = get, setinversemat = setinversemat, getinversemat = getinversemat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x, ...) {
        
        inv_mat <- x$getinversemat ()
        
        if (!is.null(inv_mat)) {
                message ("getting cached data...")
                return (inv_mat)
        }
        
        my_data <- x$get ()
        inv_mat <- solve (my_data)
        x$setinversemat (inv_mat)
        inv_mat
}
