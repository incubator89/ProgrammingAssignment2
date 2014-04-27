## Below are my two functions that are used to create a special object
## that stores a matrix and caches its inversion

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inversion of the matrix
## 4. get the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
# 1.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
# 2.
        get <- function() x
# 3.
        setinv <- function(solve) inv <<- solve
# 4.
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv ,
             getinv = getinv )
}


## The second function inverts the special "matrix" created with
## the above function. However, it first checks to see if the inverse
## matrix has already been computed. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, it performs
## the inversion of the matrix and sets the inverse matrix in the cache
## via the setmean function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinv(inv)
        inv
}
