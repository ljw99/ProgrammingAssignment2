## Testing the vector/mean sample code by changing vector to matrix and 
## mean to solve worked for the matrix/solve assignment.
## The vector/mean sample code function names are modified to indicate and 
## provide matrix/solve functionality.
## Two functions are used to create a special "matrix" object and 
## cache its inverse.  The first builds the functions and cacheSolve
## uses the functions to compute the inverse of the special 
## "matrix" if not already calculated and cached (and matrix has not
## changed) else it retrieves the inverse from the cache.

## makeCacheMatrix creates four functions
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inv
##	4. get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve uses the functions created in first function to:
## 	-create the matrix in a special environment
##	-compute the inverse if not stored in cache
##	-return the matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
