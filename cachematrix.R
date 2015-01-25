## Computing the inverse of a square matrix and saving the result to the cache. 
## The following functions assume that the matrix supplied is always invertible.
##  Inverse of a square matrix is computed using solve function in R



## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                ##  If the matrix in the cache is not identical to the input matrix 
                ##  then  resetting corresponding cached inverse matrix value to null(s <<-null)  
                if(!identical(get(),y) ){
                        x <<- y
                        s <<- NULL
                }        
        }
        get <- function() x
        setInvMatrix <- function(invMatrix) s <<- invMatrix
        getInvMatrix <- function() s
        list(set = set, get = get,
        setInvMatrix = setInvMatrix,
        getInvMatrix = getInvMatrix)
}


## CacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInvMatrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInvMatrix(s)
        s
        
}
