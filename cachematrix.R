## The two functions in this R script compute the inverse of a matrix and cache it 
## in order to return the cached version in subsequent calls and thus avoid costly computation

## The "makeCacheMatrix()" function creates a special "matrix" object that can cache its inverse. 
## It works similar to a class in an object-oriented programming language

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        getMatrix <- function() x
        
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        
        list (setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)

}


## The "cacheSolve()" function computes the inverse of a special "matrix" previously created with
## the "makeCacheMatrix()" function above. If the inverse has been already calculated, it retrieves
## it from the cache; otherwise, it computes the inverse and immediately caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if (!is.null(inv)) {
                message("retrieving cached inverse matrix")
                return (inv)
        }
        
        mat <- x$getMatrix()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
