## This function creates a special "matrix" object that can cache its
## inverse.

## Call it with something like m<- makeCacheMatrix()
## Then set the matrix values using m$set(.....)
## Retrieve the stored values using m$get()

makeCacheMatrix <- function(x = matrix()) {
        ## Clear the inverse matrix.
        matrix.inverse <- NULL
        
        ## If called with set, then store the passed matrix values 
        ## and set the inverse matrix to NULL.
        set <- function(y) {
                x <<- y
                matrix.inverse <<- NULL
        }
        
        ## If called with get, then return the stored matrix values
        ## from x
        get <- function() x
        
        ## If called with setinverse, compute the inverse and store it to
        ## matrix.inverse
        setinverse <- function(m.inverse) matrix.inverse <<- m.inverse
        
        ## If called with getinverse, retrieve the cached values for the 
        ## matrix inverse.
        getinverse <- function() matrix.inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Retrieve the inverse matrix. (You'll get NULL if it is not stored)
        matrix.inverse <- x$getinverse()
        
        ## If it was stored, say so and return the cached value.
        if(!is.null(matrix.inverse)) {
                message("getting cached data")
                return(matrix.inverse)
        }
        
        ## Retrieve the saved matrix values
        data <- x$get()
        
        ## Compute the inverse of the saved matrix.
        matrix.inverse <- solve(data, ...)
        
        ## Save the calculated values for the matrix inverse. 
        x$setinverse(matrix.inverse)
        
        ## Return the matrix inverse.
        matrix.inverse
}
