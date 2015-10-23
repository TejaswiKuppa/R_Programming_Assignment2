## The following functions makeCacheMatrix.R and cacheSolve.R cache
## and return the inverse of a given matrix.

## makeCacheMatrix creates a special matrix to cache the inverse of a given
## matrix.

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) { ##Function to set the value of the matrix
##This function is used to change the value of the matrix stored in the function.
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x ##Function to get the value of the matrix
        
        setinv <- function(matrix_inverse) x_inv <<- matrix_inverse  ##Function to set the value of the inverse                 
                                           
        getinv <- function() x_inv ##Function to get the value of the inverse 
        
        list(set = set, get = get,setinv = setinv,getinv = getinv)

}


## cacheSolve calculates the inverse of the matrix created in the function
## above and returns it in the console.

cacheSolve <- function(x, ...) {
        x_inv <- x$getinv()
        if(!is.null(x_inv)) { ##Check to see if inverse has already been calculated.
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get() ##Get the matrix to calculate it's inverse.
        
        x_inv <- solve(data, ...) ##Calculate the inverse.
        
        x$setinv(x_inv) ##Set the value of the inverse in the cache.
        
        x_inv ##Return inverse of matrix.
## Test: Performing matrix multiplication on x and x_inv should yield an identity matrix.
}
