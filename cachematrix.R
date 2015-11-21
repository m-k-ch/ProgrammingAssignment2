## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix is a function of functions that retrieve or assign values
## The cacheSolve function checks if the inverse of the matrix exists. 
## If not then it computes it, otherwise it retrieves an existing value


## Write a short comment describing  this function
## the get function returns the value stored in the main function
## the set function replaces the value stored in the main function (X) with another value (y)
## the getinv function retrieves the inverse matrix of the matrix stored in minv
## the setinv function replaces the value of the matrix "minv" with the matrix "invert" 
## 

makeCacheMatrix <- function(x = matrix()) {

        minv <<- NULL
        
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(invert) minv <<- invert
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function
## "minv" is aasigned the output of the getinv function generated in the makeCacheMatrix function
## if no value is found the function gets the original matrix and calls the solve function to get its inverse
## it then assigns the value to "minv" and retuns the value


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting inverse matrix")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
        
        
        }
