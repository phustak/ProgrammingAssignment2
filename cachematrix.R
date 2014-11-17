## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse. 
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing functions to 


## Function makeCacheMatrix
## Initializes data structure to hold, set and get a matrix and its inverse. 
## parameter x - optional initial matrix data. If not specified, the 
##                 structure will initialize with an empty matrix,
##                 and the data can be added later using the set function
##
## The return is a list with 4 elements which are the following functions ...
##  1.set the value of the matrix (currently cached inverse matrix is deleted)
##  2.get the value of the matrix
##  3.set the value of the inverse
##  4.get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve
##   Calculates or retrives a cached value of the inverse of the matrix 
##   represented by the data structure initialized by the function 
##   makeCacheMatrix
## parameter x - required data structure containing the original and/or \
##  the cached inverse of the matrix
## parameter ... - any additional optional parameters for the solve function
##
## Returns the inverse of the matrix. If a cached version exists, it is 
## returned directly, otherwise it is calculated, stored and returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
