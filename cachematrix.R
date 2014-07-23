
## A pair of functions that cache the inverse of a matrix

##  This function creates a list that provides these functions:
### . get :    the value of the stored matrix is returned
### . getInv : the inverse matrix is returned, it can be NULL if it has 
###            not already been calculated
### . set :    the matrix is updated/stored, 
###            and inverse matrix is reset to NULL
### . setInv : the inverse matrix is updated/stored, it is originally created to
###            be used by cacheSolve function, but it is also provided to CLI.
###            If this function is used by CLI, no checking of the inverse 
###            matrix correctness is done.


makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
  
        set <- function(y) {

                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInv <- function(input) inverse <<- input
        getInv <- function() inverse

        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function returns the inverse matrix of the matrix stored in list 
## created with previous function.
## The inverse matrix is calculated in case it has not been calculated yet,
## otherwise it is recovered from cache, in order to save computation costs.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getInv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}
