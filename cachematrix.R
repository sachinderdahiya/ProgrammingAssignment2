## The following two functions are used to cache the inverse of a matrix
## On basic assumption that the matrix supplier is always invertible and using 
## solve(x) function of R.

## makeCacheMatrix creates a list containing a function to
## 1. set :set the value of the matrix
## 2. get : get the value of the matrix
## 3. setinverse: set the value of inverse of the matrix
## 4. getinverse: get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(parmx) {
        x <<- parmx
        invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
    	if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    	}
	data <- x$get()
    	inv <- solve(data)
	message("getting recalculated data.")
    	x$setinverse(inv)
    	return(inv)
}	
