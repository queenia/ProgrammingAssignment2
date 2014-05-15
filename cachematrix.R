## Caching the Inverse of a Matrix

## creates a matrix object that can cache its inverse
# makeCacheMatrix contains four functions

makeCacheMatrix <- function(x = matrix()) {
    	inverse <- NULL

	# function 1: set the value of the matrix
    	set <- function(y) {
        	x <<- y
        	inverse <<- NULL
    	}
    	
	# function 2: get the value of the matrix
	get <- function() x

	# functions 3: set the value of the inversed matrix
    	setinverse <- function(imatrix) inverse <<- imatrix

	# function 4: get the value of the inversed matrix
    	getinverse <- function() inverse

    	list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)    
}

## computes the inverse of the special matrix returned by makeCacheMatrix above
## cacheSolve first checks to see if the inversed matrix has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    	inverse <- x$getinverse()
        if(!is.null(inverse)) {
	        message("getting cached inverted matrix")
	        return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
