## Qin Wang, May 14, 2014
## Caching the Inverse of a Matrix

## creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    	inverse <- NULL
    	set <- function(y) {
        	x <<- y
        	inverse <<- NULL
    	}
    	get <- function() x
    	setinverse <- function(imatrix) inverse <<- imatrix
    	getinverse <- function() inverse
    	list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)    
}

## computes the inverse of the special matrix returned by makeCacheMatrix above

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
