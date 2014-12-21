## The following two functions work together to create a special 'matrix'
## object that can cache the results of computing the inverse of the matrix.
## The two functions are:
##   makeCacheMatrix: Creates an object that can cache the inverse of a given
##                    matrix.
##   cacheSolve:      Given an object created by makeCacheMatrix, return the 
##                    inverse of a given matrix.


## makeCacheMatrix function. Returns an object that can cache the inverse of 
## a given matrix. The object returned by this method has 4 methods as follows:
##	set:        set the matrix for which the inverse will be computed and cached.
##	get:        returns the matrix for this the inverse is cached
##	setinverse: sets the (cached) inverse of the given matrix
##	getinverse: returns the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(i) inv <<- i
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve function. Returns the inverse of a matrix object created via makeCacheMatrix.
## Uses the solve function to compute the inverse one time. Subsequent calls using the same
## matrix return the cached results.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		##message("getting cached data") # uncomment this line for testing
		return(inv)
	}
	data <- x$get()
      inv <- solve(data) %*% data
	x$setinverse(inv)
	inv
}