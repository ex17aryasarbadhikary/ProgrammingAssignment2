## cachematrix.R
## created on: 08-Aug-2018
## created by: Arya Kishore Sarbadhikary
## description: this program caches the inverse of a matrix rather than computing it repeatedly 
## functions do: makeCacheMatrix()- creates a special "matrix" object that can cache its inverse
## cacheSolve()- computes the inverse of the special "matrix" returned by makeCacheMatrix()

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if (!is.null(i)) {
	  message("getting cached data")
	  return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
