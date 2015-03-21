## cached solver of a matrix inversion computation
## first create a cache matrix class via makeCacheMatrix()
## the matrix value can either be supplied in constructor call
## or via the class' set method
## get the inverse by the cacheSolve call
## note a regular matrix will be returned as the result

## makeCacheMatrix class
## regular matrix value can be supplied in the makeCacheMatrix call
## or via the class' set method
## get method will return the reulgar matrix value

makeCacheMatrix <- function(x = matrix()) {
	# private data member to store the inverse
	inv <- NULL

	set <- function(y) {
		# Do NOT reset inv if x and y have the same value
		if (!(is.matrix(x) && is.matrix(y) &&
		    dim(x) == dim(y) && all(x == y))) {
			inv <- NULL
		}
		x <<- y
	}
	get <- function() { x }
	setinverse <- function(inverse) { inv <<- inverse }
	getinverse <- function() { inv }

	list(set = set, get = get,
	     setinverse = setinverse, getinverse = getinverse)
}

## solve the inverse of the cache matrix class from above
## skip and return the cached inverse value if found from previous computation

cacheSolve <- function(x, ...) {
	## Return a regular matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
	}
	else {
		inv <- solve(x$get(), ...)
		x$setinverse(inv)
	}
	inv
}
