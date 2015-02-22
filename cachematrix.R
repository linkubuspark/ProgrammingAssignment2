## The following calculates the inverse of the special "matrix" created with the below functions. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL
	set <- function(y) {
		x <<- y
		inverseMatrix <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inverseMatrix <<- inverse
	getinverse <- function() inverseMatrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inverseMatrix <- x$getinverse()
	if(!is.null(inverseMatrix)) {
		message("getting cached inverse")
		return(inverseMatrix)
	}
	data <- x$get()
	inverseMatrix <- solve(data, ...)
	x$setinverse(inverseMatrix)
	inverseMatrix
}
