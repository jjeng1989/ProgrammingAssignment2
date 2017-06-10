## This program's purpose is to set 
## and get matricies in and out of cache.
## This program also creates the matrix's 
##inverse after checking that the inverse is not in
## cache.

## This is a metafunction that creates the 
## getter setter functions. mat = matrix value; inv = inverse
makeCacheMatrix <- function(mat = matrix()) {
	mat_inv <- NULL
	set <- function(input) {
		mat <<- input
		mat_inv <<- NULL
	}
	get <- function() mat
	setInv <- function(inv) mat_inv <<- inv
	getInv <- function() mat_inv
	list(set = set, get = get,
	     setInv = setInv,
		 getInv = getInv)
}


## This function uses the functions in makeCacheMatrix to cache
## and output the inverse matrix.
cacheSolve <- function(x, ...) {
    mat_inv <- x$getInv()
	if(!is.null(mat_inv)) {
		message("getting cached data")
		return(mat_inv)
	}
	data <- x$get()
	mat_inv <- solve(data, ...)
	x$setInv(mat_inv)
	mat_inv
}
