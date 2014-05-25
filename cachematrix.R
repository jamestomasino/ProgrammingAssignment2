## makeCacheMatrix
##   creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	# Reset local vars
	m <- NULL

	## set x
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	## get x
	get <- function() {
		x
	}

	## setInverse stores inverse result into var 'm'
	setInverse <- function(inverse) {
		m <<- inverse
	}

	## getInverse gets the current inverse val
	getInverse <- function() {
		m
	}

	## return
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve
##   Calculates the inverse of a matrix, or gets cache if it exists
cacheSolve <- function(x, ...) {

	## Check for cached value
	m <- x$getInverse()

	## if cached value is not null, use it
	if(!is.null(m)) {
		return(m)
	}

	## not cached, lets do it for real
	newM <- x$get()

	## Actually calculate the inverse of x, store in 'm'
	m <- solve(newM, ...)

	## cache result
	x$setInverse(m)

	## return
	m
}
