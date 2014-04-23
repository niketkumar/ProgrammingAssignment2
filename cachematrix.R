## makeCacheMatrix converts a given matrix into a cacheable one and
## cacheSolve is a wrapper function which should be called to calculate 
## inverse of a matrix. The later function calculates matrix inverse only
## if it is not already calculated and cached.

## Converts a given matrix into a cacheable one which works in tandem with cacheSolve(.)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Invokes the costly function, resolve() if it is not already calculated and cached.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(is.null(i)){
		m <- x$get()
		i <- solve(m, ...)
		x$setinverse(i)
		return(i)
	}else{
		message("getting cached inverse")
		return(i)
	}
}
