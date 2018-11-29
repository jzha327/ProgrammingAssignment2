## A pair of functions that cache the inverse of a matrix

## Create matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <-NULL
	set <- function(matrix) {
		x <<- matrix
		i <<- NULL
	}		
	get <-function(){
		x
	}	
	setInverse <-function(inverse) {
		i <<- inverse
	}
	getInverse <- function(){
		i
	}	
	list(set=set, get =get,
	setInverse=setInverse,
	getInverse =getInverse)
}


## Compute the inverse of the matrix returened by "makeCacheMatrix" above

cacheSolve <- function(x, ...) {
      
        m <- x$getInverse()
        mif(!is.null(m)) {
        	message("getting cached data")
        	returen(m)
        }
        data <-x$get()
        m <-solve(data) %*% data
        x$setInverse(m)
        m
}
