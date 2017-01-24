
## makeCacheMatrix creates a list containing a function, 
	## 1. set value of the matrix
	## 2. get value of the matrix
	## 3. set value of inverse of the matrix
	## 4. get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## The function cacheSolve returns the inverse of a matrix
## First cacheSolve if the inverse is already computed
## If so, cacheSolve takes the result, messages "getting cached data." and skips the computation
## If not, cacheSolve computes the inverse and sets the value in the cache via setinverse function

## cacheSolve assumes the matrix is always invertible
cacheSolve <- function(x, ...) {
    	inv <- x$getinverse()
    		if(!is.null(inv)) {
        	message("getting cached data.")
        	return(inv)
  	  }

	  data <- x$get()
	inv <- solve(data, ...)
   	 x$setinverse(inv)
   	 inv
       }
