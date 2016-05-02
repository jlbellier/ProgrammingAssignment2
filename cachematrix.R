## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a matrix and computes its inverse matrix.It also computes the getters anf setters for both original matrix and its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) {
			inv <<- solve
		}
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## This function checks if the invert of a matrix x is stored in cache. 
## If so, it retrieves the value stored in cache.
## If not, then it computes the invert matrix using the 'getsolve' function defined in makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
		## Here we check if matrix is invertible
		if (det(data) == 0)
		{
			print("matrix is not invertible")
			inv <- NULL
		}
		else 
		{
			inv <- solve(data, ...)
		}
        x$setsolve(inv)
        inv
}
