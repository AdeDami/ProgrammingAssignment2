## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
	
## This function creates a special "matrix" object that can cache its inverse.
	
	
	makeCacheMatrix <- function(x = matrix()) {    ## The input to the function is a matrix
	        invMatrix <- NULL                      ## invMatrix will hold the inverse matrix; set to NULL
	        set <- function(y) {                   ## Defining the set function
	                x <<- y                        ## The value of matrix in parent environment
	                invMatrix <<- NULL             ## If the matrix is changed or new, reset invMatrix to Null
	        }
	        get <- function() x                    ## Defining the get function 
	        setInverse <- function(inverse) invMatrix <<- inverse
	                                               ## Assigns value of invMatrix in parent environment
	        getInverse <- function() invMatrix     ## Gets the value of invMatrix when called
	        list(set = set,
	             get = get,
	             setInverse = setInverse,
	             getInverse = getInverse)           ## Functions to be accessed using the $ operator
	}
	
	## cacheSolve: This function computes the inverse of the special "matrix" 
	## returned by makeCacheMatrix above.
	## If the inverse has already been calculated (and the matrix has not changed),
	## then the cachesolve should retrieve the inverse from the cache.
	
	cacheSolve <- function(x, ...) {               ## Return a matrix that is the inverse of 'x
	        
	        invMatrix <- x$getInverse()
	        if (!is.null(invMatrix)){              ## Getting the cached inverse data
	                message("getting cached data")
	                return(invMatrix)
	        }
	        mat <- x$get()
	        invMatrix <- solve(mat, ...)
	        x$setInverse(invMatrix)
	        invMatrix                             ## The inverse matrix returned 
	}
