## This program is designed to demonstrate the concept of Lexical Scoping in R.
## In the program, there is a pair of functions: makeCacheMatrix and cacheSolve.
## makeCacheMatrix stores a matrix input, or calculates the inverse of the matrix.
## The special assignment <<- operator is used in makeCacheMatrix to update mkMtx.
## cacheSolve uses the inverse of the matrix from makeCacheMatrix when it exists, 
## or calculates the inverse of the matrix if it was not solved.


## makeCacheMatrix is a list of four functions: 1) set --reinitialize a matrix
## 2) get --get a matrix input, 3) setInverse --solve the inverse of a matrix
## 4) getInverse --get the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the variable of mkMtx
		mkMtx <- NULL 
		
        ## a function to reinitialize and update mkMtx using special <<- 
        set <- function(y) {
                x <<- y
                mkMtx <<- NULL
        }
       
	## a function to get a matrix input
	get <- function() x
		
	## a function to calcluate the inverse, update mkMtx using special <<- 
        setInverse <- function(mkMtx) mkMtx <<- solve(x)  # Use R built-in solve
        
	## a function to get the inverse of a matrix from mkMtx
	getInverse <- function() mkMtx
        
	## create a list of all 4 functions, and return them as the output
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve uses 'getInverse', 'get', 'setInverse' defined in makeCacheMatrix;
## if it gets the inverse of the matrix, then it will use it directly,
## if it does not get the inverse of the matrix, then it will solve it.

cacheSolve <- function(x, ...) {
        ## Get the inverse of the matrix and update mkMtx using 'getInverse'
	mkMtx <- x$getInverse() 

	## If mkMtx is not NULL, then it will be returned, no more calculation.
	if(!is.null(mkMtx)) {
                message("getting cached matrix")
                return(mkMtx)
        }
		
	## If mkMtx is NULL, then get the input matrix, solve it, 
	## and assign the inverse to mkMtx
        data <- x$get()       # use 'get' already defined to get the matrix
        mkMtx <- solve(data, ...)    # Use R built-in solve and update mkMtx
        x$setInverse(mkMtx)       # Update the input matrix with 'setInverse'
        mkMtx            # return the updated mkMtx
}
