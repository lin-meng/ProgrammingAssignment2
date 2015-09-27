## This program is designed to demonstrate the concept of Lexical Scoping in R.
## In the program, there is a pair of functions: makeCacheMatrix and cacheSolve.
## makeCacheMatrix stores a matrix input, or calculates the inverse of the matrix.
## The <<- operator used in makeCacheMatrix updates invMtx between functions within.
## cacheSolve gets the matrix inverse from makeCacheMatrix when it exists, or
## calculates the matrix inverse if it is not solved.


## makeCacheMatrix is a list of four functions: 1) set--reinitialize a input matrix
## 2) get--get the input matrix, 3) setInverse--calcluate the matrix inverse,
## 4) getInverse--get the matrix inverse. The 4 functions are arranged sequentially.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the variable of invMtx
	invMtx <- NULL 
		
    ## a function to reinitialize the input matrix and invMtx using <<- 
    set <- function(y) {
                x <<- y
                invMtx <<- NULL
    }
       
	## a function to get the input matrix
	get <- function() x
		
	## a function to calcluate the matrix inverse and update invMtx using special <<- 
    setInverse <- function(invMtx) invMtx <<- solve(x)  # use R built-in solve
        
	## a function to get the matrix inverse from invMtx
	getInverse <- function() invMtx
        
	## create a list of all 4 functions, and return them as the output
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve uses 'getInverse', 'get', 'setInverse' defined in makeCacheMatrix;
## if it gets the matrix inverse, then it will use and return it directly,
## if it does not get the matrix inverse, then it will solve and return it.

cacheSolve <- function(x, ...) {
    ## Get the matrix inverse and update invMtx using 'getInverse'
	invMtx <- x$getInverse() 

	## If invMtx is not NULL, then it will be returned, no more calculation.
	if(!is.null(invMtx)) {
                message("getting cached matrix")
                return(invMtx)
    }
		
	## If invMtx is NULL, then get the input matrix, solve it, 
	## and assign the matrix inverse to invMtx
        data <- x$get()       # use 'get' to get the input matrix
        invMtx <- solve(data, ...)    # use R built-in solve and update invMtx
        x$setInverse(invMtx)       # generate the matrix inverse with 'setInverse'
        invMtx            # return the updated invMtx
}


