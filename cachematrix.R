
## This function creates a special "matrix" object that can cache its inverse
## You might want to cache an inverse because inversion calculation is costly, especially for large matrices
## As a reminder - only square matrices have inverses

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve reports the Inverse of a Matrix if was already cached
## If the matrix inverse isn't stored in cache, cacheSolve will calculate the inverse and return it
## Either way, the matrix inverse will be reported 

cacheSolve <- function(x, ...) {
        
	## Try to grab the inverse of the matrix, which may have been calculated & stored in the MakeCacheMatrix function
   	  m <- x$getInv()
	
	## Check to see if we already have the inverse or not 
        if(!is.null(m)) {
                message("getting cached data")
			## if we already have the inverse, report it and end the function
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
