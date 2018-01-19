## Cache the inverse of a matrix using the following
## two functions

## This function 'makeCacheMatrix' creates a 'special' matrix object that can
## cache its inverse, it creates a list containing a function to:	

    ##set the value of the matrix
    ##get the value of the matrx
    ##set the value of the inverse
    ##get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function 'cacheSolve' calcutes the inverse of the 'special' matrix
## created with the above function. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and 
## skips  the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setinverse function. 
## This function assumes the matrix supplied is always invertible


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}

