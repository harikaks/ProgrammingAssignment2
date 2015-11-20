## calculates the inverse of a special object that stores "matrix". 
## And caches the value of the inverse so that when we need it again, it can be
## looked up in the cache rather than recomputed.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mi<-NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mi <<- inverse
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then retrieves the 
## inverse from the cache.Otherwise,it calculates the inverse 

cacheSolve <- function(x, ...) {
        # get inverse of a matrix
        mi <- x$getinverse()
        # Returns the inverse, if the inverse has been alredy calculated  for the matrix
        if(!is.null(mi)) {
                message("getting cached matrix inverse")
                return(mi)
        }
        
        data <- x$get()
        # Computes inverse if it is a new matrix
        mi <- solve(data, ...)
        # sets the value of the inverse in the cache
        x$setinverse(mi)
        mi
}


