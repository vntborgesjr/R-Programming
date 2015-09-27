## The following two functions are used to cache the inverse of a matrix

## makeCacheMatrix() outputs a list of functions that can create a matrix
## (e.g. set, get) and cache its inverse (e.g. setInverse, getinverse).

makeCacheMatrix <- function(x = matrix()) {
        
        # set the matrix
        
        inv <- NULL
        set <- function(y) {
                x <<- y # <<- assign a value to an object in a environment different from the current
                inv <<- NULL
        }
        
        # get the matrix
        
        get <- function() x 
        
        # set the inverse
        
        setinverse <- function(solve) inv <<- solve 
        
        # get the inverse
        
        getinverse <- function() inv 
        
        # return a lis of functions
        
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve() calculate the inverse matrix of the matrix computed by makeCacheMatrix()
## or if the inverse has already been computed and the matrix has not changed
## it will return the inverse from the cache 

cacheSolve <- function(x, ...) {
        
        # return the inverse of the original matrix
        
        inv <- x$getinverse()
        
        # if the inverse has already been computed...
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) # return the it from the cache and skips the comutation
        }
        
        # if the inverse has not been computed...
        
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv) # calculate the inverse of the matrix
        inv # return the inverse
}
