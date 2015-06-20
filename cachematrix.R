## Two functions below calculate inverse of a matrix and create set of functions 
## to cache and retrive the matrix and it's inverse

## create a set of functions to store and retrive matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(newInverse) inverse <<- newInverse
        getinverse <- function() inverse
        list (set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
}


## check cache for the inverse first via getinverse function call, 
## if it doesn't exists then calculate the inverse and store it's in 
## cache using setinverse function

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("Getting cached inverse value")
                return (inverse)
        }
        matrix <- x$get()
        ## Calculate and return a matrix that is the inverse of 'matrix'
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}
