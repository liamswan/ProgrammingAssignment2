## The two functions bellow are used to (1) create a 
## special object that stores a matrix and (2) cache's 
## and returns its inverse.


## The function, makeCacheMatrix creates a special
## "matrix", that is actually a list containing a 
## function to:
## 1. set the value of the matrix: set()
## 2. get the value of the matrix: get()
## 3. set the value of the inverse: setinverse()
## 4. get the value of the inverse: getinverse()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function(y) x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function, cacheSolve checks to see if the inverse
## of the given "matrix" has already been calculated.
## If it has, it returns the inverse from the cache 
## instead of re-calculating the inverse. Otherwise it
## it returns the inverse and stores it in the cache via 
## the setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
