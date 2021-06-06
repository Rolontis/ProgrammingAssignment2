## Create a special object that stores a matrix and cache's its inverse matrix.
## This function creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the invser of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                        x <<- y
                        m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the matrix created with the above function
## It first checks if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculated the inverse matrix and sets this in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}