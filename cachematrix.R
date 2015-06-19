# Coursera R Programming 
# Assignment: Caching the Inverse of a Matrix

# Example simple (2x2 square) matrix and its inverse:
# cmatrix <- matrix(1:4, nrow=2, ncol=2, byrow=T)
# cinv <- solve(cmatrix)

# Example use of below functions:
# 1. cm <- makeCacheMatrix(cmatrix)
# 2. cs <- cacheSolve(cm) - will do computation
# 3. cs <- cacheSolve(cm) - calling again will use cache

# This function creates a special "matrix" list object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    #get/set interface for extended matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    m <- x$getinverse() #Attempt request for cached inverse
    if(!is.null(m)) {
        message("Return cache")
        return(m)
    }
    message("Return computation")
    data <- x$get() #get matrix
    m <- solve(data, ...)
    x$setinverse(m) #Cache inverse for subsequent requests
    return(m)
}
