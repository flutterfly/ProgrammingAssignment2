## These functions take the inverse of a given matrix and store the matrix in 
## cache.

## The below function defines methods for setting the initial matrix, getting the intial matrix,
## storing the inverse matrix into cache, and getting the inverse matrix from cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(inverse) m <<- inverse
        getMatrix <- function() m
        list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)

}


## The below function takes a matrix, finds its inverse, and stores the inverse matrix in cache.
## If the inverse matrix is already store in cache, the below function prints a message telling
## the user that it is returning the already-cached data, and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
