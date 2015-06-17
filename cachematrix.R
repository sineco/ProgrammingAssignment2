## This function creates a special "matrix" object that can cache its inverse.

## This function creates a list of functions that can be used to 
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverseMatrix) inv <<- inverseMatrix
    getinv <- function() inv
    # Return a list with the setters and getters functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of a matrix. However, if the inverse has been 
## already calculated it retrieves from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # Retrieve the matrix values
    data <- x$get()
    # Calculate the inverse
    inv <- solve(data)
    x$setinv(inv)
    inv
}
