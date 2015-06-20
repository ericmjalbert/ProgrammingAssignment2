## The basic idea of these functions is to create an object that can hold 
## functions for a matrix. Mainly allowing for the inverse of the matrix
## to be catched in memory, so that it doesn't always need to be computed.


## Creates an object that holds a list of functions that allow for 
##    multiple operations on a matrix. These operations are fairly 
##    simple, limited to: 
##        - setting a matrix
##        - getting the matrix
##        - setting the inverse of the matrix
##        - getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    # Getter and setter for matrix
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL    # Reset inverse for new matrix 
    }
    get <- function() x

    # Getter and setter for inverse matrix
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv

    # List here is to label each function within this object
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of the matrix x. It does this by reading
##    in the catched inverse value from x. If this value exists then we are 
##    done and we return that value. Otherwise we need to compute inverse.
##
##    This is done so that the inverse would only have to be calculated once.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {     # Check if the inverse value already exists
        message("getting cached data")
        return(inv)         # If it does, just grab that value
    }
    data <- x$get()         # Grab the matrix x
    inv <- solve(data)
    x$setinv(inv)   # Actually compute the inverse and save it to x
    inv                     # Last line is output; return the inverse.
}
