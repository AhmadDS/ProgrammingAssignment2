
# make Matrix Function 
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    #set matrix value, and reset inverse
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    #get the matrix
    get <- function() x
    #set the passed inverse
    setinverse<- function(inv) inv_matrix <<- inv
    #get the cached invers
    getinverse<- function() inv_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# get inverse and cache 
cacheSolve <- function(x, ...) {
    # get cached inverse 
    inv <- x$getinverse()
    # if cached value != null return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if inverse not cached, calculate it and cache it
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# Testing Commands
# mat <- matrix(rnorm(1000000),1000,1000)
# mc <- makeCacheMatrix(mat)
# cacheSolve(mc)

