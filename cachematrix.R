## Functions to store and retrieve a matrix inverse computation
## Function calls should take place like this:
## >   x <- makeCacheMatrix(my_matrix)
## >   cacheSolve(x)
## >   cacheSolve(x)
## Where my_matrix must be an invertible matrix (determinant <> 0).
## In the first call, cacheSolve(x) should compute and cache the inverse,
## in subsequent calls it should return the cached inverse.

## makeCacheMatrix() defines a list of 3 functions on the given matrix:
## - get: to retrieve the matrix itself
## - setinv: to store the inverse matrix (outside of the function closure,
##           so it can be retrieved by another function)
## - getinv: to retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # initialize variable for the cached inverse matrix
        cinvx <- NULL
        
        # define functions on x
        get <- function() x
        setinv <- function(invx) cinvx <<- invx
        getinv <- function() cinvx
        
        # add the functions to a list
        list (get = get,
              setinv = setinv,
              getinv = getinv)
}

## cacheSolve() checks if an inverse of the matrix (x) already exists in cache.
## If so, it returns the cached inverse plus a text line indicating that it has 
## read from cache.
## If not, it performs the inversion operation (solve(x)) and stores it in cache.
## It does all of this by calling the functions/methods defined on x in the 
## makeCacheMatrix() function.

cacheSolve <- function(x) {
        
        ## get the inverse of x from the cache
        cinvx <- x$getinv()
        
        ## if found, return it and issue a message
        if (!is.null(cinvx)) {
                message("Getting cached data")   # message
                return(cinvx)                    # return inverse and exit function
        }
        
        # if not, perform the solve operation and store and return the inverse
        data <- x$get()       # read matrix
        invx <- solve(data)   # make inverse
        x$setinv(invx)        # store it in cache
        invx                  # return the inverse
}
