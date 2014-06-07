## This R function is able to cache the calculations to
## get the inverse of a matrix that can be time-consuming computations

## This function, makeCacheMatrix create a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse of a Matrix
## get the value of the Inverse of a Matrix
makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is a matrix that is processed 
        ## 'inv' will store inverse matrix , null initialize
        inv <- NULL     
        ## set the value of the Matrix
        set <- function(y){     
                x <<- y
                inv <<- NULL
        }
        ## get the value of the Matrix
        get <- function() x
        ## set the value of the Inverse of a Matrix
        setinv <- function(m) inv <<- m
        ## get the value of the Inverse of a Matrix
        getinv <- function() inv
        ## list assigned functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function calculates the inverse of a matrix of the "matrix"
## created with the makeCacheMatrix function. 
## It first checks to see if the inverse of a matrix has already been calculated.
## If so, it gets the inverse of a matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of a matrix of the matrix and sets the value
## of the inverse of a matrix in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## 'x' is a function and '...' parameters to bring this function
        ## 'inv' get the Inverse of a Matrix from the cache and return
        inv <- x$getinv() 
        if(!is.null(inv)){
                message("getting cached data")
                return(inv) 
        }
        ## data get the matrix from the cache and compute the inversa matrix
        data <- x$get()
        inv <- solve(data, ...)
        ## store the inversa matrix in cache
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        return(inv)
        
}
## Example for use together:
# mattrix <- matrix(1:4, byrow = FALSE, ncol = 2)
# obj <- makeCacheMatrix(mattrix)
# cacheSolve(obj)
# when we need it again, it can be looked up in the cache
# cacheSolve(obj)