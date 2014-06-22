
## 1. 'makeCacheMatrix': Given an input matrix, this function creates a special 
## object that can cache its inverse.
## 2. 'cacheSolve': This function computes the inverse of the original matrix 
## via the special object returned by 'makeCacheMatrix' above. 


## The following function takes in a matrix as input. It creates a list containing
## funcions that can set and get the values of the original matrix and its inverse.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inv_mat <- function(inv_mat) m <<- inv_mat
    get_inv_mat <- function() m
    list(set = set, get = get,
         set_inv_mat = set_inv_mat,
         get_inv_mat = get_inv_mat)
    

}



## The following function first checks to see if the inverse matrix has already 
## been calculated. If so, it returns the inverse matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse matrix of the data and 
## sets the value of the inverse matrix in the cache via 'set_inv_mat' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$get_inv_mat()
    if(!is.null(inv_mat)) {
        message("getting cached inverse matrix")
        return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data, ...)
    x$set_inv_mat(inv_mat)
    
    inv_mat
    
}
