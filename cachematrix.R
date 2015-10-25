## These are a pair of functions that cache the inverse of a matrix.

##---------------------------------------------------------
## 'makeCacheMatrix', takes a matrix as its argument and creates a list object 
##  with four functions: 

##  1) 'set(y)', a function that re-instantiates the object created with 
##      'makeCacheMatrix' with a new matrix value, when called from outside 
##      'makeCacheMatrix'.
##  2) 'get()', a function that returns the object crated by 'makeCacheMatrix'.
##  3) 'setinverse(solve)', a function that fills the content of the cache 
##      (c_im1) with the inverse matrix of the matrix in the object created by 
##      'makeCacheMatrix'.
##  4) 'getinverse()', a function that returns the content of the cache (c_im1).

makeCacheMatrix <- function(x = matrix()) {
    c_im1 <- NULL
    
    set <- function(y = matrix()) {
        x <<- y
        c_im1 <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) c_im1 <<- solve
    
    getinverse <- function() c_im1
    
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}
##------------------------------------------------------------
##  'cacheSolve', does three things:

##  1) It checks the content of the cache (c_im2) calling 'x$getinverse' and
##      determines if it has content, or not.
##  2) If it doesn't (i.e., if it is NULL), it gets the matrix created by 
##      'makeCacheMatrix' calling 'x$get()' and computes its inverse matrix.
## NOTE:    The conditional (below) is an add on that determines if the inverse 
##          matrix can be calculated. If it cannot be computed, it avoids R's 
##          error message & debugging step and, instead, sends a friendly 
##          message finishing the call.
##  3) Lastly, it stores the computed inverse matrix into the cache (c_im2) and 
##      returns it when called.

cacheSolve <- function(x, ...) {
    
    c_im2 <- x$getinverse()
    
    if(!is.null(c_im2)) {
        message("Getting cached data...")
        return(c_im2)
    }
    
    data <- x$get()
    
    if ( inherits( try( c_im2 <- solve(data, ...), silent=TRUE),  "try-error")){
        print("Oops, the inverse matrix doesn't exist.")
    } 
    else {
        x$setinverse(c_im2)
        c_im2
    }
}
##---------------------------------------------------------------

