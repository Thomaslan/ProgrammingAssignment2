## The purpose of the 2 functions defined in this file is to return
## the inverse of an input matrix by :
## * computing it if necessary (i.e. if the computation
##     has not already been done for the same matrix)
## * if possible, retrieving the previously computed, cached inverse matrix.



## This function defines a list of 4 subfunctions/methods for setting and
## retrieving the values of an input matrix and its inverse matrix.
## The data are stored in the local environment of the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL    #initializing the cached inverse matrix
    
    set <- function(y) {
        x <<- y             #set the value of the input matrix
        i <<- NULL          #removing previously cached inverse matrix (if any)
    }
    
    get <- function() x    #get the value of the input matrix
    
    setInverse <- function(inv) i <<- inv   #set the value of the inverse matrix
    
    getInverse <- function() i              #get the value of the inverse matrix
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)     #returns a list of the 4 functions
}



## This function looks for a cached value of the inverse matrix of x
## and makes the calculation if no cached value is found.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()     #looks for a cached value of the inverse matrix
    
    #if a cached value has been found:
    if(!is.null(i)) {
        message("getting cached data")
        return(i)           #returns the cached value
    }
    
    #if no cached value has been found:
    data <- x$get()
    i <- solve(data, ...)        #executes the computation of the inverse matrix
    x$setInverse(i)         #stores the computed value into cache
    i                       #returns the computed value
}



## Example execution script:

#  > z = makeCacheMatrix()       #initialization
#  > z$set(matrix(rnorm(10000), 100, 100))    #input matrix
#  > cacheSolve(z)               #return inverse matrix (computing)
#  > cacheSolve(z)               #return inverse matrix (retrieving cached data)
