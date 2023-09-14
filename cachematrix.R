## There are two functions, one that contains list of functions to set and get the matrix and its inverse. 
##Other function calculates the inverse of given matrix.

## makeCacheMatrix creates a list containing functions 1. setmatrix 2. getmatrix 3. setinverse 4.getinverse

makeCacheMatrix <- function(x = matrix()) {
        inverse<- NULL
        setmatrix <- function(y){
                x <<- y
                inverse <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(setmatrix = setmatrix,getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function finds inverse of above matrix
##first it will check if inverse is cached, if yes, it will output a message of getting cached data.
##if not cached, it will calculate inverse using solve() function (only suitable for sqaure matrix).


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getmatrix()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        
}
