## Two functions, makeCacheMatrix and cacheSolve:
##
## 1. makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
##
## 2.cacheSolve: 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache.

## Description of makeCacheMatrix:
## makeCacheMatrix creates a list containing functions to 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of inverse of the matrix 
## 4. get the value of inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL 
        set <- function(y) { 
               x <<- y 
               inv <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 


## ## Description of cacheSolve:
## The following function returns the inverse of the matrix. It first checks if 
## the inverse has already been computed. If so, it gets the result and skips the 
## computation (because return ends the function). 
## If not, it computes the inverse, sets the value in the cache via 
## setinverse function. 

cacheSolve <- function(x, ...) { 
        inv <- x$getinverse() 
        if(!is.null(inv)) { 
                message("getting cached data.") 
                return(inv)                       
        } 
        data <- x$get() 
        inv <- solve(data) 
        x$setinverse(inv) 
        inv 
} 
