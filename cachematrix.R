## Put comments here that give an overall description of what your
## functions do

## Below functions are used to create closure on matrix object and use 
## closure environment to store objects per particular matrix instance
## In this case, we're using the 2nd function to calculate, store and return
## the inverse of CacheMatrix (if not available) or 
## return the stored inverse of CacheMatrix object 
## (without repeating calculation) in case the function has 
## at least once been invoked on a given CacheMatrix instance


## Write a short comment describing this function

## makeCacheMatrix() is used to wrap R matrix given by input parameter (x) 
## and make its inverse cacheable; 
## 4 inner functions of makeCacheMatrix are used to retrieve R matrix (get),
## redefine it (set), calculate its inverse and cache it (setinv), 
## and retrieve cached inverse of the stored R matrix (getinv);
## by using "<<-" inner functions make assignments to variables 
## of the parent environment (closure) (and thus do not create local variables)

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinv <- function(im) invm <<- solve(im)
    getinv <- function() invm
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    

}


## Write a short comment describing this function

## cacheSolve() is used to return cached inverse of the matrix object 
## created by makeCacheMatrix() if available
## otherwise, the inverse is calculated, 
## stored in closure variable and returned 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inversed matrix")
        return(inv)
    }
    data <- x$get()    
    x$setinv(data)
    x$getinv()
}
