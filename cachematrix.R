## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # initial value is NULL since nothing is in the cache
        inv <- NULL
        # set the value of a matrix
        set <- function(y) {
                x <<- y
                # clear the cache when a new value is assigned
                inv <<- NULL
        }
        # get the value of a matrix
        get <- function() x
        # set the value of the inverse matrix
        setinverse <- function(solve) inv <<- solve
        # get the cached value
        getinverse <- function() inv
        # return a list where each element is a function
        list(set=set, get=get, 
            setinverse=setinverse, 
            getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # ask for the cached value of the matrix
        inv <- x$getinverse()
        # if it is not NULL then it use this value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # otherwise ask for the matrix, caclulate the inverse and store it in
        # the cache
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        # return the inverse
        inv
}
