## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function(){x}
    setTranspose <- function(tran){ m <<- tran}
    getTranspose <- function(){m}
    list(set =set,get=get,setTranspose=setTranspose,getTranspose=getTranspose)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getTranspose()
    if(!is.null(m)){
        message("Getting cached data")
        return(m)
    }
    data <-x$get()
    m <- solve(data,...)
    x$setTranspose(m)
    m
}