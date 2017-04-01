## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

########################################
##          makeCacheMatrix
##Input: square matrix ,and it is is a square invertible matrix
##Output:a special "matrix" list
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function(){x}
    setInv <- function(tran){ m <<- tran}
    getInv <- function(){m}
    list(set =set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function

########################################
##          cacheSolve
##Input: a special "matrix"
##Output: a inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)){
        message("Getting cached data")
        return(m)
    }
    data <-x$get()
    m <- solve(data,...)
    x$setInv(m)
    m
}

########################################
## test steps
##please follow the below steps to test the functions
hilbert <- function(n) 
{ 
    i <- 1:n;
    1 / outer(i - 1, i, "+") 
}
a <- hilbert(8);
b <- makeCacheMatrix(a)
c <- cacheSolve(b)
round(a %*% c,3)
c <- cacheSolve(b)