## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {    
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(myinv) inv <<- myinv
    getinv <- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache

cacheSolve <- function(x,...){
    inv0 <-x$getinv()
    if(!is.null(inv0)){
        message("getting cached inversed matrix")
        return(inv0)
    }
    data <- x$get()
    if(dim(data)[1] != dim(data)[2]) stop("matrix must be square!\n")
    inv0 <- solve(data)
    x$setinv(inv0)
    inv0
}
       ## Return a matrix that is the inverse of 'x'
}
