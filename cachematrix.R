
## Pair of functions to create inverse of matrix and cache the inverse
## in case inverse of the same matrix should be calculated


##
## Function creates a special object that stores a numeric matrix
## and caches its inverse
## There is assumption that matrix can be changed only by using set()
## When function set() is used inverse is set to null,and cachesolve()
## has to recalculate it
## 

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(inpMatrix){
                x <<- inpMatrix
                invMatrix <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(invM) invMatrix <<- invM
        getInvMatrix <- function()  invMatrix
        list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the "matrix" object returned
##  by makeCacheMatrix(). If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
## In this version there is assumption that the matrix is always invertible

cacheSolve <- function(x, ...) {
        invM <- x$getInvMatrix()
        if(!is.null(invM)){
                message("getting inverse matrix from cashe")
                return(invM)
        }
        message("calculate new inverse")
        mtx <- x$get()
        invM <- solve(mtx)
        x$setInvMatrix(invM)
        invM
}

## simple test for inverse matrix cache 
##
##   testM = matrix(c(1,0,1.5,0,1,2,1.2,0,1),nrow=3,ncol=3,byrow=TRUE)
##   y <- makeCacheMatrix(testM)
##   invTestM <- cacheSolve(y)
##   invTestM <- cacheSolve(y)
##   testM[1,1] <-5
##   y$set(testM)
##   invTestM <- cacheSolve(y)

  
  
  
