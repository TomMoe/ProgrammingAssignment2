## The first function creates a set of functions to carry out, storing a matrix in the global environment, 
## getting the matrix from global evironment, storing the inverse matrix in the global evironment, and getting
## the inverse matrix from memory

makeCacheMatrix <- function(x = matrix()) {
 CachedInv <- NULL
       set <- function(y){
           x <<- y
           CachedInv <<- NULL
       }
       get <- function() x
       setInvMat <- function(InvMatrix) CachedInv <<- InvMatrix
	 getInvMat <- function() CachedInv
       list(set=set,get=get,setInvMat = setInvMat, getInvMat = getInvMat)
}


## The second function retrieves a value for the inverse matrix. Checks to see that the 
## inverse matrix exsits and the matrix enter is the same as the original matrix.
## If the original matrix is the same as the original and the cached inverse exists, the 
## cached matrix is return, else calculate the inverse matrix and store it in the global evironment

cacheSolve <- function(z, ...) {
       CachedInv <- x$getInvMat()
       if(!is.null(CachedInv) & identical(z,x)){
          message("Returning Cached Inverse Matrix")
          return(CachedInv)
       }
       matdata <- x$get()
       NewInv <- solve(matdata, ...)
       x$setInvMat(NewInv)
       CachedInv
}

