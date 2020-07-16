## This function eases the matrix inversion by caching rather than performing the matrix inversion repeatedly

## makeCacheMatrix function creates the matrix to store the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL                                        ## invMat will store the inverse matrix 
  
  set <- function(y) {                                  ## a function that defines the values of the matrix
    x <<- y                                           
    invMat <<- NULL                                     ## reset invMat to NULL when there is new matrix
  }
  
  get <- function() x                                   ## obtains the value of the matrix
  
  setInverse <- function(inverse) invMat <<- inverse    ## sets the value of the inverse
  getInverse <- function() invMat                       ## obtains the value of the inverse
  
  ## list allows the $ operator to be used in the succeeding functions below
  list(
    set = set, 
    get = get, 
    setInverse = setInverse, 
    getInverse = getInverse
    )

}

## cacheSolve is the computational function that retrieves the calculated inverse from the cache


cacheSolve <- function(x, ...) {
  invMat <- x$getInverse()                              ## Returns a matrix that is inverse of x and assign it to invMat
  if(!is.null(invMat)) {                                ## This if function checks if the inverse is already calculated
    message("getting cached data")                      ## If already calculated, obtain the cached data and stop computation
    return(invMat)                                      ## The invMat will be returned
  }
  data <- x$get()                                       ## If not the previous case, compute the inverse of the matrix and store it to cache
  invMat <- solve(data, ...)                            ## Method to solve the inverse of the matrix using solve 
   x$setInverse(invMat)
  invMat
}
