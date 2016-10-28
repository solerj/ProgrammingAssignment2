## Example:
## x<-matrix(1:4,c(2,2))
## z<-makeCacheMatrix(x)
## cacheMatrix(z)   - For this output, the inverse has to be calculated because
##                  it has not been worked out before. At this point, the
##                  inverse is stored in z
## cacheMatrix(z)   - For the output, the function simply returns the inverse
##                  which is stored in z


## The makeCacheMatrix function creates a special "matrix" in the form of a list
##containing four functions:
    ## The first sets the the matrix
    ## The next returns the matrix
    ## The next sets the inverse of the matrix
    ## The last one returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) v <<- solve
    getinverse <- function() v
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##The following function calculates the inverse of the special "matrix" created
##with the above function. However, it first checks to see if the inverse has
##already been calculated. If so, it gets the inverse from the cache and skips
##the computation. Otherwise, it calculates the inverse of the matrix and sets
##the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(z, ...) {
    v <- z$getinverse()
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    data <- z$get()
    v <- solve(data, ...)
    z$setinverse(v)
    v
}
