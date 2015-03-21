##The functions below create a matrix object that can cache its inverse and then checks to see if the inverse has already been calculated, and if not, it computes the inverse of the matrix
## matrix
## The makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get  <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
    setSolve = setSolve,
    getSolve = getSolve)

}


## The cacheSolve function checks to see if the matrix inverse has been computed. If not, it computes the inverse of the matrix returned by makeCacheMatrix. Else, it skips "re-computing" and returns the already computed inverse matrix.

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
