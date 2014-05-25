## Initialize the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
    ##Initiatalize a null matrix
    i<-NULL    
    ##Set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ##Get the matrix
    get <- function() x
    ##Set the cached version of the inverse matrix
    setcache <- function(cache) i <<- cache
    ##Get the cached version of the inverse matrix
    getcache <- function() i
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}

## Initialize the cacheSolve function
cacheSolve <- function(x, ...) {
    ##check to see if the inverse matrix is null
    i <- x$getcache()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Return matrix that is the inverse of x
    data <- x$get()
    i <- solve(data, ...)
    x$setcache(i)
    i
}