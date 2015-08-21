## The following two functions used in conjunction will allow a user to store
## the inverse of a matrix in cache.  Therefore if the user ever needs the inverse
## again, they can just call the function and it will pull the matrix inverse from
## the cache instead of recalculating it.

## Create a data structure function that stores a matrix and funcitons to
## set a new matrix, get the matrix, store a cache of the inverse, return the cache of the inverse
## The function returns a list of all these functions for the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setCache <- function(x_inv) m <<- x_inv  #stores inverse in cache
    getCache <- function() m
    list(set = set, get = get,
         setCache = setCache,
         getCache = getCache)
}


## Return the inverse of the matrix.  If the value has already been calcuated
## it will be pulled from the cache.  If not it will be calculated and stored
## in the instance of the makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getCache()
    if(!is.null(m_inv)) {
        message("getting cached inverse")
        return(m_inv)
    }
    m <- x$get()
    m_inv <- solve(m)
    x$setCache(m_inv)
    m_inv
}


