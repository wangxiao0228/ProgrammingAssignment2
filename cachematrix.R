## The functions are writen to cache the inverse of a Matrix. If the inverse of
## the matrix has been calculated, it should be cached. When we need it agian,
## it can be looked up in the cache rather than recomputed.
## 

## makeCacheMatrix creates a special "matrix" objuect that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above.
## if the inverse has already been calculated , then the cacheSolve should retrieve 
## the inverse from the cache; if not, the inverse of the matrix should be calculated
## by function solve.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
