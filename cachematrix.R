makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

#mat <- matrix(c(7,2,1,0,3,-1,-3,4,-2), nrow = 3, ncol = 3)
#cache_matrix <- makeCacheMatrix(x = mat)
#value_matrix <- cacheSolve(cache_matrix)
#new_cache_matrix <- cacheSolve(cache_matrix)