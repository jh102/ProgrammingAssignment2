# This function creates a special "matrix" object that can cache its inverse using function solve()
# "<<-"  is used to assign a value to an object of a different environment from the current one
# k is a free variable
# The solve() function return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    Ma_solve <<- NULL
    set <- function(k){
        x <<- k
        Ma_solve <<- NULL
        
    }
    
    get <- function() X
    Ma_setsolve <- function(solve) Ma_solve <<- solve
    Ma_getsolve <- function() Ma_solve
    
    list(set = set, get = get,
         Ma_setsolve = Ma_setsolve,
         Ma_getsolve = Ma_getsolve)
    
    
}



# This function calculates the inverse of the special "Matrix" returned by makeCacheMatrix()
# Also checks if the inverse has already been calculated, then cacheSolve() will retrieve the inverse
# from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of "x"
    Ma_solve <- x$Ma_getsolve()
    if(!is.null(Ma_solve)) {
        message("Getting cached data")
        return(Ma_solve)
    }   
    
    M1 <- x$get()
    Ma_solve <- solve(M1, ...)
    x$Ma_solve(Ma_solve)
    Ma_solve
    
}
    


