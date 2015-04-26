# This file contains 2 functions which help to cache
# the inverse of a matrix and return it when requested.

# This function is used to create a special
# matrix along with a list of functions which
# act as getter and setter for matrix as well as
# inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(inp){
        x<<-inp
        i<<-NULL
    }
    get<-function() x
    setinv<-function(inv) i<<- inv
    getinv<-function() i
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv)
}


# This function is used to checks if the inverse is 
# already calculated and returns if calculated or
# calculates and saves for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
