## the functions below were adapted from the example provided by the course
## assignment instruction by simply replace mean() with solve(), and objects
## named with "mean" by "inverse"

## makeCacheMatrix function creates a list containing functions to
## set/get the value of matrix, and set/get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<-inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() function checks if the inverse of the matrix was calculated
## and cache. if not, then caches the inverse by calling the setinverse()
## function created in the makeCacheMatrix()

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
