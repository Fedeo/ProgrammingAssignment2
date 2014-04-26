## These functions are intended to create and cache the inverse of matrix
## if used together they allow to store the result of 'inverting' a matrix so
## that each time the inverse operation is needed for the same matrix (with same data)
## there is no need to execute the operation again but cached inverse matrix is used
## An example:
## > mymatrix <- matrix(runif(9,1,10),nrow=3,ncol=3)  
## > cacheSolve(makeCacheMatrix(mymatrix)) #generate the inverse and store it
## > cacheSolve(makeCacheMatrix(mymatrix)) #get the cached data and provide the result previously computed


## makeCacheMatrix generates a list out of a matrix so to store original data and cached result after
## an inverse computation is done

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)

}


## cacheSolve generate the inverse of a matrix and store the result. If inverse has been already calculated
## it returns directly the cached inverse matrix without any additional calculation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmatrix()
    
    # if inverse was already calculated it returns previous calculation
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if first time it generates the inverse of x using the solve function
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
