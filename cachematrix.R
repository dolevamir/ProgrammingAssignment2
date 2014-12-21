## Create an inverse matrix


makeCacheMatrix <-function(x = matrix()) {
        inversed_matrix <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inversed_matrix <<- solve
        getinverse <- function() inversed_matrix
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

cacheSolve <- function (x, ...) {
        inversed_matrix <- x$getinverse
        if(!is.null(inversed_matrix)){
                message("getting cached data")
                return(inversed_matrix)
        }
        data <- x$get()
        inversed_matrix <- solve(data, ...)
        x$setinverse(inversed_matrix)
}
