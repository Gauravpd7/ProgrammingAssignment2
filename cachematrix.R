## The functions are caching the inverse of the matrix rather than computing 
## it repeatedly, as the computations involved in inversing a matrix are quite expensive
## If the inverse of a matrix has already been calculated then the function retrieves the 
## inverse from the cache

##--------------------------------------------------------------------------------------------


## The makeCacheMatrix function creates a special matrix object using the <<- operator
## It returns a list which contains the following functions
## set - sets the value of the matrix object
## get - get the value of the matrix object
## setinverse - set the value of the inverse matrix
## getinverse - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse){
                i <<-inverse
        }
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




##The cacheSolve function computes the inverse of the special matrix returned by the 
## makeCacheMatrix function.If the inverse has already been calculated and the matrix
## has not changed, then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat)
        x$setinverse(i)
        i
}
