## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        ##Set inverse property
        inv <- NULL
        
        ##Set value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ##Get value of matrix and return the matrix
        get <- function() x
        
        ##Set inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ##Get inverse of the matrix and return the inverse property
        getinverse <- function() inv
        
        ##Return the matrix with defined functions 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Create a function to compute the inverse of the special matrix returned by
##the makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ##Get the matrix that is the inverse of x
        inv <- x$getinverse()
        
        ##If statement for if the inverse has already been calculated, return it
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        
        ##Compute the inverse
        data <- x$get() 
        inv <- solve(data)
        
        ##Cache the inverse
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse
        inv
}