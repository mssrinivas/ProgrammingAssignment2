
## Calculating inverse of a Matrix (Memoization Technique)

## This function creates a "matrix" object
## that can be used to cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
        inverse <- NULL
        set <- function(x)  {
            mat <<- NULL
            inverse <<- NULL
        }
        
        get <- function() return (mat)
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() return (inverse)
        return(list(set = set , get = get , setinverse = setinverse , getinverse = getinverse))

}


##Used for computing inverse of the "matrix"
##returned by the above mentioned function "makeCacheMatrix"
##If the inverse has been already calculated ("if the ,atrix has not changed"),
##then "CacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        inverse <- mat$getinverse()
        if(!is.null(inverse)) {
               message("getting cached data...")
               return(inverse)
        }
        
        data <- mat$get()
        inverse <-solve(data,...)
        mat$setinverse(inverse)
        return(inverse)
}
