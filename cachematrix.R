## Put comments here that give an overall description of what your
## functions do


## This function defines a matrix object with a set of 
## functions (get, set,getinverse,setinverse)to modify the object
## The functions within the matrix object is used by the 
## cacheSolve function to compute the inverse matrix and to retrieve matrix information

makeCacheMatrix <- function(x = matrix()) {

	  i <- NULL
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse= matrix()) i <<- inverse
        getinverse <- function() i

	  #put the matrix object functions into a list for easy access of the function elements
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function receives a matrix object and retrieves its inverse
## If the value of the matrix inverse is null then the inverse 
## matrix is calculated and then set within the matrix object using setinverse
## If the value of the inverse matrix is NOT null then the solve() 
## function is not used and the getinverse() is used to retrieve the inverse (caching)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrixval <- x$get()
        i <- solve(matrixval, ...)
        x$setinverse(i)
        
}
