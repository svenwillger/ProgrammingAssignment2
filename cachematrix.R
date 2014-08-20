## Put comments here that give an overall description of what your
## functions do

## The first function contains three functions that are not directly run when makeCacheMatrix is called.
## This function allows you to create a matrix and if a inverted matrix is computated by the below function cacheSolve
## it cache this inverted matrix.

makeCacheMatrix <- function(x = matrix()) {     
    
    # you have to provide an entry in the form of makeCacheMatrix(matrix('data','nrow','ncol'))
    # be aware that the matrix needs to be square meaning ncol=nrow, e.g makeCacheMatrix(matrix(1:4,2,2))
    
    i <- NULL                       # 'i' will be our inverted matrix and it's reset to NULL every 
                                    # time makeCacheMatrix is called
    
    get <- function() x             # this function returns the inverted matrix of the original matrix
    
    setinverse <- function(solve)   # this is called by cacheSolve() during the first cacheSolve()
    { i <<- solve }                 # access and it will store the inverted matrix using superassignment
    
    getinverse <- function() i      # this will return the cached inverted matrix to cacheSolve() on
                                    # subsequent accesses
    
    list(get = get,                 # This list is returned with the newly created object.       
         setinverse = setinverse,   # It lists all the functions that are part of
         getinverse = getinverse)   # the object.  If a function is not on the list then it cannot
                                    # be accessed externally.
    
}

## The function cacheSolve checks if an inverted matrix for a given matrix is computated and if this is true
## returns the cached inverted matrix. If there is no inverted matrix cached it computes a matrix that was defined 
## by the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()                 # accesses the object 'x' and gets the value of the inverted matrix
    if(!is.null(i)) {                   # if an inverted matrix was already cached (not NULL) ...
        
        message("getting cached data")  # ... send this message to the console
        return(i)                       # ... and return the inverted matrix ... "return" ends 
        #   the function cachemean(), note
    }
    data <- x$get()                    # we reach this code only if x$getinverse() returned NULL
    i <- solve(data, ...)              # if 'i' was NULL then we have to create an inverted matrix
    x$setinverse(i)                    # store the inverted matrix in x (see setinverse() in makeCacheMatrix)
    i                                  # return the inverted matrix to the code that called this function
}