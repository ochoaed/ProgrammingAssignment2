#Matrix inversion  caching the inverse of a matrix rather than compute it repeatedly 
makeCacheMatrix <- function(x = matrix()){    
    m <- NULL
    set <- function(y){
        x <<- y  
        m <<- NULL #set the value of the matrix
    }
    get <- function() x #get the value of the matrix
    setInverse <- function(solve) m<<- solve #set the value of the inverse matrix
    getInverse <- function() m #get the value of the  inverse matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)  }

cacheSolve <- function(x, ...) {
    m <- x$getInverse()                 
    if(!is.null(m)){                    #if the inverse has been  calculated previously sent message and show inverse
        message("getting cached data")     
        return(m)                         
    }
    data <- x$get()                     # get the matrix used in makeCacheMatrix function and calculate inverse 
    m <- solve(data, ...)               
    x$setInverse(m)                 
    m}
