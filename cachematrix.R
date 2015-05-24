
makeCacheMatrix <- function (x = matrix()){
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL                
        }
        
        get <- function()x
        setinv <- function(inv) v <<- inv
        getinv <- function() v
        
        list (set = set, get = get, setinv = setinv, getinv = getinv)
        
}



cacheSolve <- function(x, ...){
        
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
                
        }
        
        t <- x$get()
        v <- solve(t, ...)
        x$setinverse(v)
        v
        
}
