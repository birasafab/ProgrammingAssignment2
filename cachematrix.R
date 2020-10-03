## The following functions serves to cache the inverse of an input matrix. this avoids the re-computation of the inverse repeatedly when the matrix has not changed

## the following matrix create an empty matrix and secure its memory, which will be used to store an inverse for input matrix

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<- function(y) {
                x<<-y
                m<<- NULL
                
        }
        get<- function() x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m
        list(set=set, get = get, 
             setinverse=setinverse, 
             getinverse=getinverse)

}


## The following function verify if there is an inverse of input matrix if not changed and the retrieves it, 
##if the matrix has changed it first calculates it and cache it under m then output it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if(!is.null(m)){
                message("getting the inverse of matrix x")
                return(m)
        }
        h<-x$get()
        m<-solve(h)
        x$setinverse(m)
        m
}
