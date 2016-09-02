## makeCacheMatrix creates a special matrix
## set the matrix
## get the matrix
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y)
     {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv<<-inverse
     getinv <- function() inv
     list(set=set, get=get, setinv=setinv, getinv=getinv)


}


## calculates the inverse of matrix
## checks if inverse has already been calculated
## if so, it gets inverse from cache and skips computation
## otherwise calculates inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv<-x$getinv()
     if(!is.null(inv))
     {
        message("getting cached data")
        return(inv)
     }
     data<- x$get()
     inv<- solve(data,...)
     x$setinv(inv)
     inv

}
