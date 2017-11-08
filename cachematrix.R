## The following functions cache the inverse of a matrix

## The function will create a special matrix object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
i<-NULL #variable for inverse
  set<-function(y)
  {
    x<<- y
    i<<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) i<<-inverse
  getinverse<- function() i
  list(set= set, get= get, setinverse= setinverse, getinverse= getinverse)
}


## The function is used to inverse the "special matrix" returned by the above function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<- x$getinverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  
  data<- x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}
