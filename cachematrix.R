## These are 2 functions that can be used to:
## 1) create a matrix
## 2) get it's value
## 3) get it's inverse
## 4) cache it's inverse

## This is the main function, that will be used to achieve the 4 tasks
## described above

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {							#The set function: 
    x <<- y										#the <<- operator is used to
    m <<- NULL									#to ensure that the variable x
  }												#is assigned the new matrix
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will be used to get the inverse of the matrix, and send it
## back to the main function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}