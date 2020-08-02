cachematrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
  x <<- y
  a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}
cachesolution <- function(x, ...) {
  a <- x$getInverse()
  if(!is.null(a)){
    return(a)
  }
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}