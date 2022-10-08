##Creating a function that creates and caches a matrix

##Assigning a function that creates matrix to variable createcacheMatrix

makeCacheMatrix <- function(mtx = matrix()) {
  x <- NULL
  
  set <- function(matrix) {
    mtx <<- matrix
    x <<- NULL
  }
  
  get <- function() {
    mtx
  }
  
  setInverse <- function(inverse) {
    x <<- inverse
  }

  getInverse <- function() {
    x
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


##Calculates inverse of matrix

cacheSolve <- function(x, ...) {
  
  mtx <- x$getInverse()
  
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data) %*% data
  x$setInverse(mtx)
  mtx
}
