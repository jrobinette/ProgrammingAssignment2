## this function is the same as the example given in the
## assignment to create a special "vector"


makeCacheMatrix <- function(x=matrix()) {
  jen = NULL
  set = function(y) {
    x <<- y
    jen <<- NULL
  }
  get = function() x
  setinv = function(inverse) jen <<- inverse
  getinv = function() jen
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## the following function caching the inverse of the Matrix

cacheSolve <- function (x, ...) {
  jen = x$getinv()
  if(!is.null(jen)) {
    message("getting cached data")
    return(jen)
  }
  mat.data = x$get()
  jen = solve(mat.data, ...)
  x$setinv(jen)
  return(jen)
  
}

## this script tests the results

test = function(matrixN) {
  jentest = makeCacheMatrix(matrixN)
  start.time = Sys.time()
  cacheSolve(jentest)
  dur = Sys.time() - start.time
  print(dur)
  
}

  
