## makeCacheMatrix sets  the value for x (as input matrix) and inv as null
## inv is assigned null, so that on previous value gets erased from memory

## after setting the value, get/getMatrix function passed the value to the next function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<-NULL
  }
  get<-function() x
  setMatrix<-function(solve) inv<<-solve
  getMatrix<-function() inv
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## cacheSolve will calculate the inverse of the matrix it retrievs from makeCacheMatrix$getMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getMatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setMatrix(inv)
  inv
}
