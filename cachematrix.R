## This function will calculate the inverse of a given matrix, checking if ##that inversion calculation has already been run. If already run, the ##inversion results will have been cached.


## The makeCacheMatrix function first sets arguments to NULL if the second function has not yet been run. Second part of this function creates a list of functions to provide order for the second function. The functions in this list set values of a matrix, caches the matrix that is input to check against an output.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## The cacheSolve function checks if the inverse of a matrix has already ben calculated, and if so returns message text.If this function does not find a previously existing inverse matrix, an inverse is calculated.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached inverse matrix solution")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
