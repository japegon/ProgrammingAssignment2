makeCacheMatrix <- function(mat = matrix()){
  inv <- NULL
  set <- function(y){
    mat <<- y
    inv <<- NULL
  }
  get <- function(){
    mat
  }
  setInv <- function(inverse){
    inv <<- inverse
  }
  getInv <- function(){
    inv
  }
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

cacheSolve <- function(mat, ...){
  inv <- mat$getInv()
  print(mat)
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data)
  mat$setInv(inv)
  inv
}
