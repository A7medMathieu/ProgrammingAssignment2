## Put comments here that give an overall description of what your
## functions do

## set and get the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  ##declare inverse variable to save the inverse of matrix  
  inverse<-NULL
  ##setter and getter for the original matrix function 
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  ##end of setter and getter for the original matrix function
  #-----------------------------------
  ##setter and getter for the inverse matrix function 
  setInverse<-function(inv)  inverse<<-inv
  getInverse<-function()  inverse
  ##end setter and getter for the inverse function matrix function
  
  ##return list of setters and getters functions 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x' and save it in 
##makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## get cached matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## if the function doesn't have inverse 
  ##get the matrix
  data <- x$get()
  ##inverse the matrix by built-in function solve()
  inv <- solve(data)
  ##set inverse matrix 
  x$setInverse(inv)
  ##return it 
  inv      
}
