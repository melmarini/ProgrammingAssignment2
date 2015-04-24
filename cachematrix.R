## MeM 12/04/2015 Coursera R Programming: Programming assignment 2

## This function returns a list with functions to set/get original matrix and inverse in memory
makeCacheMatrix <- function(mat = matrix()) 
{
  
  ## Empty matrixinverse variable
  matinv <- NULL
  
  ## Specified matrix is set in memory
  ## Inverse is set to 0 (failsafe to not calculate with a wrong cached inverse)
  set <- function(y) 
  {
    mat <<- y
    matinv <<- NULL
  }
  
  ## Get the specified matrix stored in memory
  get <- function() mat
  
  ## Set the inverse of the specified matrix in memory
  setinverse <- function(solveminv) matinv <<- solveminv
  
  ## Get the inverse stored in memory
  getinverse <- function() matinv
  
  ## Returns a list of the functions above
  list(
    set = set 
    ,get = get
    ,setinverse = setinverse
    ,getinverse = getinverse
  )
  
}

## MeM 12/04/2015 Coursera R Programming: Programming assignment 2

## This functions returns the inverse of a matrix if already calculated, otherwise it calculates
## the inverse and stores it in memory. 
cacheSolve <- function(x, ...) 
{
  
  ## Returns a matrix that is the inverse of the variable mat
  matinv <- x$getinverse()
  
  ## Returns the inverse if it has already been calculated and stored in memory
  if(!is.null(matinv)) 
  {
    message("getting cached data")
    return(matinv)
  }
  
  ## Calculate the inverse and stores it in memory, if the inverse was not found in memory 
  mat <- x$get() 
  matinv <- solve(mat) 
  x$setinverse(matinv)
  
  ## Prints the inverse
  print(matinv)
  
}





