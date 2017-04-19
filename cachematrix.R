## INSTRUCTIONS TO RUN
##
## 1. Import function
## 2. Define a matrix (e.g. b <- diag(3,6))
## 3. Define the matrix's cache (e.g. func <- makeCacheMatrix(b))
## 4. Run cacheSolve function (e.g. cacheSolve(func))
## 5. Run cacheSolve again to recover cached inverse
## 6. Repeat for any matrix, making sure to make a new matrix cache for each matrix (step 3)



## Creates an object that stores a matrix and can store its inverse once calculated.
##
##
makeCacheMatrix <- function(x = matrix()) {
  ## Define inv variable  
  inv <- NULL
  
  ## Never actually used in my method of calling functions  
  set <- function(y) {
    x <<- y
    inv <<- NULL  }
  
  ## Used by cacheSolve to retrieve matrix       
  get <- function() x
  
  ## Sets inv to calculated inverse from cacheSolve function     
  setInverse <- function() inv <<- inverse
  
  ## Called by cacheSolve function to determine whether it 
  ## it needs to calculate an inverse (if null) 
  getInverse <- function() inv
  
  ## Defines func variable  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}






## Calculates the inverse of the matrix. If the inverse is stored in the cache, then the 
## function retrieves it rather than recalculating the inverse.
##
##
cacheSolve <- function(x, ...) {
  ## Imports inv variable (null if new matrix)  
  inv <- x$getInverse()
  
  ## If imported inv is not null, then no need to recalculate
  ## inverse, simply export the inv matrix and exit rest of the
  ## cacheSolve function
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) }
  
  ## In inv is null, then import the matrix that needs to be 
  ## inverted (data=imported_matrix)
  data <- x$get()
  
  ## Set variable inverse to equal the inverse of the imported
  ## matrix
  inverse <<- solve(data, ...)
  
  ## Call the setInverse function which will set inv to the 
  ## calculated inverse (set inv to inverse)
  x$setInverse()
  
  ## Print the inverse matrix   
  inverse  
}
