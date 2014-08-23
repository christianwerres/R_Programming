
## These script can be used to cache the inverse of a matrix.
## Assumption: the matrix supplied is invertible.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	  A_1 <- NULL
	  set <- function(B) {
		A <<- B
		m <<- NULL
	  }
	  get <- function() A
	  setInvers <- function(A) A_1 <<- solve(A)
	  getInvers <- function() A_1
	  list(set = set, get = get,
		   setInvers = setInvers,
		   getInvers = getInvers)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	  A_1 <- x$getInvers()
	  if(!is.null(A_1)) {
		message(" getting cached data ")
		return(A_1)
	  }
	  message(" no invers matrix available \n calculating...")
	  data <- x$get()
	  A_1 <- x$setInvers(data)
	  print(A_1)		
}


## example code:
# A <- rbind(c(0,1,0), c(0,0,1), c(1,0,0))
# tmp <- makeCacheMatrix(A)

# tmp$getInvers()
## not calculated yet

## calculate or get invers of matrix:
# cacheSolve(tmp)
