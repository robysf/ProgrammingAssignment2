### This code defines two functions, makeCacheMatrix and cacheSolve, that together allow one
### to evaluate the Inverse of a matrix if not previously cached, and to return the cached value
### if it has been previously calculated and cached

## makeCacheMatrix returns a list of functions that allow one to make and query the cached value
## for the inverse, Inv, of a matrix, x

## (since I'm new to coding, I probably over-commented below)
## (but it helped me to try to understand what each line was doing)

makeCacheMatrix <- function(x = matrix()) {   # input argument is a matrix, x
  Inv <- NULL                                 # initializes the inverse to a null value locally
  set <- function(y) {                        # defines the function set...
    x <<- y                                   # ...which allows you to set a value for x and...
    Inv <<- NULL                              # ...sets Inv to null globally if not eval'd in...
  }                                                  # ...the parent env't already
  get <- function() x                         # defines the fcn get, which prints value of x
  setInv <- function(solve) Inv <<- solve     # defines fcn setInv = caches inverse of x in Inv
  getInv <- function() Inv                    # defines fcn getInv, which returns Inv if cached
  list(set = set, get = get, setInv = setInv, getInv = getInv) # output of fcn is a list of fcns
}


## cacheSolve looks to see if the inverse of the input matrix, x, has already been cached
## if so, it simply returns the cached inverse and exits
## if not, it calculates the inverse, caches it globally, and returns the calculated value

cacheSolve <- function(x, ...) {              # input argument is a matrix, x
  ## Return a matrix that is the inverse of 'x'  
  Inv <- x$getInv                             # query cache
  if(!is.null(Inv)) {                         # if an inverse of matrix x is cached...
    message("getting cached data")            # ...then tell me so...
    return(Inv)                               # ...print cached value of Inv and exit the fcn
  }                                           # (if no inverse was cached)
  data <- x$get()                             # store the matrix x as "data"
  Inv <- solve(data, ...)                     # calculate its inverse, assign to Inv locally...
  x$setInv(x)                                 # ...& save this globally in the cached value Inv...
  Inv                                         # then print the calculated value of Inv
}

