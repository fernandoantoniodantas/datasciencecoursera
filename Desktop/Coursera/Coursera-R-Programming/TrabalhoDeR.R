makeCacheMatrix <- function(a = matrix()) {
        i <- NULL
        
        set <- function(y) 
        {
                 a <<- y
                 i <<- NULL
         }
        
         get <- function() a
         setInverse <- function(inverse) i <<- inverse
         getInverse <- function() i
         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(a, ...) {
         i <- a$getInverse()
         matr <- a$get()
         i <- solve(matr, ...)
         a$setInverse(i)
         i
}

#my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#my_matrix$get()

