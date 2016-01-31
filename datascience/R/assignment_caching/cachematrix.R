# Create a caching object for a matrix
makeCacheMatrix <- function(x = matrix()) {
  matrix.inverse = NULL

  matrix.set = function(new_matrix) {
    matrix.inverse <<- NULL
    x  <<- new_matrix
  }

  matrix.get = function() { x }
  matrix.inv = function() {
    if (is.null(matrix.inverse)) { matrix.inverse <<- solve(x) }
    matrix.inverse
  }

  # return access functions
  list(set = matrix.set,
       get = matrix.get,
       inv = matrix.inv)
}


# redundent... logic exists in caching obj.
cacheSolve <- function(x, ...) {
  x$inv()
}




test = function() {
  print('Create Data')
  matrix.test.1 = matrix(runif(3**2, 1, 100), 3, 3)
  matrix.test.2 = matrix(runif(4**2, 1, 100), 4, 4)
  matrix.test.3 = matrix(runif(5**2, 1, 100), 5, 5)
  matrix.test.4 = matrix(runif(16**2, 1, 100), 16, 16)

  print('Create Cache, w 3x3')
  cm = makeCacheMatrix(matrix.test.1)

  print('')
  print("Cache-get: 3x3")
  print(cm$get())

  print('')
  print('Cache-inv: 5x5')
  print(cm$inv())

  print('')
  print('Cache-set: 4x4')
  print(cm$set(matrix.test.2))

  print('')
  print('Cache-inv: 4x4')
  print(cm$inv())
  print(cm$inv())

  print('')
  print('Create Cache: 5x5')
  cm5 = makeCacheMatrix(matrix.test.3)
  print('now invert')
  print(cacheSolve(cm5))
}