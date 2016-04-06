# The functions makeCacheMatrix() and cacheSolve() work in conjunction with each
#   other to calculate the inverse of an invertible matrix and to cache the
#   result, returning the cached value if cacheSolve() is called on a matrix for
#   which the inverse matrix is already cached, preventing unnecessary use of
#   processing time.



# A call to makeCacheMatrix() returns a list of four functions and stores the
#   value of the matrix passed as an argument, if any. The value of this matrix
#   can subsequently be changed with a call to set().
#
#   Example: X = makeCacheMatrix( diag( 3)) would accept the 3x3 identity matrix
#            as an argument and assign the returned list as the value of X. The
#            3x3 identity matrix would be stored as x in the environment that
#            created the functions in the returned list. A subsequent call of
#            X$set( diag( 2)) would result in the
#            value of x in the environment in which set() was created to be
#            changed to the 2x2 identity matrix. (Note that the identity matrix
#            is its own inverse.)
#
#            Consider the following code:
#
#                 > X = makeCacheMatrix( diag( 3))
#				  >
#			      > cacheSolve( X)
#			           [,1] [,2] [,3]
#				  [1,]    1    0    0
#				  [2,]    0    1    0
#				  [3,]    0    0    1
#				  >
#				  > ls( environment( X$set))
#				  [1] "get.X"         "get.X.inverse" "set"        "set.inverse"  
#				  [5] "x"             "X.inverse"    
#				  > 
#
#            The objects in the environment in which X was created are shown.
#            These are the objects that are modified with the "<<-" assignment
#            operator in the functions contained by X.


makeCacheMatrix = function( x = matrix()) {
	
	X.inverse = NULL
		# creates object X.inverse
	
	set = function( y) {
		x <<- y
			# Changes the value of x, the argument of makeCachMatrix(), to y,
			#   the argument of set().
			
		X.inverse <<- NULL
			# Resets the value of X.inverse, if it has one, to NULL
			
	}
		# Creates a function that allows user to change the value of the base
		#   matrix passed to makeCacheMatrix() and resets the value of X.inverse
		#   to NULL.
	
	get.X = function() x
		# Creates a function that returns and displays the value of x, the
		#   argument of makeCachMatrix(). Note that the value of x could have
		#   previously been changed
		#   by set(), subsequent to the original makeCacheMatrix() call.
	
	set.inverse = function( inverse) X.inverse <<- inverse
		# Creates function that assigns the value of its argument to X.inverse.
	
	get.X.inverse = function() X.inverse
		# Creates a function that returns and displays the value of X.inverse.
	
	list( set = set, get.X = get.X, set.inverse = set.inverse,
		get.X.inverse = get.X.inverse)
		# Returns a list of the four functions created. It does not return the
		#   value of the original matrix passed as an argument.
}




# The argument for cacheSolve() should be a list returned by a call of
#   makeCacheMatrix(). cacheSolve() returns the inverse of x, the matrix passed
#   as an argument to makeCacheMatrix(), or the modified value of x, as
#   described in the example provided in the comment preceding the
#   makeCachMatrix() definition. If the inverse of x has not yet been
#   calculated, cacheSolve() calculates, caches, and returns the value of the
#   inverse. If the inverse has been previously calculated, the value is
#   retrieved from cache and returned, along with a message informing the user
#   that the returned value is from cache.



cacheSolve = function( x, ...) {
		# Return a matrix that is the inverse of 'x'
	
	X.inverse = x$get.X.inverse()
		# Calls get.X.inverse() of x, which is presumably a list returned by
		#   makeCacheMatrix(), and assigns the returned value to X.inverse.
		#   Note that this variable "X.inverse" is different from the variable
		#   "X.inverse" created in the makeCacheMatrix() function call.
		
	if (!is.null( X.inverse)) {	
		# Condition checks to see if the value of X.inverse is not NULL.
		
		message( "Getting cached data ...")
			# Informs user that inverse is not being calculated; a cached value
			#   will be returned.
			
		return( X.inverse)
			# Cached value of matrix inverse, i.e., X.inverse, is returned.
	}
	
	DATA = x$get.X()
		# Assigns current value of the matrix passed as argument of
		#   makeCacheMatrix() to variable DATA.
	
	X.inverse = solve( x$get.X(), ...)
		# Calculates inverse of current value of the matrix passed as argument
		#   of makeCacheMatrix(), which is itself obtained by calling get.X(),
		#   and assigns the value of that inverse to X.inverse. Again, note that
		#   this variable "X.inverse" is different from the variable "X.inverse"
		#   created in the makeCacheMatrix() function call.
	
	x$set.inverse( X.inverse)
		# Assigns value of variable X.inverse created cacheSolve() call to
		#   X.inverse created in makeCacheMatrix() function call.
	
	X.inverse
		# Returns value of variable X.inverse
}