FData <- setRefClass(
  # Set the name for the class
  "FData",

  # Define the fields
  fields = list(
    X = "matrix",
    # covariates
    Y = "matrix",
    # series
    m = "numeric",
    n = "numeric",
    vecY = "matrix"
  ),

  # Set the methods
  methods = list(
    # set Data from a file
    setData = function(X, Y) {
      Y <<- Y
      setDataProperties()
      if (n == 1) {
        Y <<- t(Y)
      }

      X <<- X

    },


    setDataProperties = function() {
      n <<- nrow(Y)
      m <<- ncol(Y)
      vecY <<- matrix(t(Y), ncol = 1)
    }

  )
)
