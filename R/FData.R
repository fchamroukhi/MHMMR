FData <- setRefClass(
  "FData",
  fields = list(
    X = "numeric", # Covariates
    Y = "matrix", # Response
    m = "numeric",
    n = "numeric",
    vecY = "matrix"
  ),
  methods = list(
    setData = function(X, Y) {

      Y <<- as.matrix(Y)

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
