model <- setRefClass(
  "linreg",
  fields = list(
    data = "data.frame",
    formula = "formula"
  ),
  methods = list(
    initialize = function(formula, data){
      # Creating X matrix and the dependet variable y
      X <- model.matrix(formula, data = data)
      y <- data[[all.vars(formula)[1]]]

      # Regression coefficients
      beta_hat <- (solve(t(X) %*% X)) %*% (t(X) %*% y)

      # Fitted val
      y_hat <- X %*% beta_hat

      # Residuals
      e_hat <- y - y_hat

      # Degrees of freedom
      n <- nrow(X)
      p <- ncol(X)

      df <- n - p
    }




  )
)


linreg <- function(formula, data) {
  model$new
}


data("iris")
test <-model.matrix(iris$Species ~.,,data = iris)
