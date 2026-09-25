
linear_reg <- function(formula, data){
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

    # Residual variance
    sigma_hat2 <- (t(e_hat) %*% e_hat) / df

    # Variance of regression coefficients
    var_beta_hat <- sigma_hat2 %*% (solve(t(X) %*% X))

    # t-values
    t_beta <- beta_hat / sqrt(var_beta_hat)
  }
)




