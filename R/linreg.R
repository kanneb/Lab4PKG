linreg_class <- setRefClass("linreg",
                            fields = list(X = "matrix", y = "numeric",
                                          formula = "formula", beta_hat = "matrix",
                                          y_hat = "matrix", e_hat = "matrix"
                                          ),

                            methods = list(

                              show = function(){
                                "Print the coefficents and their names."
                                coefficnet <- drop(.self$beta_hat)
                                cat("Coefficents: \n")
                                print(coefficnet)

                              },
                              plot = function(){

                              },
                              resid = function(){
                                "Returns the residuals as a vector."
                                drop(.self$e_hat)
                              }





                            )
              )



#' Linear Regression
#'
#'
#'
#'
#'
#'
#'
linreg <- function(formula, data){
    # Creating X matrix and the dependet variable y
    X <- model.matrix(formula, data = data)
    y <- data[[all.vars(formula)[1]]]

    # Regression coefficientsS
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
    var_beta_hat <- as.numeric(sigma_hat2) * (solve(t(X) %*% X))


    # t-values
    t_beta <- beta_hat / sqrt(diag(var_beta_hat))
    return(linreg_class$new(X = X, y = y, formula = formula,
                            beta_hat = beta_hat,y_hat = y_hat, e_hat = e_hat))
  }



data(iris)

data_reg <- linreg(Petal.Length ~ Species, iris)

print(data_reg)

#print((drop(data_reg$resid())))



















