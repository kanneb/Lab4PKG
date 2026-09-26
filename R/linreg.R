#' Linear Regression object
#'
#'
#'@importFrom ggplot2 ggplot aes geom_point labs geom_smooth stat_summary
linreg_class <- setRefClass("linreg",
                            fields = list(X = "matrix", y = "numeric", data = "data.frame",
                                          formula = "formula", beta_hat = "matrix",
                                          y_hat = "matrix", e_hat = "matrix",
                                          data_name = "character", sigma_hat2 = "numeric"
                                          ),
                            methods = list(

                              show = function(){
                                "Print the coefficents and their names."
                                coefficnet <- drop(.self$beta_hat)
                                cat("Call:\n")
                                cat("linreg(formula = ",deparse(formula),", data = ",.self$data_name,")\n\n",sep="")
                                cat("Coefficents: \n")
                                print(coefficnet)

                              },

                              plot = function(){
                                data_combined <- data.frame(
                                  y_hat = drop(.self$y_hat),
                                  e_hat = drop(.self$e_hat),
                                  e_std = sqrt(abs(.self$e_hat/(sqrt(.self$sigma_hat2))))
                                )
                                data_combined <- cbind(data_combined, .self$data)
                                x_lable = paste0("Fitted Values \nlm(",deparse(.self$formula),")")


                                plot1 <- ggplot(data_combined, aes(x = y_hat, y = e_hat)) +
                                                geom_point(shape = 1)+
                                                stat_summary(fun = median, geom = "line", color = "red") +
                                                labs(title = "Residuals vs Fitted",
                                                       x = x_lable, y = "Residuals")

                                plot2 <- ggplot(data_combined, aes(x = y_hat, y = e_std)) +
                                                geom_point(shape = 1)+
                                                stat_summary(fun = median, geom = "line", color = "red") +
                                                labs(title = "Scale−Location",
                                                     x = x_lable, y = expression(sqrt(abs("Standardized residuals"))))
                                print(plot1)
                                print(plot2)

                              },

                              resid = function(){
                                "Returns the residuals as a vector."
                                drop(.self$e_hat)
                              },

                              pred = function(){
                                "Returns the predicted Y values."
                                drop(.self$y_hat)
                              },

                              coef = function(){
                                "Returns the the coefficients as a named vector."
                                drop(.self$beta_hat)
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
#'@export
linreg <- function(formula, data){
    data_name = deparse(substitute(data))
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
    return(linreg_class$new(X = X, y = y, data= data, formula = formula,
                            beta_hat = beta_hat,y_hat = y_hat, e_hat = e_hat,
                            data_name = data_name, sigma_hat2 = as.numeric(sigma_hat2)))
  }



#data(iris)

#data_reg <- linreg(Petal.Length ~ Species, iris)

#print(data_reg)
#data_reg$plot()
#print(data_reg$resid())


















