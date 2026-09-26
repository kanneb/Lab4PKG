#' Linear Regression object
#'
#'
#'@importFrom ggplot2 ggplot aes geom_point labs stat_summary
linreg_class <- setRefClass("linreg",
                            fields = list(X = "matrix", y = "numeric", data = "data.frame",
                                          formula = "formula", beta_hat = "matrix",
                                          y_hat = "matrix", e_hat = "matrix",
                                          data_name = "character", sigma_hat2 = "numeric",
                                          df = "numeric", std_err = "numeric", t_beta = "numeric", p_value = "numeric"
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
                                x_lable = paste0("Fitted Values \nlm(",deparse(.self$formula),")")

                                plot1 <- ggplot(data_combined, aes(x = y_hat, y = e_hat)) +
                                                geom_point(shape = 1)+
                                                stat_summary(fun = median, geom = "line", color = "red") +
                                                labs(title = "Residuals vs Fitted",
                                                       x = x_lable, y = "Residuals")

                                plot2 <- ggplot(data_combined, aes(x = y_hat, y = e_std)) +
                                                geom_point(shape = 1)+
                                                stat_summary(fun = median, geom = "line", color = "red") +
                                                labs(title = "Scale-Location",
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
                              },

                              summary = function() {
                                "Prints coefficients, standard errors, t-values, p-values, sigma and df."
                                tab <- cbind("Estimate"     = drop(.self$beta_hat),
                                             "Std.Error" = .self$std_err,
                                             "t value"    = .self$t_beta,
                                             "Pr(>|t|)"   = .self$p_value)
                                print(tab)
                                cat("\nResidual standard error:", sqrt(.self$sigma_hat2),
                                    "on", .self$df, "degrees of freedom\n")
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
    std_err <- sqrt(diag(var_beta_hat))
    t_beta  <- drop(beta_hat) / std_err

    p_value <- 2 * pt(abs(t_beta), df, lower.tail = FALSE)

    return(linreg_class$new(X = X, y = y, data= data, formula = formula,
                            beta_hat = beta_hat,y_hat = y_hat, e_hat = e_hat,
                            data_name = data_name, sigma_hat2 = as.numeric(sigma_hat2),
                            df = df, std_err = std_err, t_beta = t_beta, p_value = p_value))
  }



















