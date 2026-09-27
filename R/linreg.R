#' Linear regression object
#'
#' Reference class that stores the results of a linear regression fitted
#' with \code{linreg()}. Objects are created by \code{linreg()}, not directly.
#'
#' Fits a linear regression model with ordinary least squares and returns
#' an object with methods for printing, plotting and summarising the fit.
#'
#' @field X Design matrix.
#' @field y Response variable.
#' @field data The data used to fit the model.
#' @field formula The model formula.
#' @field data_name Name of the data set, used when printing.
#' @field beta_hat Estimated coefficients.
#' @field y_hat Fitted values.
#' @field e_hat Residuals.
#' @field sigma_hat2 Estimated residual variance.
#' @field df Degrees of freedom.
#' @field std_err Standard errors of the coefficients.
#' @field t_beta t-values of the coefficients.
#' @field p_value p-values of the coefficients.
#'
#' @examples
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' mod
#' mod$summary()
#' mod$coef()
#'
#' @importFrom ggplot2 ggplot aes geom_point labs stat_summary
#' @importFrom methods setRefClass new
#' @export linreg
#' @exportClass linreg
linreg <- setRefClass("linreg",
                            fields = list(X = "matrix", y = "numeric", data = "data.frame",
                                          formula = "formula", beta_hat = "matrix",
                                          y_hat = "matrix", e_hat = "matrix",
                                          data_name = "character", sigma_hat2 = "numeric",
                                          df = "numeric", std_err = "numeric", t_beta = "numeric", p_value = "numeric"
                                          ),
                            methods = list(
                              initialize = function(formula, data){

                                stopifnot(
                                  "formula must be a formula" = inherits(formula, "formula"),
                                  "data must be a data.frame" = is.data.frame(data),
                                  "all variables in formula must exist in data" = all(all.vars(formula) %in% names(data)),
                                  is.numeric(data[[all.vars(formula)[1]]]),
                                  !anyNA(data[all.vars(formula)])
                                )

                                data_name <<- deparse(substitute(data))
                                formula <<- formula
                                data <<- data

                                # Creating X matrix and the dependet variable y
                                X <<- model.matrix(formula, data = data)
                                y <<- data[[all.vars(formula)[1]]]

                                # Regression coefficientsS
                                beta_hat <<- (solve(t(X) %*% X)) %*% (t(X) %*% y)

                                # Fitted val
                                y_hat <<- X %*% beta_hat

                                # Residuals
                                e_hat <<- y - y_hat

                                # Degrees of freedom
                                n <- nrow(X)
                                p <- ncol(X)
                                df <<- n - p

                                # Residual variance
                                sigma_hat2 <<- as.numeric((t(e_hat) %*% e_hat) / df)

                                # Variance of regression coefficients
                                var_beta_hat <- as.numeric(sigma_hat2) * (solve(t(X) %*% X))


                                # t-values
                                std_err <<- sqrt(diag(var_beta_hat))
                                t_beta  <<- drop(beta_hat) / std_err

                                p_value <<- 2 * pt(abs(t_beta), df, lower.tail = FALSE)
                              },

                              print = function(){
                                "Print the coefficents and their names."
                                coefficnet <- drop(.self$beta_hat)
                                cat("Call:\n")
                                cat("linreg(formula = ",deparse(formula),", data = ",.self$data_name,")\n\n",sep="")
                                cat("Coefficents: \n")
                                base::print(coefficnet)
                              },

                              show = function(){
                                "Prints the object, same as print()."
                                print()
                              },

                              plot = function(){
                                "Plots Residuals vs Fitted and Scale-Location using ggplot2."
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
                                                       x = x_lable, y = "Residuals")+
                                                theme_liu()

                                plot2 <- ggplot(data_combined, aes(x = y_hat, y = e_std)) +
                                                geom_point(shape = 1)+
                                                stat_summary(fun = median, geom = "line", color = "red") +
                                                labs(title = "Scale-Location",
                                                     x = x_lable, y = expression(sqrt(abs("Standardized residuals"))))
                                base::print(plot1)
                                base::print(plot2)

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
                                stars <- ifelse(.self$p_value < 0.001, "***",
                                                ifelse(.self$p_value < 0.01, "**",
                                                       ifelse(.self$p_value < 0.05, "*", "")))
                                tab <- data.frame(tab, stars, check.names = FALSE)
                                base::print(tab)
                                cat("\nResidual standard error:", sqrt(.self$sigma_hat2),
                                    "on", .self$df, "degrees of freedom\n")
                              }
                            )
              )

