#' Ridge regression object
#'
#' Reference class that stores the results of a ridge regression object, fitted
#' with \code{ridgereg()}
#'
#' Ridge regression is a modified linear regression that uses L2 regularization.
#' It adds a penalty term that shrinks the coefficients of the object.
#'
#' @field X Design matrix.
#' @field y Response variable.
#' @field I Identity matrix
#' @field lambda Ridge regularization parameter, controlling the size of the penalty on the coefficients.
#' @field data The data used to fit the model.
#' @field formula The model formula.
#' @field data_name Name of the data set, used when printing.
#' @field beta_hat Estimated coefficients.
#' @field y_hat Fitted values.
#'
#' @examples
#' frm <- formula(Petal.Length~Sepal.Width+Sepal.Length)
#' obj <- ridgereg(frm, data = iris, lambda = 1)
#' obj$predict()
#' obj$coef()
#' obj$print()
#'
#' @importFrom methods setRefClass new
#'
#' @export ridgereg
#' @exportClass ridgereg

ridgereg <- setRefClass("ridgereg",
    fields = list(
      formula = "formula",
      data = "data.frame",
      X = "matrix",
      I = "matrix",
      y = "numeric",
      lambda = "numeric",
      beta_hat = "matrix",
      data_name = "character",
      y_hat = "matrix"
    ) ,

    methods = list(
      initialize = function(formula, data, lambda){
        stopifnot(
          "formula must be a formula" = inherits(formula, "formula"),
          "data must be a data.frame" = is.data.frame(data),
          "lambda must be a numeric and non-negative" = is.numeric(lambda) && lambda >= 0
        )

        data_name <<- deparse(substitute(data))
        formula <<- formula
        data <<- data

        # Creating X matrix and the dependet variable y and matrix I
        X <<- model.matrix(formula, data = data)
        y <<- data[[all.vars(formula)[1]]]
        I <<- diag(ncol(X))

        # x_norm
        X_norm <- X
        x_mean <- colMeans(X[,-1, drop = FALSE])
        x_sd <- apply(X[,-1, drop = FALSE], FUN = sd, MARGIN = 2)
        X_norm[,-1] <- scale(X[,-1, drop = FALSE], center = x_mean, scale = x_sd)

        # Regression coefficients
        beta_hat <<- ( solve( (t(X_norm)%*%X_norm) + (lambda*I) ) ) %*% ( t(X_norm)%*%y )

        # Fitted values
        y_hat <<- X_norm %*% beta_hat
      } ,

      print = function(){
        "Print the coefficients and their names."
        coefficient <- drop(.self$beta_hat)
        cat("Call:\n")
        cat("ridgereg(formula = ",deparse(formula),", data = ",.self$data_name,")\n\n",sep="")
        cat("Coefficients: \n")
        base::print(coefficient)
      } ,

      predict = function(newdata = NULL){
        "Predicting values on newdata, printing out predicted values if newdata argument
        has input"
        if (is.null(newdata)) {
          return(y_hat)
        }

        stopifnot(
          "newdata has to be a data.frame" = is.data.frame(newdata)
        )

        X_new <- model.matrix(frm, data = newdata)

        X_new[,-1] <- scale(X_new[,-1], center = x_mean, scale = x_sd)

        y_hat_new <- X_new %*% beta_hat

        y_hat_new
      } ,

      coef = function(){
        "Return coeffeciients"
        drop(.self$beta_hat)
      }
    )
)
