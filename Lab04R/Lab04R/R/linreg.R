#' Calculate greatest common divisor
#' The class for linear regression
#' @name linreg
#' @rdname linreg
#' @aliases  linreg 
#' @title Linear Regression Class
#' @param formula formula
#' @param data The data frame on which we want to do regression.@return nothing; Create a class which calculate linear regression \code{Formula} and \code{data}.
#' @references \url{https://en.wikipedia.org/wiki/Least_squares}
#' @references \url{https://en.wikipedia.org/wiki/QR_decomposition}
#' @import methods
#' @import ggplot2
#' @export linreg
#' 
#' @field data The data frame on which we want to do regression.
#' 
#' @field formula  The data frame on which we want to do regression.
#' @field X The Model matrix of independent variables.
#' @field Y the matrix of dependent variables
#' @field k number of rows in coefficient to calculate the df
#' @field call it return the call of the function
#' @field coefficient return the coefficient
#' @field fitted.values return the fitted value of Y
#' @field transposeX just the transpose of the x
#' @field df the Degree of freedom
#' @field residual.values returnt he value of residuals
#' @field residualvariance return the variance of residial 
#' @field coefvariance return the variance of coefficient
#' @field dependentVariable return the column name of dependent variable from the formula
#' @field qr_coef return the coef using QR methonf
#' @field qr_coefvariance returnt he varinace of QR coefficient
#' 
#' Some length description maybe it works
#' @examples 
#'
#' linregObject = linreg$new(Petal.Length~Species, data=iris)
#' linregObject$summary()
#' 
linreg <- setRefClass(Class = "linreg",
                      fields = list(
                        formula='formula',
                        data = "data.frame",
                        X="matrix",
                        Y="matrix",
                        k = "numeric",
                        call="call",
                        coefficient = "matrix",
                        fitted.values = "matrix",
                        transposeX = "matrix",
                        df = "numeric",
                        residual.values = "matrix",
                        residualvariance = "vector",
                        coefvariance = "matrix",
                        dependentVariable = "character",
                        qr_coef = "matrix",
                        qr_coefvariance = "matrix"
                      ))

linreg$methods (
  #' @name initialize
  #' @title initialize object
  #' @description method description. method description
  #' @param formula parameter description
  #' @param data df
  initialize= function(formula,data) {
    "Initialize the class and run all the function to calculate the linear regrassion"
    .self$formula = formula
    .self$data = data
    .self$X = model.matrix(formula,data)
    .self$dependentVariable = all.vars(formula)[[1]]
    .self$Y = as.matrix(data[dependentVariable])
    .self$call  = match.call(expand.dots = TRUE,call = call('linreg', formula, data = substitute(data)))
    .self$transposeX = t(.self$X)
    .self$coefficient = .self$coef_matrix()
    .self$fitted.values = .self$fitted(.self$coefficient)
    .self$residual.values = .self$residual(.self$fitted.values)
    .self$df = .self$df_function()
    .self$residualvariance = .self$residual_variance(.self$residual.values)
    .self$coefvariance = .self$coef_variance(.self$residualvariance)
  })

linreg$methods(
  #' @name coef
  #' @title method description
  coef_matrix = function() {
    "Calculuating the coefficent"
    inverse = solve((.self$transposeX %*% .self$X))
    coef_result = inverse %*% .self$transposeX %*% .self$Y
    .self$k = nrow(coef_result)
    return(coef_result)
  })

linreg$methods (
  #'@name fitted
  #' @title method description
  #' @param coef parameter description
  fitted = function(coef)
  {
    "calculating the fitted Value"
    fitted_value = .self$X %*% coef
    return(fitted_value)
  })

linreg$methods (
  #'@name pred
  #' @title method description
  pred = function()
  {
    "calculating the fitted Value"
    return(as.vector(.self$fitted.values))
  })

linreg$methods (
  #'@name resid
  #' @title method description
  resid = function()
  {
    "calculating the fitted Value"
    return(as.vector(.self$residual.values))
  })
linreg$methods (
  #'@name coef
  #' @title method description
  coef = function()
  {
    "calculating the fitted Value"
    vec = as.vector(.self$coefficient)
    names(vec) = rownames(.self$coefficient)
    return(vec)
  })

linreg$methods (
  #'@name residual
  #' @title method description
  #' @param fit_matrix parameter description
  residual = function(fit_matrix)
  {
    "calculating the residual values"
    residual_matrix_result = .self$Y - fit_matrix
    # residual_result = as.vector(residual_matrix_result)
    # names(residual_result) = 1:length(residual_matrix_result)
    return(residual_matrix_result)
  })

linreg$methods (
  
  #'@name df_function
  #' @title method description
  #' method description
  df_function = function()
  {
    "calculating the df"
    n = nrow(.self$X)
    return(n - .self$k)
  })

linreg$methods (
  #'@name residual_variance
  #' @title method description
  #' method description
  #' @param residual_matrix parameter description
  residual_variance = function(residual_matrix)
  {
    "calculating the residual variance"
    residual_transpose = t(residual_matrix)
    num = residual_transpose %*% residual_matrix
    return(as.vector(num/.self$df))
  })

linreg$methods (
  #'@name coef_variance
  #' @title method description
  #' method description
  #' @param residual_variance parameter description
  coef_variance = function(residual_variance)
  {
    "calculate the coef variance"
    inverse = solve(.self$transposeX %*% .self$X)
    return ( residual_variance * inverse)
  })

linreg$methods (
  #'@name pt
  #' @title method description
  #' method description
  pt = function()
  {
    "PT"
    coef_vector = as.vector(.self$coefficient)
    return(pt(coef_vector,df=.self$df))
  })

linreg$methods (
  #'@name stdError
  #' @title method description
  #' method description
  stdError = function()
  {
    "ERROR"
    return(sqrt(diag(.self$coefvariance)))
  })

linreg$methods (
  #'@name tValue
  #' @title method description
  #' method description
  tValue = function()
  {
    "calculate the tValue"
    return(.self$coefficient/sqrt(diag(.self$coefvariance)))
  })

linreg$methods (
  #'@name pValue
  #' @title method description
  #' method description
  pValue = function()
  {
    "pvalue"
    p_value = do.call(pt,list(abs(.self$tValue()),df=.self$df,lower.tail = FALSE))
    return(2 *p_value)
  })

linreg$methods (
  #'@name plot
  #' @title method description
  #' method description
  plot = function()
  {
    "Generating the plot using GGPlot"
    fitted_value = .self$fitted.values
    r_varianace = .self$residualvariance
    r = .self$residual.values
    root_stand_r = sqrt(abs(r/sqrt(r_varianace)))
    base::print(qplot(fitted_value,r,xlab = 'Fitted Values',ylab = 'Residual',main = 'Residual vs Fitted') +
                  theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method = 'lm',color="red", se=FALSE))
    
    base::print(qplot(fitted_value,root_stand_r,xlab = 'Fitted Values',ylab = expression(sqrt(abs('Standardized residuals'))),main = 'Scale-Location') +
                  theme(plot.title = element_text(hjust = 0.5))+ geom_smooth(method = 'lm',color="red", se=FALSE))
  })

linreg$methods (
  #'@name print
  #' @title method description
  #' method description
  print = function()
  {
    "print function like lm print"
    vec_coe = as.vector(.self$coefficient)
    names(vec_coe) = rownames(.self$coefficient)
    cat("\nCall:\n", paste(deparse(.self$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
    if (length(.self$coefficient)) {
      cat("Coefficients:\n")
      print.default(format(vec_coe), print.gap = 2L,quote = FALSE)
    }
    else
    {
      cat("No coefficients\n")
    }
    
  })

linreg$methods (
  #'@name summary
  #' @title method description
  #' method description
  summary = function()
  {
    "summary function like lm summary"
   
    
    # vec_coe = as.vector(.self$coefficient)
    # vec_coe = c(vec_coe,as.vector(.self$stdError()))
    # vec_coe = c(vec_coe,as.vector(.self$tValue()))
    star_column <- c("***", "***", "***")
    coef_matrix <- cbind(round(.self$coefficient,5), round(.self$stdError(),5), round(.self$tValue(),5),.self$pValue(), star_column)
    colnames(coef_matrix) <- c("Estimate", "Std. Error", "t value", "p value", " ")
    print.table(coef_matrix)
    
    cat('Residual standard error:', sqrt(residualvariance))
    cat(' on ')
    cat(.self$df,"degrees of freedom:\n")
    
  })

linreg$methods (
  #'@name decomposition
  #' @title method description
  #' method description
  decomposition = function() {
    "calculating the QR from X"
    x = .self$X
    n = ncol(x)
    m = nrow(x)
    q = matrix(0, m, n)
    r = matrix(0, n, n)
    for (j in 1:n) {
      v = x[,j] 
      if (j > 1) {
        for (i in 1:(j-1)) {
          r[i,j] = t(q[,i]) %*% x[,j]
          v = v - r[i,j] * q[,i] 
        }      
      }
      r[j,j] = sqrt(sum(v^2))
      q[,j] = v / r[j,j]
    }
    qrcomp = list('Q'=q, 'R'=r)
    return(qrcomp)
  })

linreg$methods (
  #'@name qrcoef
  #' @title method description
  #' method description
  qrcoef = function()
  {
    "Calcualute Q-1 Rt X"
    qr_matrix =.self$decomposition();
    estimationMatrix  = (solve(qr_matrix$R) %*% t(qr_matrix$Q)) %*% .self$Y
    colnames(estimationMatrix) = dependentVariable
    rownames(estimationMatrix) = colnames(.self$X)
    .self$qr_coef = estimationMatrix
    return(estimationMatrix)
  })

linreg$methods (
  #'@name qr_coef_variance
  #' @title method description
  #' method description
  qr_coef_variance = function()
  {
    "Calcualte Variance"
    coef = .self$qrcoef()
    fitted_values = .self$fitted(coef)
    residual_values = .self$residual(fitted_values)
    residual_variance = .self$residual_variance(residual_values)
    coef_variance = .self$coef_variance(residualvariance)
    .self$qr_coefvariance = coef_variance;
    return(coefvariance)
  })

