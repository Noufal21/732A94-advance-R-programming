% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linreg.R
\docType{class}
\name{linreg}
\alias{linreg}
\title{Linear Regression Class}
\arguments{
\item{formula}{formula}

\item{data}{The data frame on which we want to do regression.@return nothing; Create a class which calculate linear regression \code{Formula} and \code{data}.}
}
\description{
Calculate greatest common divisor
The class for linear regression
}
\section{Fields}{

\describe{
\item{\code{data}}{The data frame on which we want to do regression.}

\item{\code{formula}}{The data frame on which we want to do regression.}

\item{\code{X}}{The Model matrix of independent variables.}

\item{\code{Y}}{the matrix of dependent variables}

\item{\code{k}}{number of rows in coefficient to calculate the df}

\item{\code{call}}{it return the call of the function}

\item{\code{coefficient}}{return the coefficient}

\item{\code{fitted.values}}{return the fitted value of Y}

\item{\code{transposeX}}{just the transpose of the x}

\item{\code{df}}{the Degree of freedom}

\item{\code{residual.values}}{returnt he value of residuals}

\item{\code{residualvariance}}{return the variance of residial}

\item{\code{coefvariance}}{return the variance of coefficient}

\item{\code{dependentVariable}}{return the column name of dependent variable from the formula}

\item{\code{qr_coef}}{return the coef using QR methonf}

\item{\code{qr_coefvariance}}{returnt he varinace of QR coefficient

Some length description maybe it works}
}}

\section{Methods}{

\describe{
\item{\code{coef()}}{calculating the fitted Value}

\item{\code{coef_matrix()}}{Calculuating the coefficent}

\item{\code{coef_variance(residual_variance)}}{calculate the coef variance}

\item{\code{decomposition()}}{calculating the QR from X}

\item{\code{df_function()}}{calculating the df}

\item{\code{fitted(coef)}}{calculating the fitted Value}

\item{\code{initialize(formula, data)}}{Initialize the class and run all the function to calculate the linear regrassion}

\item{\code{plot()}}{Generating the plot using GGPlot}

\item{\code{pred()}}{calculating the fitted Value}

\item{\code{print()}}{print function like lm print}

\item{\code{pt()}}{PT}

\item{\code{pValue()}}{pvalue}

\item{\code{qr_coef_variance()}}{Calcualte Variance}

\item{\code{qrcoef()}}{Calcualute Q-1 Rt X}

\item{\code{resid()}}{calculating the fitted Value}

\item{\code{residual(fit_matrix)}}{calculating the residual values}

\item{\code{residual_variance(residual_matrix)}}{calculating the residual variance}

\item{\code{stdError()}}{ERROR}

\item{\code{summary()}}{summary function like lm summary}

\item{\code{tValue()}}{calculate the tValue}
}}

\examples{

linregObject = linreg$new(Petal.Length~Species, data=iris)
linregObject$summary()

}
\references{
\url{https://en.wikipedia.org/wiki/Least_squares}

\url{https://en.wikipedia.org/wiki/QR_decomposition}
}
