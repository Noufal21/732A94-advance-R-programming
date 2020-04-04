## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(Lab04R)
library(ggplot2)

## ------------------------------------------------------------------------
linregObject = linreg$new(Petal.Length~Species, data=iris)
linregObject$coef()

## ------------------------------------------------------------------------
linregObject = linreg$new(Petal.Length~Species, data=iris)
linregObject$qrcoef()

## ------------------------------------------------------------------------
linregObject = linreg$new(Petal.Length~Species, data=iris)
linregObject$print()

## ------------------------------------------------------------------------
linregObject = linreg$new(Petal.Length~Species, data=iris)
linregObject$summary()

## ------------------------------------------------------------------------
linregObject = lm(Petal.Length~Species, data=iris)
summary(linregObject)

## ------------------------------------------------------------------------
linregObject = linreg$new(Petal.Length~Species, data=iris)
linregObject$plot()

