\name{KernSmoothIRT-package}
\alias{KernSmoothIRT}
\docType{package}
\title{KernSmoothIRT Package}
\description{This package fits non-parametric item and option characteristic curves using kernel smoothing. It allows for optimal selection of the smoothing bandwidth using cross-validation and a variety of exploratory plotting tools.
}
\details{
\tabular{ll}{
Package: \tab KernSmoothIRT\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-05-29\cr
Requires: \tab plotrix \cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}

The function ksIRT creates an ksIRT object using kernel smoothing. Plotting the ksIRT object creates a variety of exploratory plots including: item characteristic curves, option characteristic curves, density plots, expected value plots, standard error plot, test and item information plots, and a probability simplex plot for the top 3 highest probability options of each item. The NIRT function will use a default kernel smoothing bandwidth, but the smooth option allows the function to select this bandwidth using an optimization routine that minimizes a cross-validation statistic.

}
\author{
Angelo Mazza, Antonio Punzo, Brian McGuire
Maintainer: Brian McGuire <mcguirebc@gmail.com>

}
\references{

Ramsay JO (1991). Kernel Smoothing Approaches to Nonparametric Item Characteristic
   Curve Estimation. Psychometrika, 56(4), 611 to 630.

}



