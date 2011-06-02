\name{ksIRT}
\alias{ksIRT}
\alias{plot.ksIRT}
\alias{print.ksIRT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ksIRT - kernel smoothing in Item Reponse Theory
}
\description{
This function fits nonparametric item and options characteristic curves using kernel smoothing techniques. It allows for a variety of exploratory plotting tools.
}
\usage{
ksIRT(responses, key = NULL, scale = NULL, kernel="gaussian", labs = NULL, weights = NULL, miss = "category", NAweight=0, theta = NULL, nval=51, bandwidth = "default", enumerate=list("norm",0,1), groups=FALSE)
\method{plot}{ksIRT}(x, plottype = "OCC", items="all", subjects, axistype = "distribution", alpha, main, xlab, ylab, xlim, ylim, cex, ...)
\method{print}{ksIRT}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{responses}{
Input data matrix with options selected from each individual for each item. Rows represent individuals, columns represent items. Missing values are inserted as \code{NA}.
}
  \item{key}{
%%     ~~Describe \code{key} here~~
An optional numeric vector. If provided it must be a numeric vector with the number of entries equal to the number of items. If the items are nominal (multiple choice), it should contain option for the correct response.

If the data are ordinal, key should contain the largest option value for each item. In this case, the weight assigned to each option is equal to its option number.

More complicated weighting schemes such as partial credit can be specified in the \code{weights} option. If \code{weights} is specified, \code{key} must be left blank. 

}
  \item{scale}{
Either a character string or a numeric vector specifying the type of items. If all of the items are nominal then \code{scale="nominal"}. If all the items are ordinal, \code{scale="ordinal"}. If the test has a mixture of nominal and ordinal items, then scale should be a vector with length equal to the number of items with entries of 1 for each nominal item and 0 for each ordinal item.


}
  \item{kernel}{
A character string specifying the smoothing kernel. Must be either \code{"gaussian"}, \code{"quadratic"} or \code{"uniform"}. The default is \code{"gaussian"}.
}
  \item{labs}{
Optional list of labels for each item. If omitted, each item will be labelled according to its numerical order. These labels will be used in plotting.
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
Optional list argument that may be used in lieu of including a key. Specifying weights allows for more complicated weighting schemes than all or nothing on nominal items, and weight equal to option value for ordinal items. Weight must be a list of length equal to the number of items. Each entry in the list must be a matrix with two rows. The first row should contain each option, and the second row should contain the weight given to each of the options in row 1.

If weights is omitted and the data are nominal, then a weight of 1 is given to the correct response from key.
If weights is omitted and the data are ordinal, then a weight equal to the option number is given to each response.
}
  \item{miss}{
%%     ~~Describe \code{miss} here~~
A character string specifying the method used to manage missing responses.

The default value, \code{miss="category"}, treats missing responses as an option value with zero weight. In this case, the missing values will be plotted as an additional option on the Option Characteristic Curves. You can provide an argument to \code{NAweight} if you would like missing observations to have non-zero weight.

\code{miss="random.unif"} substitutes another option value by sampling from the non-missing responses to the same item with each option equally probable of selection.

\code{miss="random.multinom"} substitutes another option value by sampling from the non-missing responses to the same item with each option's probability of selection equal to that option's relative frequency amongst the other responses.

\code{miss="omit"} omits missing observations from the analysis. In the computation of point polyserial correlation, pairwise deletion is used.
}
\item{NAweight}{
A scalar numeric weight given to missing responses. The default is zero.
}
  \item{theta}{
%%     ~~Describe \code{theta} here~~
An optional numeric vector of quantiles at which to estimate curves. The default is 51 evenly spaced points with end points determined by the number of subjects and the type of \code{enumerate} used.

}
  \item{nval}{
An optional scalar value that specifies the number of evenly spaced points at which curves are estimated. The end points are determined by the number of subjects and the type of \code{enumerate} used. This argument is an alternative to completely specifying the points at which to estimate curves with \code{theta}. If both \code{nval} and \code{theta} are specified, then \code{theta} will be used.
 
}
  \item{bandwidth}{
%%     ~~Describe \code{bandwidth} here~~
Either a character string or a numeric vector specifying the smoothing bandwidth to use for kernel smoothing. The default is: \code{1.06*sigma.hat*(nex)^(-.2)} for each item where \code{nex} is the number of subjects. \code{sigma.hat} is the standard deviation of the distribution specified with \code{enumerate} if a normal distribution is used. If the distribution used to enumerate the subjects is not normal, then \code{sigma.hat} is the standard deviation of the relative ranks on the quantile scale of the distribution that is specified. Alternatively, you may specify a numeric vector that contains a smoothing bandwidth for each item. 

If \code{bandwidth="CV"}, then the bandwidth is chosen for each item through cross-validation.
 }
\item{enumerate}{
A list specifying the distribution to be used to enumerate (in acceptation of Ramsay, 1991) the subjects. By default a standard normal distribution is used. You can adopt another distribution by specifying the first element of the list as \code{"norm"}, \code{"beta"}, \code{"unif"}, \code{"gamma"}, etc. where the character string is the same as used in the quantiles function \code{qnorm()}, \code{qbeta()}, \code{qunif()}, \code{qgamma()}. The other elements of the list should be the distribution parameters as required by the quantiles functions.
}

\item{groups}{
An optional vector of length equal to the number of subjects containing the group designation for each subject. Adding this option allows for comparisons within groups using the Differential Item Functioning tools (see details section).

}
\item{x}{A ksIRT created with ksIRT()}

\item{plottype}{the type of plot to be used (see details below). If no \code{plottype} is specified then 
option characteristic curves are plotted. See details section below for the types of plots that can be created.}

\item{items}{a vector containing the items to be plotted. If \code{items='all'}, all items are plotted.}

\item{alpha}{A numeric scalar indicating the confidence level to use in creating confidence intervals in the option and item characteristic curve plots. By default, the item characteristic plot uses \code{alpha=.05} and the option characteristic plot does not plot confidence intervals, but either can be changed by specifying an alpha level or confidence intervals may be omitted entirely with \code{alpha=FALSE}.}

\item{subjects}{A vector specifying the subjects to plot. This option is only used for credibility plots \code{plottype="credibility"}.}

\item{axistype}{A character string specifying the display variable to be used on the x-axis. The default is \code{axistype="distribution"}, which uses the quantiles of the distribution specified in \code{enumerate}. The alternaive is \code{axistype="scores"} which displays the expected score.}

\item{main}{Optional character string title. See plot().}

\item{xlab}{Optional character string x-axis label. See plot().}

\item{ylab}{Optional character string y-axis label. See plot().}

\item{xlim}{Optional numeric vector of x-axis limits. See plot().}

\item{ylim}{Optional numeric vector of y-axis limits. See plot().}

\item{cex}{Optional scalar used for point magnification. See par().}

\item{...}{other parameters passed to the plot function}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
Plotting the ksIRT object (\code{plot(ksIRT,...)}) produces a few different plots that are of interest. The different plots created from various \code{plottype} options are described below:
\describe{

\item{\code{plottype="density"}}{
Produces a simple kernel density plot of the observed scores
}

\item{\code{plottype="ICC"}}{
Plot of the item characteristic curves for each of the item numbers in the \code{items} option. By default, plots for every item.
}

\item{\code{plottype="OCC"}}{
Plot of the option characteristic curves for each of the item numbers in the \code{items} option. By default, plots for every item.
}

\item{\code{plottype="expected"}}{
Plot of the observed vs. expected scores.
}

\item{\code{plottype="sd"}}{
Plot of the standard deviation of observed scores.
}

\item{\code{plottype="info"}}{
Plots item information function for each of the item numbers in the vector items. 
}

\item{\code{plottype="testinfo"}}{
Plots item information function for the entire test. 
}

\item{\code{plottype="se"}}{
Plots the standard error of estimated scores.
}

\item{\code{plottype="reliability"}}{
Plots the reliability of the test at different trait levels.
}

\item{\code{plottype="credibility"}}{
Plots the credibility and actual score for each subject specified by the \code{subjects} option. By default, plots for each subject.
}

\item{\code{plottype="triangle"}}{
Produces a triangle simplex plot with the highest 3 probability options for each item specified by the \code{items} option. By default, plots for all items.
}

\item{\code{plottype="tetrahedron"}}{
Produces a tetrahedron simplex plot with the highest 4 probability options for each item specified by the \code{items} option. The tetrahedron plot can be rotated by using the mouse. By default, plots for all items.
}

\item{\code{plottype="PCA"}}{
Produces Principle Component Analysis plot of the test. One component represents difficulty while the other represents discrimination.
}

## Below are Differential Item Functioning (DIF) plots
## They are available only when the groups option is specified when creating the ksIRT object

\item{\code{plottype="densityDIF"}}{
Plots density of observed scores for each of the different groups specified with the \code{groups} used in creating the ksIRT object.
}

\item{\code{plottype="expectedDIF"}}{
Plots pairwise expected value comparison plots for each of the different groups specified with the \code{groups} used in creating the ksIRT object.
}

\item{\code{plottype="ICCDIF"}}{
Plots item characteristic curves for each of the different groups. Accepts the same arguments as \code{plottype="ICC"}, but by default does not show confidence intervals. This can be changed with the \code{alpha} option.
}

\item{\code{plottype="OCCDIF"}}{
Plots option characteristic curves for each of the different groups. Accepts the same arguments as \code{plottype="OCC"}
}

}
Printing the ksIRT object shows the point polyserial correlation correlation between each item and the overall test score.


}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

Returned from this function is a ksIRT object which is a list with the following components:

\item{binres}{
A matrix of binary responses. Each row corresponds to a single option. The first column specifies the item. The second specifies the option. The third is the weight. Each additional column is a binary indication of whether a subject selected that option.
}
\item{probs}{
A matrix with the first 3 columns the same as \code{binres}. There are \code{nval} additional columns with the smoothed probability of selecting each option.
}
\item{Stderrs}{
A matrix with the first 3 columns the same as \code{binres}. There are \code{nval} additional columns with the standard errors of selecting each option.
}

\item{scoresbysubject}{
A vector containing the observed score of each subject.
}

\item{itemlabels}{
A list containing the label for each item.
}

\item{theta}{
A vector of quantiles to use for curve estimation.
}

\item{quantiles}{
A vector score quantiles for plotting.
}

\item{quantilestheta}{
A vector of distribution quantiles for plotting.
}

\item{scoresattheta}{
A vector of scores used for plotting at each \code{theta} point.
}

\item{SmthWgts}{
A matrix of weights use in kernel smoothing.
}

\item{scale}{
A vector indicating whether each item is ordinal or nominal; 1 indicates nominal, 0 indicates ordinal.
}

\item{enumerate}{
A list indicating the distribution to rank subjects (see \code{enumerate} in Arguments).
}

\item{probrank}{
A vector of quantile ranks for each subject.
}

\item{band}{
A vector of bandwidths used for kernel smoothing each item.
}

\item{nitems}{
A scalar indicating the number of items.
}

\item{nex}{
A scalar indicating the number of subjects.
}

\item{nval}{
A scalar indicating the number of points for curve estimation.
}

\item{subsets}{
A list of ksIRT objects created for each of the subgroups specified by \code{groups}.
}

\item{groups}{
A vector indicating the different groups specified by \code{groups}.
}
\item{pserial}{
The point polyserial correlation for each item.
}
}



\examples{

data(Psych101)
#Psych1 <- ksIRT(responses=Psychresponses, key=Psychkey, scale="nominal",  bandwidth="CV")
Psych1 <- ksIRT(responses=Psychresponses, key=Psychkey, scale="nominal")

plot(Psych1, plottype="ICC")
plot(Psych1, plottype="OCC")
plot(Psych1, plottype="credibility", axistype="scores")
plot(Psych1, plottype="testinfo", axistype="scores")
plot(Psych1, plottype="PCA")
plot(Psych1, plottype="triangle", items=1:5)
plot(Psych1, plottype="tetrahedron", items=6:10)


data(BDI)
BDI1 <- ksIRT(responses=BDIresponses, key=BDIkey, scale="ordinal", groups=c(rep('Young',121), rep('Old',121)))

plot(BDI1, plottype="ICCDIF", cex=.8)
plot(BDI1, plottype="expectedDIF")


################### Aditional Examples ###############################

## Alternative kernels
Psych2 <- ksIRT(Psychresponses, key=Psychkey, kernel="quadratic", scale="nominal", nval=100)
Psych3 <- ksIRT(Psychresponses, key=Psychkey, kernel="uniform", scale="nominal")

## Alternative enumeration
Psych4 <- ksIRT(Psychresponses, key=Psychkey, scale="nominal", enumerate=list("beta",2.5,2.5))




## Overall expected value
plot(Psych1, plottype="expected")

## Overall standard deveviation
plot(Psych1, plottype="sd")

## Overall density
plot(Psych1, plottype="density")

## Item information
plot(Psych1, plottype="info")

## Reliability
plot(Psych1, plottype="reliability")

## Standard Error
plot(Psych1, plottype="se")

## Density (DIF)
plot(BDI1, plottype="densityDIF", ylim=c(0,0.1))


}

\references{
Ramsay JO (1991). Kernel Smoothing Approaches to Nonparametric Item Characteristic
   Curve Estimation. Psychometrika, 56(4), 611 to 630.
}




