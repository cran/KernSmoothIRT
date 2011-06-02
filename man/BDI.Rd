\name{BDI}
\alias{BDI}
\alias{BDIkey}
\alias{BDIresponses}
\docType{data}
\title{
Beck's Depression Inventory key and responses
}
\description{
Responses and key to the famous psychological exam \emph{Beck's Depression Inventory}. The key is \code{BDIkey}, the responses are \code{BDIresponses}. 
This exam contains ordinal responses, to be consistent with ksIRT() syntax, key is a numeric vector with the highest level ordinal response for each of the 21 questions on the exam. 
Responses is a numeric matrix with each examinee's response to each question. Each column represents a question and each row represents a subject.
There are 242 subjects who took the test, thus, \code{BDIresponses} has 242 rows and 21 columns.
}
\usage{
data(BDI)
BDIkey
BDIresponses
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
See ksIRT() help page for more information and examples on how to use these data with the \pkg{KernSmoothIRT} package.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Ramsay JO (1991). Kernel Smoothing Approaches to Nonparametric Item Characteristic
   Curve Estimation. Psychometrika, 56(4), 611 to 630.
}

\keyword{datasets}
