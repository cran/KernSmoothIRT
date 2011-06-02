\name{Psych101}
\alias{Psych101}
\alias{Psychkey}
\alias{Psychresponses}
\docType{data}
\title{
Psych101 key and responses

}
\description{
Responses and key to Psych 101 exam. The key is \code{Psychkey}, the responses are \code{Psychresponses}. Key is a numeric vector with the correct responses for each of the 100 questions on the exam. 
Responses is a numeric matrix with each students answers to each question. Each column represents a question and each row represents a student.
There are 379 students who took the exam, thus, \code{Psychresponses} has 379 rows and 100 columns.
}
\usage{
data(Psych101)
Psychkey
Psychresponses
}
\details{

See ksIRT() help page for more information and examples on how to use these data with the \pkg{KernSmoothIRT} package.
}
\source{
Ramsay JO (1991). Kernel Smoothing Approaches to Nonparametric Item Characteristic
   Curve Estimation. Psychometrika, 56(4), 611 to 630.
}


\keyword{datasets}
