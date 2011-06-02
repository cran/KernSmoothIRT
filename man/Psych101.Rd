\name{Psych101}
\alias{Psych101}
\alias{Psychkey}
\alias{Psychresponses}
\docType{data}
\title{
Psych101 key and responses

}
\description{
Responses and key to Psych 101 exam. The key is \code{Psychkey}, the responses are \code{Psychresponses}.\code{Psychkey} is a numeric vector with the correct responses for each of the 100 questions on the exam. \code{Psychresponses} is a numeric matrix with each students answers to each question. Each column represents a question and each row represents a student. The data came from a multiple-choice examination given to 379 students in an introductory course in psychology in the Christmas exam period of 1989 at McGill. The test itself consisted of 100 multiple-choice items, each having four response options. The data were collected as part of a number of studies conducted by Prof. David Zuroff.
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
Ramsay JO. TestGraf: A program for the graphical analysis of multiple choice test and questionnaire data." \url{http://www.psych.mcgill.ca/faculty/ramsay/ramsay.html}.

Ramsay JO, Abrahamowicz M (1989). Binomial
regression with monotone splines: A psychometric application." Journal of the American Statistical
Association, 84(408), 906-915.

}


\keyword{datasets}
