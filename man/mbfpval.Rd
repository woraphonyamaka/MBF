\name{MBF}
\alias{MBF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MBF(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
This package introduces an alternative approaches for the p-value namely Minimum Bayes Factors (MBF), for find the evidence against a point null hypothesis under the linear regression context.  MBF is the one counterpart to p-value offered by the Bayesian approach, which relies solely on the observed sample to provide direct probability statements about the parameters of interest. This  approach lie in the same range as p-values, which facilitates us to make the comparison.the MBF is interpreted following the Goodman (1999) labelled intervals. As a result, a BF10 between 1-1/3 is considered weak evidence for H1 , form 1/3 – 1/10 considered moderate evidence for H1, 1/10 – 1/30 is considered substantial evidence, form 1/30 to 1/100 Strong, form 1/100 to 1/300 Very strong, and <1/300 Decisive.
}
\value{
\item{MBF }{Maximum Bayes Factor value}

}
\references{
Goodman, S. N. (1999). Toward evidence-based medical statistics. 1: The P value fallacy. Annals of internal medicine, 130(12), 995-1004.

Held, L., & Ott, M. (2016). How the maximal evidence of p-values against point null hypotheses depends on sample size. The American Statistician, 70(4), 335-341.

Stern, H. S. (2016). A test by any other name: P values, Bayes factors, and statistical inference. Multivariate behavioral research, 51(1), 23-29.
}
\author{
Woraphon Yamaka
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
t=2  # t-statistic
MBF(t,method="goodman")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
