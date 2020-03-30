\name{disaggregate}
\alias{disaggregate}
\title{Disaggregate input-output systems
}
\description{The disaggregation function follows the methodology of Blair & Miller (2009). 

NOTE: The function only works with single region "national" \code{InputOutput} object. Consider \code{\link{agg.region}}}

\usage{
disaggregate(io, X = list(), U = list(), V = list(), new.regions,
             check = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{X}{An R length list of same-length new region total production matrix, where R is the number of new regions}
  \item{U}{An R length list of same-length new region row sums of the new intermediate transaction matrix, where R is the number of new regions}
  \item{V}{An R length list of same-length new region column sums of the new intermediate transaction matrix, where R is the number of new regions}
  \item{new.regions}{The names assigned to the new regions}
  \item{check}{Logical. Check if sector specific row sums, column sums, and total production of inputs are consistent with the original \code{InputOutput} object}

}
\details{Broadly speaking, a disaggregated \code{A} is created via \code{\link{lq}}, then is balanced using \code{\link{ras}}. See the online "Gettting started ioanalysis" manual available at real.illinois.edu for more details.
}
\value{
\item{A.new}{The new estimated technical coefficients matrix}
}
\references{
Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

}
\author{
John J. P. Wade
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lq}}, \code{\link{ras}}, \code{\link{agg.region}} \code{\link{as.inputoutput}}
}
\examples{
data(toy.IO)
X = list(toy.IO$X[1:5], toy.IO$X[6:10])
U = list(rowSums(toy.IO$Z[1:5, ]), rowSums(toy.IO$Z[6:10, ]))
V = list(colSums(toy.IO$Z[, 1:5]), colSums(toy.IO$Z[, 6:10]))

io = agg.region(toy.IO, regions = c(1,2), newname = 'Magic')

A.new = disaggregate(io, X, U, V, new.regions = unique(toy.IO$RS_label[,1]))
Z.new = A.new \%*\% diag(c(unlist(X)))

io.new = as.inputoutput(Z = Z.new, X = unlist(X), RS_label = toy.IO$RS_label)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
