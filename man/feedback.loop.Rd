\name{feedback.loop}
\alias{feedback.loop}

\title{Feedback Loop Analysis}

\description{Calculates the complete hierarchical feedback loop as described in Sonis et al. (1995). A feed back loop is complete if it contains all region-sector pairs. Much like a sudoku puzzle, there may only be one identified cell in each row and each column per loop. The loops are hierarchical in the sense that first loop maximizes the transactions given the aforementioned constraints.

Note: A feedback loop solves the Linear Programming Assignment problem.

Warning: Computation time depends on size of the system. A progress bar is printed.}

\usage{feedback.loop(io, agg.sectors, agg.regions)}

\arguments{
  \item{io}{An object of class \code{InputOutput} calculated from \code{\link{as.inputoutput}}}
  \item{agg.sectors}{An option to aggregate the sectors to compare regions only. Default is \code{FALSE}.}
  \item{agg.regions}{An option to aggregate the regions to compare sectors only. Default is \code{FALSE}.}
}
\details{The feedback loop solves the following optimization problem:

\deqn{max_X vec(Z)'X}
such that:
\deqn{i) A_{col}X = vec(1)}
\deqn{ii) A_{row}X = vec(1)}
\deqn{iii) vec(0) \le x \le vec(1)}

where \eqn{Z} is the intermediate transaction matrix from \code{io}, \eqn{X} is a vectorize selctor matrix of the cells in \eqn{Z}, \eqn{A_{col}} is a constraint matrix to ensure only one cell per column is selected, \eqn{A_{row}} is a constraint matrix to ensure only one cell per row is selected, and constraint \eqn{iii)} ensures the values in the selector matrix are either one or zero. 

After each loop, the selected cells are set to an extremely negative number to prevent selection in the next loop.

See the documentation on http://www.real.illinois.edu/ for more details and interpretation of the loops.
}

\value{
Produces a nested list: \code{fl}
\item{fl}{Contains \code{"loops"} and \code{"value"}}
\item{loops}{Contains a list over each loop's selector matrix. Retrieve by calling \code{fl$loops[[1]]}}
\item{value}{Contains a vector of the total value of intermediate transactions for each loop.}


}
\references{Sonis, M., Hewings, G. J., & Gazel, R (1995). The structure of multi-regional trade flows: hierarchy, feedbacks and spatial linkages. \emph{The Annals of Regional Science}, 29(4) 409-430.
}

\author{
John J. P. Wade
}


\seealso{ 
\code{\link{as.inputoutput}}
}
\examples{
data(toy.IO)
class(toy.IO)

fbl = feedback.loop(toy.IO)

fbl$loops[[1]]
heatmap.io(fbl$loops[[1]], RS_label = toy.IO$RS_label)

fbl$value
fbl$per = fbl$value / sum(fbl$value) * 100

obj = data.frame(x = 1:length(fbl$per), y = fbl$per)

ggplot(obj, aes(x = x, y = y)) + 
  geom_line() +
  labs(x = 'Loop', y = 'Percent', title = 'Proportion of Total Intermediate Transactions per Loop')

# Aggregating regions
fbl_2 = feedback.loop(toy.IO, agg.regions = TRUE)
io_2  = agg.region(toy.IO, regions = 'all', newname = 'magic')
heatmap.io(fbl_2$loops[[1]], RS_label = io_2$RS_label)

# Aggregating everything for exciting results
feedback.loop(toy.IO, agg.regions = TRUE, agg.sectors = TRUE)
}

