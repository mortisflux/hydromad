\name{plotPCNSE}
\alias{plotPCNSE}
\title{
  Annotated parallel coordinates plot of model performance across periods
}
\description{
  Plot performance of model realisations, identifying non-dominated models
}
\usage{
plotPCNSE(res, objectives = "r.squared", return.data = FALSE)
}
\arguments{
    \item{res}{
    data.frame of results, including the column \code{sim.period} and
    the columns named in \code{objectives}. At least one of the following column names
    should be included as id variables:
    \code{Model.str},\code{Catchment},\code{calib.period},,\code{Cal.objfn}
    Other columns will be ignored.
  }
  \item{objectives}{
        Vector of column names containing performance measures. We assume
    higher values are better. Values should be transformed prior to use.
}
  \item{return.data}{
    return data.frame used in call to \code{ggplot}. Facilitates custom plotting.
}
}
\value{
  \code{\link{ggplot}} object, which can be plotted. Or data.frame if
  \code{return.data} is \code{TRUE}.
}
\author{
Joseph Guillaume
}
\seealso{
  \code{\link{areModelsDominated}} for raw data,
  \code{\link{paretoCatchments}} for further analysis
}
\examples{
data(YeAl97)
plotPCNSE(subset(YeAl97,Catchment=="Salmon"),objectives="E")
}
\keyword{ models }
