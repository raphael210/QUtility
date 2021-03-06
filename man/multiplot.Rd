% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pub01_utilityFuncs.R
\name{multiplot}
\alias{multiplot}
\title{multiplot}
\usage{
multiplot(..., plotlist = NULL, ncol = 1)
}
\arguments{
\item{...}{Plot objects}

\item{plotlist}{A list of plot objects}

\item{ncol}{Number of columns of plots}
}
\value{
a page of plots
}
\description{
put multiple graphs on one page.
}
\details{
It can take any number of plot objects as arguments, or if it can take a list of plot objects passed to plotlist. First, set up the plots and store them, but don't render them yet. The details of these plots aren't important; all you need to do is store the plot objects in variables.Once the plot objects are set up, we can render them with \code{multiplot}. See details in \code{http://wiki.stdout.org/rcookbook/Graphs/Multiple}
}
\examples{
p1 <- qplot(y=1:4)
p2 <- qplot(1:5,10:14)
p3 <- qplot(4:30)
p4 <- qplot(3:9,4:10)
multiplot(p1, p2, p3, p4, ncol=2)
}
\author{
Ruifei.Yin
}
