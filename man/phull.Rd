\name{phull}
\alias{phull}

\title{p-hull calculation}
\description{The function determines the p-hull of a finite planar set.}


\usage{phull(x, y=NULL, p=1, p_correction=TRUE)}


\arguments{
  \item{x,y}{coordinate vectors of points to join.}
  \item{p}{\eqn{0\le p\le\infty}.}
	\item{p_correction}{logical; if \code{TRUE} then \eqn{p>100} or \eqn{<0.1} is changed to \code{Inf} or \code{0}, respectively.}
}

\details{
The coordinates can be passed in two equal-length vectors, a two-column matrix, or a list.

The coordinates may not contain \code{NA} values.

The p-hull is calculated using the \eqn{O(n \log n)}-time algorithm
(Gagolewski et al, 2009)
which generalizes the well-known Graham's scan (Graham, 1972).
}

\value{
 The function returns an object of class \code{phull}.
 It is a list containing the following components:
  \item{n}{the number of input points.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{xrange}{a vector of 2 elements; range of \eqn{x} coordinates.}
  \item{yrange}{a vector of 2 elements; range of \eqn{y} coordinates.}
  \item{bl2br,br2tr,tr2tl,tl2bl}{internal; definition of p-hull.}
}

\references{
	M. Gagolewski, M. Nowakiewicz, M. Debski (2009).
	\emph{Efficient algorithms for computing ``geometric'' scientific impact indices}.
	Submitted for publication.

	R. L. Graham (1972).
	An efficient algorithm for determining the convex hull of a finite planar set.
	\emph{Information Processing Letters} 1, 132--133.

}

\examples{
x <- rnorm(1000); y <- rnorm(1000); # generate some points
phull_0.5 <- phull(x, y, 0.5); # calculate the p-hull
phull_1.0 <- phull(x, y, 1.0);
phull_2.0 <- phull(x, y, 2.0);
phull_5.0 <- phull(x, y, 5.0);

plot(x, y, type="p", pch='*'); # plot the points
draw(phull_0.5, col=2); # draw the first p-hull
draw(phull_1.0, col=3);
draw(phull_2.0, col=4);
draw(phull_5.0, col=5);
legend("topleft", c("p=0.5", "p=1.0", "p=2.0", "p=5.0"), col=2:5, lty=1)

plot(x, y, type="p", pch='*'); # plot the points
lines(phull_5.0);       # add p-lines
}
\keyword{nonparametric}
