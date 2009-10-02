points.phull <- function(x, fivecolors=FALSE, ...)
{
	if (class(x) != "phull") stop("x is not of type phull.");

	if (fivecolors)
	{
		points(x$bl2br, col=2, ...);
		points(x$br2tr, col=3, ...);
		points(x$tr2tl, col=4, ...);
		points(x$tl2bl, col=5, ...);
		points(rbind(x$bl2br[nrow(x$bl2br),], x$br2tr[1,]), ...);
		points(rbind(x$br2tr[nrow(x$br2tr),], x$tr2tl[1,]), ...);
		points(rbind(x$tr2tl[nrow(x$tr2tl),], x$tl2bl[1,]), ...);
		points(rbind(x$tl2bl[nrow(x$tl2bl),], x$bl2br[1,]), ...);
	} else {
		points(as.matrix(x, FALSE), ...);
	}
}
