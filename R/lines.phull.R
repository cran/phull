lines.phull <- function(x, fivecolors=FALSE, ...)
{
	if (class(x) != "phull") stop("x is not of type phull.");

	if (fivecolors)
	{
		lines(x$bl2br, col=2, ...);
		lines(x$br2tr, col=3, ...);
		lines(x$tr2tl, col=4, ...);
		lines(x$tl2bl, col=5, ...);
		lines(rbind(x$bl2br[nrow(x$bl2br),], x$br2tr[1,]), ...);
		lines(rbind(x$br2tr[nrow(x$br2tr),], x$tr2tl[1,]), ...);
		lines(rbind(x$tr2tl[nrow(x$tr2tl),], x$tl2bl[1,]), ...);
		lines(rbind(x$tl2bl[nrow(x$tl2bl),], x$bl2br[1,]), ...);
	} else {
		lines(as.matrix(x, TRUE), ...);
	}
}
