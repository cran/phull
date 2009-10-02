as.matrix.phull <- function(x, recycle=TRUE, ...)
{
	if (class(x) != "phull") stop("x is not of type phull.");

	ret <- rbind(x$bl2br, x$br2tr, x$tr2tl, x$tl2bl);

	if (recycle)
		ret <- rbind(ret, x$bl2br[1,]);

	return(ret);
}
