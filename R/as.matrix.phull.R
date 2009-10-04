as.matrix.phull <- function(x, recycle=TRUE, nres=0, ...)
{
	if (class(x) != "phull") stop("x is not of type phull.");

	if (nres <= 0)
	{
		ret <- rbind(x$bl2br, x$br2tr, x$tr2tl, x$tl2bl);

		if (recycle)
			ret <- rbind(ret, x$bl2br[1,]);
	} else
	{
		d <- discretize.phull(x, nres);

		ret <- rbind(d$bl2br, d$br2tr, d$tr2tl, d$tl2bl);

		if (recycle)
			ret <- rbind(ret, d$bl2br[1,]);
	}

	return(ret);
}
