print.phull <- function(x, ...)
{
	if (class(x) != "phull") stop("x is not of type phull.");

	cat(sprintf("\tp-hull, p=%g\n\n", x$p));

	cat(sprintf("data: %s\n", x$data.name));
	cat(sprintf("%d points, bounding rectangle: [%g, %g]x[%g, %g]\n\n\n",
		x$n, x$xrange[1], x$xrange[2], x$yrange[1], x$yrange[2]));


	cat("Hull coordinates:\n\n");

	cat("bottom left -> bottom right:\n");
	cat("x: "); print(x$bl2br[,1]);
	cat("y: "); print(x$bl2br[,2]);
	cat("\n");

	cat("bottom right -> top right:\n");
	cat("x: "); print(x$br2tr[,1]);
	cat("y: "); print(x$br2tr[,2]);
	cat("\n");

	cat("top right -> top left:\n");
	cat("x: "); print(x$tr2tl[,1]);
	cat("y: "); print(x$tr2tl[,2]);
	cat("\n");

	cat("top left -> bottom left:\n");
	cat("x: "); print(x$tl2bl[,1]);
	cat("y: "); print(x$tl2bl[,2]);
	cat("\n");
}
