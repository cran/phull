# Find the 8 points on the bounding rectangle
# NOTE: this routine should be rewritten in C
phull_find8points <- function(x, y)
{
	n <- length(x);
	i <- 2L;

	xmin <- x[1];
	ymin <- y[1];
	xmax <- x[1];
	ymax <- y[1];

	X8 <- rep(x[1], 8);
	Y8 <- rep(y[1], 8);

	while (i <= n)
	{
		if (x[i] < xmin)
		{
			xmin <- x[i];
			X8[c(1,8)] <- x[i];
			Y8[c(1,8)] <- y[i];
		} else if (x[i] == xmin)
		{
			if      (y[i] < Y8[1]) Y8[1] <- y[i]
			else if (y[i] > Y8[8]) Y8[8] <- y[i];
		} else if (x[i] > xmax)
		{
			xmax <- x[i];
			X8[c(4,5)] <- x[i];
			Y8[c(4,5)] <- y[i];
		} else if (x[i] == xmax)
		{
			if      (y[i] < Y8[4]) Y8[4] <- y[i]
			else if (y[i] > Y8[5]) Y8[5] <- y[i];
		}


		if (y[i] < ymin)
		{
			ymin <- y[i];
			X8[c(2,3)] <- x[i];
			Y8[c(2,3)] <- y[i];
		} else if (y[i] == ymin)
		{
			if      (x[i] < X8[2]) X8[2] <- x[i]
			else if (x[i] > X8[3]) X8[3] <- x[i];
		} else if (y[i] > ymax)
		{
			ymax <- y[i];
			X8[c(6,7)] <- x[i];
			Y8[c(6,7)] <- y[i];
		} else if (y[i] == ymax)
		{
			if      (x[i] < X8[7]) X8[7] <- x[i]
			else if (x[i] > X8[6]) X8[6] <- x[i];
		}


		i <- i + 1L;
	}

	return(matrix(c(X8,Y8), ncol=2));
}
