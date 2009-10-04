draw <- function(x, ...)
{
	UseMethod("draw");
}


fun.e <- function(x, r1, r2, p)
{
	y <- ((r2^p-(r2/r1*x)^p)^(1.0/p)); #numericaly unstable!
# 	y[x>=r1] <- 0.0;
# 	y[x<=0.0] <- r2;
	return (y);
}


# generate a discrete approximation of the p-hull
# (sample the p-hull)
phull_part_generate <- function (Q, p, npoints)
{
	n <- nrow(Q);

	ret <- NA;

	for (i in 1L:(n-1))
	{
		pt1 <- Q[i,];
		pt2 <- Q[i+1,];

		if (p == 0)
		{
			retpart <- cbind(c(pt1[1], pt1[1], pt2[1]),
			                 c(pt1[2], pt2[2], pt2[2]));
		} else if (is.finite(p))
		{
			rcnum <- (pt1[1]^p)*(pt2[2]^p)-(pt2[1]^p)*(pt1[2]^p);
			rc1   <- ( rcnum / (pt2[2]^p - pt1[2]^p))^(1/p);
			rc2   <- (-rcnum / (pt2[1]^p - pt1[1]^p))^(1/p);

			x <- seq(pt1[1], pt2[1], length=npoints);
			y <- fun.e(x, rc1, rc2, p);

			y[y<pt2[2]] <- pt2[2]; # BUGFIX: there were round-off errors for large p!

			retpart <- cbind(x,y);
		} else
		{
			retpart <- cbind(c(pt1[1], pt2[1], pt2[1]),
			                 c(pt1[2], pt1[2], pt2[2]));
		}

		if (!is.matrix(ret)) ret <- retpart
		else                 ret <- rbind(ret, retpart);
	}

	return(ret);
}



discretize.phull <- function(x, npoints)
{
	Trans.bl2br <- matRotateOrt(   0)%*%matTranslate(-x$xrange[1], -x$yrange[1]);
	Trans.br2tr <- matRotateOrt( -90)%*%matTranslate(-x$xrange[2], -x$yrange[1]);
	Trans.tr2tl <- matRotateOrt(-180)%*%matTranslate(-x$xrange[2], -x$yrange[2]);
	Trans.tl2bl <- matRotateOrt(-270)%*%matTranslate(-x$xrange[1], -x$yrange[2]);

	TransInv.bl2br <- solve(Trans.bl2br);
	TransInv.br2tr <- solve(Trans.br2tr);
	TransInv.tr2tl <- solve(Trans.tr2tl);
	TransInv.tl2bl <- solve(Trans.tl2bl);

	Q.bl2br <- vecTransform(x$bl2br, Trans.bl2br);
	Q.br2tr <- vecTransform(x$br2tr, Trans.br2tr);
	Q.tr2tl <- vecTransform(x$tr2tl, Trans.tr2tl);
	Q.tl2bl <- vecTransform(x$tl2bl, Trans.tl2bl);

	bl2br <- vecTransform(phull_part_generate(Q.bl2br, x$p, npoints), TransInv.bl2br);
	br2tr <- vecTransform(phull_part_generate(Q.br2tr, x$p, npoints), TransInv.br2tr);
	tr2tl <- vecTransform(phull_part_generate(Q.tr2tl, x$p, npoints), TransInv.tr2tl);
	tl2bl <- vecTransform(phull_part_generate(Q.tl2bl, x$p, npoints), TransInv.tl2bl);

	return(list(bl2br=bl2br, br2tr=br2tr, tr2tl=tr2tl, tl2bl=tl2bl));
}



draw.phull <- function(x, fivecolors=FALSE, npoints=100, ...)
{
	d <- discretize.phull(x, npoints);

	if (fivecolors)
	{
		lines(d$bl2br, col=2, ...);
		lines(d$br2tr, col=3, ...);
		lines(d$tr2tl, col=4, ...);
		lines(d$tl2bl, col=5, ...);
	}
	else
	{
		lines(d$bl2br, ...);
		lines(d$br2tr, ...);
		lines(d$tr2tl, ...);
		lines(d$tl2bl, ...);
	}

	lines(rbind(d$bl2br[nrow(d$bl2br),], d$br2tr[1,]), ...);
	lines(rbind(d$br2tr[nrow(d$br2tr),], d$tr2tl[1,]), ...);
	lines(rbind(d$tr2tl[nrow(d$tr2tl),], d$tl2bl[1,]), ...);
	lines(rbind(d$tl2bl[nrow(d$tl2bl),], d$bl2br[1,]), ...);

	# .....................
}

