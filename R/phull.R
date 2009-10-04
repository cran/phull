matTranslate <- function(x0, y0)
{
	return(matrix(nrow=3, ncol=3, byrow=T, c(
		1, 0, x0,
		0, 1, y0,
		0, 0, 1
	)));
}


matRotateOrt <- function(thetaDeg)
{
	if (thetaDeg == 0)
	{
		return(matrix(nrow=3, ncol=3, byrow=T, c(
				1, 0, 0,
				0, 1, 0,
				0, 0, 1
			)));
	} else if (thetaDeg == -90)
	{
		return(matrix(nrow=3, ncol=3, byrow=T, c(
				0,  1, 0,
				-1, 0, 0,
				0,  0, 1
			)));
	} else if (thetaDeg == -180)
	{
		return(matrix(nrow=3, ncol=3, byrow=T, c(
				-1, 0, 0,
				0, -1, 0,
				0,  0, 1
			)));
	} else if (thetaDeg == -270)
	{
		return(matrix(nrow=3, ncol=3, byrow=T, c(
				0, -1, 0,
				1,  0, 0,
				0,  0, 1
			)));
	}
	else return(NA);
}


vecTransform <- function(v, T)
{
	v2 <- t(T%*%t(cbind(v,1)));
	return(v2[,1:2]/v2[,3]);
}


phull <- function(x, y=NULL, p=1, p_correction=TRUE)
{
	if (is.null(y)) {
		if (is.matrix(x)) {
			if (ncol(x) != 2) stop("x must have 2 columns.");
			y <- x[,2];
			x <- x[,1];
		} else if (is.list(x)) {
			if (length(x) != 2) stop("x must have 2 elements.");
			y <- x[[2]];
			x <- x[[1]];
		} else stop("Incorrect input type.");

		DNAME <- deparse(substitute(x));
	}
	else
		DNAME <- paste(deparse(substitute(x)), deparse(substitute(y)), sep=", ");

	if (length(x) < 3) stop("The number of input points is too small.");
	if (length(x) != length(y)) stop("x and y must be of the same length.");
	if (!is.numeric(x) || !is.numeric(y)) stop("Incorrect input type.");
	if (!all(is.finite(x)) || !all(is.finite(y))) stop("The coordinates must be finite.");

	if (p < 0 || length(p) != 1) stop("p should be a number not less than 0.");
	if (p_correction && p > 100)  p <- Inf;
	if (p_correction && p < 0.01) p <- 0;


	P8 <- phull_find8points(x, y);

	xmin <- P8[1,1]; xmax <- P8[5,1];
	ymin <- P8[2,2]; ymax <- P8[6,2];


	if (p == 0)
	{
		# p=0 => bounding rectangle => the result is available immediately
		bl2br <- matrix(c(P8[1,], xmin, ymin, P8[2,]), ncol=2, byrow=T);
		br2tr <- matrix(c(P8[3,], xmax, ymin, P8[4,]), ncol=2, byrow=T);
		tr2tl <- matrix(c(P8[5,], xmax, ymax, P8[6,]), ncol=2, byrow=T);
		tl2bl <- matrix(c(P8[7,], xmin, ymax, P8[8,]), ncol=2, byrow=T);
	}
	else
	{
		# NOTE: the following lines should be rewritten in C
		sub.bl2br <- ((x <= P8[2,1]) & (y <= P8[1,2]));
		sub.br2tr <- ((x >= P8[3,1]) & (y <= P8[4,2]));
		sub.tr2tl <- ((x >= P8[6,1]) & (y >= P8[5,2]));
		sub.tl2bl <- ((x <= P8[7,1]) & (y >= P8[8,2]));

		Trans.bl2br <- matRotateOrt(   0)%*%matTranslate(-xmin, -ymin);
		Trans.br2tr <- matRotateOrt( -90)%*%matTranslate(-xmax, -ymin);
		Trans.tr2tl <- matRotateOrt(-180)%*%matTranslate(-xmax, -ymax);
		Trans.tl2bl <- matRotateOrt(-270)%*%matTranslate(-xmin, -ymax);

		TransInv.bl2br <- solve(Trans.bl2br);
		TransInv.br2tr <- solve(Trans.br2tr);
		TransInv.tr2tl <- solve(Trans.tr2tl);
		TransInv.tl2bl <- solve(Trans.tl2bl);

		Q.bl2br <- vecTransform(matrix(c(x[sub.bl2br], y[sub.bl2br]), ncol=2), Trans.bl2br);
		Q.br2tr <- vecTransform(matrix(c(x[sub.br2tr], y[sub.br2tr]), ncol=2), Trans.br2tr);
		Q.tr2tl <- vecTransform(matrix(c(x[sub.tr2tl], y[sub.tr2tl]), ncol=2), Trans.tr2tl);
		Q.tl2bl <- vecTransform(matrix(c(x[sub.tl2bl], y[sub.tl2bl]), ncol=2), Trans.tl2bl);

		bl2br <- vecTransform(phull_part_computewithr(Q.bl2br, p), TransInv.bl2br);
		br2tr <- vecTransform(phull_part_computewithr(Q.br2tr, p), TransInv.br2tr);
		tr2tl <- vecTransform(phull_part_computewithr(Q.tr2tl, p), TransInv.tr2tl);
		tl2bl <- vecTransform(phull_part_computewithr(Q.tl2bl, p), TransInv.tl2bl);
	}


	ret <- list(p=p,
		n=length(x),
		data.name=DNAME,
		xrange=c(xmin, xmax),
		yrange=c(ymin, ymax),
		bl2br=bl2br,
		br2tr=br2tr,
		tr2tl=tr2tl,
		tl2bl=tl2bl);

	class(ret) <- "phull";
	return(ret);
}

