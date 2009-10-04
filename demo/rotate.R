translateAndRotate <- function(data, x0, y0, angle)
{
	data[,1] <- data[,1]+x0;
	data[,2] <- data[,2]+y0; # translation

	cost <- cos(angle);
	sint <- sin(angle);

	rot <- matrix(nrow=2, ncol=2, byrow=T, c(
		cost, -sint,
		sint, cost
	));
	data <- t(rot %*% t(data)) # rotation

	return(data);
}


rotateAndTranslate <- function(data, x0, y0, angle)
{
	cost <- cos(angle);
	sint <- sin(angle);

	rot <- matrix(nrow=2, ncol=2, byrow=T, c(
		cost, -sint,
		sint, cost
	));
	data <- t(rot %*% t(data)) # rotation

	data[,1] <- data[,1]+x0;
	data[,2] <- data[,2]+y0; # translation

	return(data);
}




# hint: try to decrease the number of degrees of freedom for the t distribution
data <- matrix(c(rnorm(1000), rt(1000, 15)), ncol=2);
p <- 5.0;
nres <- 50;

ptest <- phull(data, p=p);
discr_0 <- as.matrix(ptest, nres=nres);

data2 <- translateAndRotate(data, -ptest$xrange[1], -ptest$yrange[1], -pi/6);
ptest2 <- phull(data2, p=p);
discr_30 <- as.matrix(ptest2, nres=nres);
discr_30 <- rotateAndTranslate(discr_30, ptest$xrange[1], ptest$yrange[1], pi/6);


data2 <- translateAndRotate(data, -ptest$xrange[1], -ptest$yrange[1], -pi/3);
ptest2 <- phull(data2, p=p);
discr_60 <- as.matrix(ptest2, nres=nres);
discr_60 <- rotateAndTranslate(discr_60, ptest$xrange[1], ptest$yrange[1], pi/3);



plot(data, type="p", pch=1);

lines(discr_0,  col=2);
lines(discr_30, col=3);
lines(discr_60, col=4);


legend("topleft", c("Rot(0 deg)", "Rot(30 deg)", "Rot(60 deg)"), col=2:4, lty=1);

