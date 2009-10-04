# NOTE: this routine should be rewritten in C
phull_part_computewithr <- function(Q, p)
{
	if (is.vector(Q))
	{
		warning("phull_part_computewithr: assumptions not met!");
		return(matrix(c(Q,Q), ncol=2, byrow=T));
	}

	n <- nrow(Q);

	P <- Q[order(Q[,1]),];

	P[P[,1]<0.0, 1] <- 0.0; # there are round-off errors!
	P[P[,2]<0.0, 2] <- 0.0;

	stack <- matrix(NA, nrow=n, ncol=2);

	stacksize <- 2L;
	stack[1,1] <- P[1,1];
	stack[1,2] <- P[1,2];

	j <- 2L;
	while ((j<=n) && P[j,2] >= P[1,2]) j<-j+1;

	stack[2,1] <- P[j,1];
	stack[2,2] <- P[j,2];

	i <- j+1L;
	while (i <= n)
	{
		x2 <- P[i,1];
		y2 <- P[i,2];
		x1 <- stack[stacksize,1];
		y1 <- stack[stacksize,2];

		if (y2 < y1)
		{
			if (x1 == x2) stack[stacksize,2] <- y2
			else
			{
				while (stacksize >= 2L)
				{
					if (is.finite(p))
					{
						rcnum <- (x1^p)*(y2^p)-(x2^p)*(y1^p);
						rc1   <- ( rcnum / (y2^p - y1^p))^(1.0/p);
						rc2   <- (-rcnum / (x2^p - x1^p))^(1.0/p);
						y0 <- fun.e(stack[stacksize-1L,1], rc1, rc2, p);
					} else {
						y0 <- y1;
					}

					if (y0 <= stack[stacksize-1L,2]) break;
					stacksize <- stacksize-1L; #pop
					x1 <- stack[stacksize,1];
					y1 <- stack[stacksize,2];
				}
				stacksize <- stacksize+1; #push
				stack[stacksize,1] <- x2;
				stack[stacksize,2] <- y2;
			}
		}

		i <- i+1L;
	}

	return(stack[1:stacksize,]);
}


