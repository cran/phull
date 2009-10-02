x <- c(rnorm(500, 0), rnorm(500, 25)); y <- rnorm(1000, 0);
phull_0.0 <- phull(x, y, 0.0);
phull_1.0 <- phull(x, y, 1.0);
phull_Inf <- phull(x, y, Inf);

plot(x, y, type="p", pch='*');
draw(phull_0.0, col=2);
draw(phull_1.0, col=3);
draw(phull_Inf, col=4);
legend("topleft", c("p=0.0", "p=1.0", "p=Inf"), col=2:4, lty=1)

