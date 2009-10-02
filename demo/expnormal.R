x <- rexp(1000); y <- rnorm(1000);
phull_0.0 <- phull(x, y, 0.0);
phull_0.2 <- phull(x, y, 0.2);
phull_1.0 <- phull(x, y, 1.0);
phull_5.0 <- phull(x, y, 5.0);
phull_Inf <- phull(x, y, Inf);

plot(x, y, type="p", pch='*');
draw(phull_0.0, col=2);
draw(phull_0.2, col=3);
draw(phull_1.0, col=4);
draw(phull_5.0, col=5);
draw(phull_Inf, col=6);
legend("topleft", c("p=0.0", "p=0.2", "p=1.0", "p=5.0", "p=Inf"), col=2:6, lty=1)

