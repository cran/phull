x <- rnorm(1000); y <- rnorm(1000);
phull_0.5 <- phull(x, y, 0.5);
phull_1.0 <- phull(x, y, 1.0);
phull_2.0 <- phull(x, y, 2.0);
phull_5.0 <- phull(x, y, 5.0);

plot(x, y, type="p", pch='*');
draw(phull_0.5, col=2);
draw(phull_1.0, col=3);
draw(phull_2.0, col=4);
draw(phull_5.0, col=5);
legend("topleft", c("p=0.5", "p=1.0", "p=2.0", "p=5.0"), col=2:5, lty=1)

