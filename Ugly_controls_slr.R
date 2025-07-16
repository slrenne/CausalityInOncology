library(viridis)
set.seed(250716)
n <- 1e3

# Generate data
x <- rnorm(n)
z <- rnorm(n)
y <- rnorm(n, mean = x + z)
m1 <- lm(y ~ x)
m2 <- lm(y ~ x + z)

# Map z to color using viridis
col_vals <- viridis(n)[rank(z)]

png(file = "figures/3.6.1simulation.png",width = 2000, height = 2000, res=300)
# Plot

plot(x, y, 
     col = col_vals, 
     main = "The craftsman", 
     pch = 16 , xaxt = "n", yaxt="n",
     xlab = "EtOH", ylab = "Chirrosis")
abline(m1,lwd=3,lty=2,col=3)
abline(m2$coefficients[1:2],lwd=3,lty=3,col=4)
legend("topleft", legend = c("Model 1", "Model 2"),
       col = c(3, 4), lty = 1, bty = "n")
dev.off()

# Extract CIs for x only
ci1 <- confint(m1)[2, ]  # x in m1
ci2 <- confint(m2)[2, ]  # x in m2

# Combine into matrix
ci_mat <- rbind(ci1, ci2)
point_est <- c(coef(m1)["x"], coef(m2)["x"])

png(file = "figures/3.6.1modelfit.png",width = 2000, height = 2000, res=300)
# Plot setup
par(mar = c(5, 8, 4, 2))  # bottom, left, top, right
plot(NULL, xlim = range(ci_mat), ylim = c(0.5, 2.5),
     xlab = "Estimate and 95% CI", ylab = "", yaxt = "n",
     main = "Model Fit")

# Horizontal line at 0 (null effect)
abline(v = 1, lty = 2, col = "gray")

# Add CI lines
segments(ci_mat[1, 1], 2, ci_mat[1, 2], 2, lwd = 2, col = 3)
segments(ci_mat[2, 1], 1, ci_mat[2, 2], 1, lwd = 2, col = 4)

# Add point estimates
points(point_est[1], 2, pch = 16, col = 3, cex = 1.2)
points(point_est[2], 1, pch = 16, col = 4, cex = 1.2)

# Y-axis labels
axis(2, at = c(2, 1), labels = c("Model 1: y ~ x", "Model 2: y ~ x + z"), las = 1)

# Optional legend
legend("topright", legend = c("Model 1", "Model 2","True value"),
       col = c(3, 4, "gray"), lty = c(1,1,2), pch = c(16,16,26), bty = "n")
dev.off()

#####################################
#                                   #
#       3.6.2  The parasite         #
#                                   #
#####################################


# Generate data
z <- rnorm(n)
x <- rnorm(n, mean = z)
y <- rnorm(n, mean = x)
m1 <- lm(y ~ x)
m2 <- lm(y ~ x + z)

# Map z to color using viridis
col_vals <- viridis(n)[rank(z)]

png(file = "figures/3.6.2simulation.png",width = 2000, height = 2000, res=300)
# Plot

plot(x, y, 
     col = col_vals, 
     main = "The Parasite", 
     pch = 16 , xaxt = "n", yaxt="n",
     xlab = "", ylab = "")
abline(m1,lwd=3,lty=2,col=3)
abline(m2$coefficients[1:2],lwd=3,lty=3,col=4)
legend("topleft", legend = c("Model 1", "Model 2"),
       col = c(3, 4), lty = 1, bty = "n")
dev.off()

# Extract CIs for x only
ci1 <- confint(m1)[2, ]  # x in m1
ci2 <- confint(m2)[2, ]  # x in m2

# Combine into matrix
ci_mat <- rbind(ci1, ci2)
point_est <- c(coef(m1)["x"], coef(m2)["x"])

png(file = "figures/3.6.2modelfit.png",width = 2000, height = 2000, res=300)
# Plot setup
par(mar = c(5, 8, 4, 2))  # bottom, left, top, right
plot(NULL, xlim = range(ci_mat), ylim = c(0.5, 2.5),
     xlab = "Estimate and 95% CI", ylab = "", yaxt = "n",
     main = "Model Fit")

# Horizontal line at 0 (null effect)
abline(v = 1, lty = 2, col = "gray")

# Add CI lines
segments(ci_mat[1, 1], 2, ci_mat[1, 2], 2, lwd = 2, col = 3)
segments(ci_mat[2, 1], 1, ci_mat[2, 2], 1, lwd = 2, col = 4)

# Add point estimates
points(point_est[1], 2, pch = 16, col = 3, cex = 1.2)
points(point_est[2], 1, pch = 16, col = 4, cex = 1.2)

# Y-axis labels
axis(2, at = c(2, 1), labels = c("Model 1: y ~ x", "Model 2: y ~ x + z"), las = 1)

# Optional legend
legend("topright", legend = c("Model 1", "Model 2","True value"),
       col = c(3, 4, "gray"), lty = c(1,1,2), pch = c(16,16,26), bty = "n")
dev.off()



