library(viridis)
set.seed(250716)
n <- 1e3


# Functions
inv_logit <- function (x) {
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}

rbern <- function (n, prob = 0.5) {
  rbinom(n, size = 1, prob = prob)
}

pl_data_sim <- function(x, y, z, 
                        xlab = "X", 
                        ylab = "Y", 
                        zlow = "Low Z", 
                        zhigh = "High Z",
                        save = TRUE, 
                        filename = "") {
  n <- length(x)  # determine number of points
  
  # Fit models
  m1 <- lm(y ~ x)
  m2 <- lm(y ~ x + z)
  
  # Create colors based on z
  col_vals <- viridis(n, alpha = 0.5)[rank(z)]
  
  # Labels for legend
  lab <- c(expression(y %~% x), expression(y %~% x + z))
  
  # Open file device if saving
  if (save) {
    filename <- paste0(filename,"_")
    png(filename = paste0("figures/",filename,"sim.png"), width = 2000, height = 2000, res = 300)
  }
  
  # Plot
  plot(x = x, y = y, 
       col = col_vals,
       main = "Data Simulation",
       pch = 16, cex = 1 + inv_logit(z) *.7 ,
       xaxt = "n", yaxt = "n",
       xlab = xlab, ylab = ylab)
  
  abline(m1, lwd = 3, lty = 2, col = 3)
  abline(m2$coefficients[1:2], lwd = 3, lty = 3, col = 4)
  
  legend("topleft", legend = lab,
         col = c(3, 4), lty = c(2, 3), lwd = 3, bty = "n")
  legend("bottomright", legend = c(zlow, zhigh),
         fill = viridis(2, alpha = 0.5), bty = "n")
  
  # Close device if saving
  if (save) dev.off()
}

pl_ci_comp <- function(x, y, z,
                       save = TRUE, 
                       filename = "") {
  # Fit models
  m1 <- lm(y ~ x)
  m2 <- lm(y ~ x + z)
  
  # Confidence intervals for x
  ci1 <- confint(m1)[2, ]
  ci2 <- confint(m2)[2, ]
  
  # Combine into matrix
  ci_mat <- rbind(ci1, ci2)
  point_est <- c(coef(m1)["x"], coef(m2)["x"])
  
  
  # Labels for legend
  lab <- c(expression(y %~% x), expression(y %~% x + z))
  
  # Open file device if saving
  if (save) {
    filename <- paste0(filename,"_")
    png(filename = paste0("figures/",filename,"comp.png"), width = 2000, height = 2000, res = 300)
  }
  
  # Create empty plot
  plot(NULL, xlim = range(c(ci_mat,1)), ylim = c(0.5, 2.5),
       xlab =  "Effect Estimate and 95% CI", 
       ylab = "", yaxt = "n",
       main = "Comparison of the Models")
  
  # Reference line for true value
  abline(v = 1, lty = 2, col = "gray")
  
  # Plot CIs and point estimates
  for(i in 1:2) {
    segments(ci_mat[i, 1], i, ci_mat[i, 2], i, lwd = 4, col = 2 + i)
    points(point_est[i], i, pch = 16, col = 2 + i, cex = 2)
    text(point_est[i], i + 0.2, labels = lab[i])
  }
  
  legend("bottomright", legend = "True Effect",
         col = "gray", lty = 2, bty = "n")
  
  # Close device if saving
  if (save) dev.off()
}



##############################################
#                                            #
#     3.6.1 The Good Colliding Craftsman     #
#                                            #
##############################################

ugly <- "3.6.1_coll_craf"
x <- rnorm(n)
z <- rnorm(n)
y <- rnorm(n, mean = x + z)

pl_data_sim(x,y,z, filename = ugly)
pl_ci_comp(x,y,z, filename = ugly)

##############################################
#                                            #
#     3.6.2 The Good Forking Craftsman       #
#                                            #
##############################################

ugly <- "3.6.2_for_craf"
x <- rnorm(n)
z <- rnorm(n, mean = x)
y <- rnorm(n, mean = x)

pl_data_sim(x,y,z, filename = ugly)
pl_ci_comp(x,y,z, filename = ugly)


##############################################
#                                            #
#     3.6.3 The Good Piping  Craftsman       #
#                                            #
##############################################

ugly <- "3.6.3_pip_craf"
x <- rnorm(n)
z <- rnorm(n)
u <- rnorm(n, mean = x + z)
y <- rnorm(n, mean = u)

pl_data_sim(x,y,z, filename = ugly)
pl_ci_comp(x,y,z, filename = ugly)


##############################################
#                                            #
#     3.6.4 The Bad Piping Parasite          #
#                                            #
##############################################

ugly <- "3.6.4_pip_par"
z <- rnorm(n)
x <- rnorm(n, mean = z)
y <- rnorm(n, mean = x)

pl_data_sim(x,y,z, filename = ugly)
pl_ci_comp(x,y,z, filename = ugly)


##############################################
#                                            #
#     3.6.5 The Biased Piping Parasite       #
#                                            #
##############################################

ugly <- "3.6.5_bia_par"
z <- rnorm(n)
u <- rnorm(n)
x <- rnorm(n, mean = z + u)
y <- rnorm(n, mean = x + u)

pl_data_sim(x,y,z, filename = ugly)
pl_ci_comp(x,y,z, filename = ugly)
