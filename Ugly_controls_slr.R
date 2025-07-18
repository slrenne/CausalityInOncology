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
  z_uni <- length(unique(z))
  
  # Fit models
  m1 <- lm(y ~ x)
  m2 <- lm(y ~ x + z)
  
  # Create colors based on z
  col_vals <- viridis(z_uni, alpha = 0.5)
  col_ord <- as.numeric(factor(z))
  
  # Labels for legend
  lab <- c(expression(y %~% x), expression(y %~% x + z))
  
  # Open file device if saving
  if (save) {
    if (filename != "") filename <- paste0(filename,"_a_")
    png(filename = paste0("figures/",filename,"sim.png"), width = 2000, height = 2000, res = 300)
  }
  
  # Plot
  plot(x = x, y = y, col = 1,
       bg = col_vals[col_ord],
       main = "Data Simulation",
       pch = 21, 
       xaxt = "n", yaxt = "n",
       xlab = xlab, ylab = ylab)
  
  abline(m1, lwd = 3, lty = 2, col = 3)
  abline(m2$coefficients[1:2], lwd = 3, lty = 3, col = 4)
  
  legend("topleft", legend = lab,
         col = c(3, 4), lty = c(2, 3), lwd = 3)
  fill_val <- col_vals[c(1,z_uni)]
  legend("bottomright", legend = c(zlow, zhigh),
         fill = fill_val)
  
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
    if (filename != "") filename <- paste0(filename,"_b_")
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
         col = "gray", lty = 2)
  
  # Close device if saving
  if (save) dev.off()
}




##############################################
#                                            #
#     2.2.1 The Fork                         #
#                                            #
##############################################

ugly <- "2.2.1_fork"
z <- rbern(n, prob = 0.5 ) 
x <- rnorm(n, mean = 2 *z)
y <- rnorm(n, mean = 2 *z) 

pl_data_sim(x,y,z, filename = ugly, 
            xlab = "Cancer", ylab = "IHD", 
            zlow = "Non-smokers",
            zhigh = "Smokers")


##############################################
#                                            #
#     2.2.2 The Pipe                         #
#                                            #
##############################################

ugly <- "2.2.2_pipe"
x <- rnorm(n)
z <- rbern(n, prob = inv_logit(1.5 * x))
y <- rnorm(n, mean = - z) 

pl_data_sim(x,y,z, filename = ugly, 
            xlab = "Oncogenic Mutations", ylab = "Survival probability", 
            zlow = "Low-grade",
            zhigh = "High-Grade")



##############################################
#                                            #
#     2.2.3 The Collider                     #
#                                            #
##############################################

ugly <- "2.2.3_coll"
x <- rnorm(n)
y <- rnorm(n) 
z <- rbern(n, prob = inv_logit(2 *x + 2* y ))

pl_data_sim(x,y,z, filename = ugly, 
            xlab =  "Genetic Cancer Syndrome",
            ylab = "Carcinogen Exposure", 
            zlow = "Non neoplastic Pts",
            zhigh = "Neoplastic Pts")


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
u <- rnorm(n)
x <- rnorm(n)
z <- rnorm(n, mean = x)
y <- rnorm(n, mean = x + 2 * u )
s <- rbern(n, prob = inv_logit( z  + 2* u ))
s <- as.logical(s)

pl_data_sim(x[s],y[s],z[s], filename = ugly)
pl_ci_comp(x[s],y[s],z[s], filename = ugly)


##############################################
#                                            #
#     3.6.3 The Good Piping  Craftsman       #
#                                            #
##############################################

ugly <- "3.6.3_pip_craf"
z <- rnorm(n)
x <- rnorm(n)
u <- rnorm(n, mean = x + z)
y <- rnorm(n, mean = u)

pl_data_sim(x,y,z, filename = ugly)
pl_ci_comp(x,y,z, filename = ugly)


##############################################
#                                            #
#     3.6.4 The pre Piping Parasite          #
#                                            #
##############################################

ugly <- "3.6.4_pip_par"
z <- rbern(n)
x <- rnorm(n, mean = z, sd = 0.2)
y <- rnorm(n, mean = x, sd = 3)

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

##############################################
#                                            #
#     3.6.6 The post Piping Parasite         #
#                                            #
##############################################

ugly <- "3.6.6_popip_par"

x <- rnorm(n)
y <- rnorm(n, mean = x)
z <- rnorm(n, mean = y)

pl_data_sim(x,y,z, filename = ugly)
pl_ci_comp(x,y,z, filename = ugly)



##############################################
#                                            #
#     3.6.7 The post Piping Parasite         #
#                                            #
##############################################

ugly <- "3.6.7_biapopip_par"
u <- rnorm(n)
x <- rnorm(n, mean = u)
y <- rnorm(n, mean = x + u)
z <- rnorm(n, mean = y)

pl_data_sim(x,y,z, filename = ugly)
pl_ci_comp(x,y,z, filename = ugly)
