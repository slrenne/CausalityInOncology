
library(rethinking)
library(vioplot)

COLORS=c("#FFCCCC", "#9999FF")
png(filename="Ugly_Controls.png", width=4000, height=3500, bg="white",res=300)
par(mfrow = c(4, 2))
# 3.6.1 The Craftsman
n <- 1e4
ETOH <- rnorm(n,0,1)
HBV <- rnorm(n,0,1)
BLP <- ETOH + HBV + rnorm(n,0,1)

dat <- list(number_of_points=n,
            ETOH=ETOH,
            HBV=HBV,
            BLP=BLP)
m_all <- cstan(file = 'Craftsman_All.stan', data = dat,chains = 4,cores = 4, 
            iter = 2000)
post_all <- extract.samples(m_all)


dat <- list(number_of_points=n,
            ETOH=ETOH,
            BLP=BLP)
m_only_beta <- cstan(file = 'Craftsman_Only_Beta.stan', data = dat, chains = 4, 
            cores = 4, 
            iter = 2000)
post_only_beta <- extract.samples(m_only_beta)

expres_1 <- expression(y%~%alpha+beta*x+gamma*z)
expres_2 <- expression(y%~%alpha+beta*x)

plot(NULL, ylim = c(0,45), xlim = c(0.9,1.1),xlab = expression(beta),
     ylab = 'Density',main="The Craftsman",yaxt='n')
dens(post_all$beta,add=TRUE,lwd=2,col=COLORS[1])
dens(post_only_beta$beta,add=TRUE,lwd=2,lty=2,col=COLORS[2])
segments(x0=1,x1=1,y0=0,y1=40,col="red",lty=2,lwd=3)
legend(x = "topleft",lty = c(1,2,2), col= c(COLORS[1],COLORS[2],'red'),
       legend=c(expres_1, expres_2,'Real Effect'),bty = "n") 

plot(NULL, xlim = c(0,3),ylim = c(0.9,1.1),ylab='Density',xlab="",xaxt = 'n',
     main="The Craftsman",yaxt='n')
axis(1,at = 1:2,labels = c(expres_1,expres_2))
vioplot::vioplot(post_all$beta, horizontal=FALSE, at=1, drawRect=FALSE,
                 col=COLORS[1],0.6,add =TRUE)
vioplot::vioplot(post_only_beta$beta,at=2, horizontal=FALSE, drawRect=FALSE,
                 col=COLORS[2],0.6,add =TRUE)
legend(x = "topleft", col= c(COLORS[1],COLORS[2]),
       legend=c(expres_1, expres_2),bty = "n",fill=c(COLORS[1],COLORS[2]))
# 3.6.2 Parasite
n <- 1e4
HBV <- rnorm(n,0,1)
ETOH <- HBV + rnorm(n,0,1)
BLP <- ETOH + rnorm(n,0,1)

dat <- list(number_of_points=n,
            ETOH=ETOH,
            HBV=HBV,
            BLP=BLP)
m_all <- cstan(file = 'Craftsman_All.stan', data = dat,chains = 4,cores = 4, 
               iter = 2000)
post_all <- extract.samples(m_all)


dat <- list(number_of_points=n,
            ETOH=ETOH,
            BLP=BLP)
m_only_beta <- cstan(file = 'Craftsman_Only_Beta.stan', data = dat, chains = 4, 
                     cores = 4, 
                     iter = 2000)
post_only_beta <- extract.samples(m_only_beta)

expres_1 <- expression(y%~%alpha+beta*x+gamma*z)
expres_2 <- expression(y%~%alpha+beta*x)

plot(NULL, ylim = c(0,70), xlim = c(0.9,1.1),xlab = expression(beta),
     main="The Parasite",
     ylab = 'Density',
     yaxt='n')

dens(post_all$beta,add=TRUE,lwd=2,col=COLORS[1])
dens(post_only_beta$beta,add=TRUE,lwd=2,lty=2,col=COLORS[2])
segments(x0=1,x1=1,y0=0,y1=40,col="red",lty=2,lwd=3)
legend(x = "topleft",lty = c(1,2,2), col= c(COLORS[1],COLORS[2],'red'),
       legend=c(expres_1, expres_2,'Real Effect'),bty = "n") 


plot(NULL, xlim = c(0,3),ylim = c(0.9,1.1),ylab='Density',xlab="",xaxt = 'n',
     main="The Parasite",
     yaxt='n')
axis(1,at = 1:2,labels = c(expres_1,expres_2))
vioplot::vioplot(post_all$beta, horizontal=FALSE, at=1, drawRect=FALSE,
                 col=COLORS[1],0.6,add =TRUE)
vioplot::vioplot(post_only_beta$beta,at=2, horizontal=FALSE, drawRect=FALSE,
                 col=COLORS[2],0.6,add =TRUE)
legend(x = "topleft", col= c(COLORS[1],COLORS[2]),
       legend=c(expres_1, expres_2),bty = "n",fill=c(COLORS[1],COLORS[2]))

# 3.6.3 -- The Forking Craftsman

n <- 1e4
ETOH <- rnorm(n,0,1)
HBV <- ETOH + rnorm(n,0,1)
BLP <- ETOH + rnorm(n,0,1)

dat <- list(number_of_points=n,
            ETOH=ETOH,
            HBV=HBV,
            BLP=BLP)
m_all <- cstan(file = 'Craftsman_All.stan', data = dat,chains = 4,cores = 4, 
               iter = 2000)
post_all <- extract.samples(m_all)


dat <- list(number_of_points=n,
            ETOH=ETOH,
            BLP=BLP)
m_only_beta <- cstan(file = 'Craftsman_Only_Beta.stan', data = dat, chains = 4, 
                     cores = 4, 
                     iter = 2000)
post_only_beta <- extract.samples(m_only_beta)

expres_1 <- expression(y%~%alpha+beta*x+gamma*z)
expres_2 <- expression(y%~%alpha+beta*x)

plot(NULL, ylim = c(0,70), xlim = c(0.9,1.1),xlab = expression(beta),
     main="The Forking Craftsman",
     ylab = 'Density',yaxt='n')
dens(post_all$beta,add=TRUE,lwd=2,col=COLORS[1])
dens(post_only_beta$beta,add=TRUE,lwd=2,lty=2,col=COLORS[2])
segments(x0=1,x1=1,y0=0,y1=40,col="red",lty=2,lwd=3)
legend(x = "topleft",lty = c(1,2,2), col= c(COLORS[1],COLORS[2],'red'),
       legend=c(expres_1, expres_2,'Real Effect'),bty = "n") 

plot(NULL, xlim = c(0,3),ylim = c(0.9,1.1),ylab='Density',xlab="",xaxt = 'n',
     main="The Forking Craftsman",yaxt='n')
axis(1,at = 1:2,labels = c(expres_1,expres_2))
vioplot::vioplot(post_all$beta, horizontal=FALSE, at=1, drawRect=FALSE,
                 col=COLORS[1],0.6,add =TRUE)
vioplot::vioplot(post_only_beta$beta,at=2, horizontal=FALSE, drawRect=FALSE,
                 col=COLORS[2],0.6,add =TRUE)
legend(x = "topleft", col= c(COLORS[1],COLORS[2]),
       legend=c(expres_1, expres_2),bty = "n",fill=c(COLORS[1],COLORS[2]))






# 3.6.3 Bias Amplification
n <- 1e4
u <- rnorm(n,0,1)
z <- rnorm(n,0,1)
x <- u + z + rnorm(n,0,1)
y <- u + x + rnorm(n,0,1)

dat <- list(number_of_points=n,
            x=x,
            z=z,
            y=y)
m_all <- cstan(file = 'Bias_Amplification_All.stan', data = dat,chains = 4,cores = 4, 
               iter = 2000)
post_all <- extract.samples(m_all)


dat <- list(number_of_points=n,
            x=x,
            y=y)
m_only_beta <- cstan(file = 'Bias_Amplification_Only_Beta.stan', data = dat, chains = 4, 
                     cores = 4, 
                     iter = 2000)
post_only_beta <- extract.samples(m_only_beta)

expres_1 <- expression(y%~%alpha+beta*x+gamma*z)
expres_2 <- expression(y%~%alpha+beta*x)

plot(NULL, ylim = c(0,60), xlim = c(0.5,2),xlab = expression(beta),
     main="Bias Amplification",
     ylab = 'Density',yaxt='n')
dens(post_all$beta,add=TRUE,lwd=2,col=COLORS[1])
dens(post_only_beta$beta,add=TRUE,lwd=2,lty=2,col=COLORS[2])
segments(x0=1,x1=1,y0=0,y1=40,col="red",lty=2,lwd=3)
legend(x = "topleft",lty = c(1,2,2), col= c(COLORS[1],COLORS[2],'red'),
       legend=c(expres_1, expres_2,'Real Effect'),bty = "n") 

plot(NULL, xlim = c(0,3),ylim = c(1.3,1.6),ylab='Density',xlab="",
     main="Bias Amplification",yaxt='n', xaxt='n')
axis(1,at = 1:2,labels = c(expres_1,expres_2))
vioplot::vioplot(post_all$beta, horizontal=FALSE, at=1, drawRect=FALSE,
                 col=COLORS[1],0.6,add =TRUE)
vioplot::vioplot(post_only_beta$beta,at=2, horizontal=FALSE, drawRect=FALSE,
                 col=COLORS[2],0.6,add =TRUE)
legend(x = "topleft", col= c(COLORS[1],COLORS[2]),
       legend=c(expres_1, expres_2),bty = "n",fill=c(COLORS[1],COLORS[2]))
dev.off()

