set.seed(1)

inv_logit <- function (x) {
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}

N <- 1000 # sets the number of replicate for each simulation


# fork

# simulation
Smoking_status <- rbern(N, prob = 0.5 ) 
IHD <- rnorm(N, mean = Smoking_status)
Lung_Cancer <- rnorm(N, mean = Smoking_status) 

# graphical parameters
mysubtitle = "Lung Cancer ← Smoking → Ischaemic Heart Disease"
my_lab <- c('Lung Cancer ~ Ischaemic Heart Disease',
            expression(Lung~Cancer%~%Ischaemic~Heart~Disease['Smoking status=1']),
            expression(Lung~Cancer%~%Ischaemic~Heart~Disease['Smoking status=0']))
a <- 0.7
cols <- c( col.alpha(3,a) , col.alpha(4,a) )

# Figure
png(file = "figures/Fork.png",width = 2000, height = 2000, res=300)
plot(Lung_Cancer, IHD, main="The Fork", 
     col= cols[Smoking_status+1], pch=16, 
     xaxt="n", yaxt="n",
     xlab="Probability of Lung Cancer",
     ylab="Probability of Ischaemic Heart Disease")
mtext(side = 3, mysubtitle)
legend("topleft", legend=my_lab, col=c(1,4,3), lty=c(2,1,1), lwd = 2, cex=0.8, bty="n")
legend('bottomright', legend = c('Smokers', 'Non-smokers'), pch = 16, col = c(4,3), bty = 'n')
# no stratification 
abline(lm(Lung_Cancer~IHD),lwd=3,lty=2,col=1)
#Stratify by Smoking_status==1
abline(lm(Lung_Cancer[Smoking_status==1]~IHD[Smoking_status==1]),lwd=3,lty=1,col=4)
#Stratify by Smoking_status==0
abline(lm(Lung_Cancer[Smoking_status==0]~IHD[Smoking_status==0]),lwd=3,lty=1,col=3)
dev.off()

# pipe
# simulation
oncogenic_mutation <- rnorm(N)
tumor_grading <- rbern(N,inv_logit(oncogenic_mutation))
survival <- rnorm(N,(2*tumor_grading-1))

# graphical parameters
mysubtitle = "Oncogenic Mutation → Tumor Grading → Survival"
my_lab <- c('Survival ~ Oncogenic Mutation',
            expression(Survival%~%Oncogenic~Mutation['Low-grade']),
            expression(Survival%~%Oncogenic~Mutatio['High-grade']))
#Figure
png(file = "figures/Pipe.png",width = 2000, height = 2000,res=300)
plot(oncogenic_mutation,survival,
     main="The Pipe", 
     col=cols[tumor_grading+1], 
     pch=16, xaxt="n",yaxt="n",
     xlab="Amount of Oncogenic Mutation", ylab="Probability of Survival")
mtext(side = 3, mysubtitle)
legend("topleft", legend=my_lab, col=c("black", 4,3), lwd = 2, lty=c(2,1,1), cex=0.8,bty="n")
legend('bottomright', legend = c('High-grade', 'Low-grade'), pch = 16, col = c(4,3), bty = 'n')
# no stratification
abline(lm(oncogenic_mutation~survival),lwd=3,lty=2,col="black")
#Stratify by HG
abline(lm(oncogenic_mutation[tumor_grading==1]~survival[tumor_grading==1]),lwd=3,lty=1,col=4)
#Stratify by LG
abline(lm(oncogenic_mutation[tumor_grading==0]~survival[tumor_grading==0]),lwd=3,lty=1,col=3)

dev.off()



# collider
# simulation
Carcinocens <- rnorm(N)
age <- rnorm(N)
cancer_presence <- rbern(N, prob = inv_logit(Carcinocens*4 + age -2))
#graphical parameters
mysubtitle = "Carcinogens Exposure → Cancer ← Age"
my_lab <- c('Age ~ Carcinogens Exposure',
            expression(Age%~%Carcinogens~Exposure['no cancer']),  
            expression(Age%~%Carcinogens~Exposure['cancer']))
#figure
png(file = "figures/Collider.png",width = 2000, height = 2000,res=300)
plot(Carcinocens+rnorm(N,sd = 0.1),age,
     main="The Collider", col=cols[cancer_presence+1], 
     pch=16, xaxt="n",yaxt="n",xlab="Carcinogens Exposure",ylab="Age")
#no stratification
abline(lm(Carcinocens~age),lwd=3,lty=2,col="black")
#Stratify by with cancer
abline(lm(Carcinocens[cancer_presence==1]~age[cancer_presence==1]),lwd=3,lty=1,col=4)
# stratification of pts without cancer
abline(lm(Carcinocens[cancer_presence==0]~age[cancer_presence==0]),lwd=3,lty=1,col=3)
# legends
legend("topleft", legend=my_lab, col=c("black", 4,3), lwd = 2, lty=c(2,1,1), cex=0.8,bty="n")
legend('bottomright', legend = c('with cancer', 'without cancer'), pch = 16, col = c(4,3), bty = 'n')
mtext(side = 3, mysubtitle)
dev.off()
