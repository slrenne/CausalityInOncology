set.seed(1)
library(rethinking)

N <- 1000 # sets the number of replicate for each simulation


# fork

# simulation
Smoking_status <- rbern(N, prob=inv_logit(0.5)) 
IHD <- rnorm(N, mean = Smoking_status)
Lung_Cancer <- rnorm(N, mean = Smoking_status) 

# graphical parameters
mysubtitle = "Lung Cancer ← Smoking → Ischaemic Heart Disease"
my_lab <- c('Lung Cancer ~ Ischaemic Heart Disease',
            expression(Lung~Cancer%~%Ischaemic~Heart~Disease['Smoking status=1']),
            expression(Lung~Cancer%~%Ischaemic~Heart~Disease['Smoking status=0']))
a <- 0.7
cols <- c( col.alpha(2,a) , col.alpha(3,a) )

# Figure
png(file = "figures/Fork.png",width = 2000, height = 2000,res=300)
plot(Lung_Cancer,
     IHD,
     main="The Fork", 
     col=cols[Smoking_status+1], 
     pch=16, 
     xaxt="n",
     yaxt="n",
     xlab="Lung Cancer",
     ylab="Ischaemic Heart Disease")
mtext(side = 3, mysubtitle)
legend("topleft", legend=my_lab, col=c(1,3,2), lty=c(2,1,1), cex=0.8,bty="n")
# no stratification 
abline(lm(Lung_Cancer~IHD),lwd=3,lty=2,col=1)
#Stratify by Smoking_status==1
abline(lm(Lung_Cancer[Smoking_status==1]~IHD[Smoking_status==1]),lwd=3,lty=1,col=3)
#Stratify by Smoking_status==0
abline(lm(Lung_Cancer[Smoking_status==0]~IHD[Smoking_status==0]),lwd=3,lty=1,col=2)
dev.off()

# pipe
png(file = "Pipe.png",width = 4000, height = 4000,res=300)

Tumor_Promoting_Survival <- rnorm(N, mean = 0, sd = 1) # generates N random values normally distributed
Survival <- rbern(N, prob = inv_logit(Tumor_Promoting_Survival)) # same as the fork's Y

Tumor_Grading <- rnorm(N, mean = Survival) # with a probability proportional to X


mysubtitle = "Tumor Promoting Mutation → Tumor Grading → Survival"

my_lab <- c('Tumor Grading ~ Tumor Promoting Survival',
            expression(Tumor~Grading%~%Tumor~Promoting~Survival['Survival=0']),
            expression(Tumor~Grading%~%Tumor~Promoting~Survival['Survival=1']))

plot(Tumor_Grading,Tumor_Promoting_Survival,main="The Pipe", col=cols[Survival+1], pch=16, xaxt="n",yaxt="n",xlab="Tumor Grading",ylab="Tumor Promoting Survival")
abline(lm(Tumor_Grading~Tumor_Promoting_Survival),lwd=3,lty=2,col="black")

#Stratify by Survival==1
abline(lm(Tumor_Grading[Survival==1]~Tumor_Promoting_Survival[Survival==1]),lwd=3,lty=1,col=2)
#Stratify by Survival==0
abline(lm(Tumor_Grading[Survival==0]~Tumor_Promoting_Survival[Survival==0]),lwd=3,lty=1,col=1)

mtext(side = 3, mysubtitle)
legend("topleft", legend=my_lab, col=c("black", 2,1), lty=c(2,1,1), cex=0.8,bty="n")
dev.off()



# collider


png(file = "Collider.png",width = 4000, height = 2000,res=300)

Tumor_Promoting_Survival <- rnorm(N, mean = 0, sd = 1) # generates N random values normally distributed

Tumor_Grading <- rnorm(N, mean = 0, sd=1) # with a probability proportional to X

Survival <- rbern(N, prob = inv_logit(Tumor_Grading+Tumor_Promoting_Survival)) # same as the fork's Y


mysubtitle = "Germline Mutation TP53 → Survival ← Age"

my_lab <- c('Germline Mutation TP53 ~ Age',
            expression(Germline~Mutation~TP53%~%Age['Survival=0']),
            expression(Germline~Mutation~TP53%~%Age['Survival=1']))

plot(Tumor_Grading,Tumor_Promoting_Survival,main="The Collider", col=cols[Survival+1], pch=16, xaxt="n",yaxt="n",xlab="Germline Mutation TP53",ylab="Age")
abline(lm(Tumor_Grading~Tumor_Promoting_Survival),lwd=3,lty=2,col="black")

#Stratify by Survival==1
abline(lm(Tumor_Grading[Survival==1]~Tumor_Promoting_Survival[Survival==1]),lwd=3,lty=1,col=2)
#Stratify by Survival==0
abline(lm(Tumor_Grading[Survival==0]~Tumor_Promoting_Survival[Survival==0]),lwd=3,lty=1,col=1)

mtext(side = 3, mysubtitle)
legend("topleft", legend=my_lab, col=c("black", 2,1), lty=c(2,1,1), cex=0.8,bty="n")
dev.off()

# https://urldefense.proofpoint.com/v2/url?u=https-3A__fabiandablander.com_r_Causal-2DInference.html&d=DwIGaQ&c=5rLNXN0mp_7LMh3Fds96xpjyD06ZuE2RU7zikolS0lg&r=y76TMtVUYsFGbfDq9nJS0WbZeQ76UT6_9yhZbHeshcQ&m=e6KY6F0bru3Z9Lc4PqtZJxd74EsKcVo-jt4VvEFhWfOx0lyO44QyKpwS742pQqOA&s=9G__4eUfXVqyPaFcBd3JlePZf1fOVpo55zcMH39Y8U4&e= 
# https://urldefense.proofpoint.com/v2/url?u=https-3A__www.latentview.com_blog_causal-2Dinference-2Dexploring-2Dthe-2Dhow-2Dbehind-2Dthe-2Dwhy_&d=DwIGaQ&c=5rLNXN0mp_7LMh3Fds96xpjyD06ZuE2RU7zikolS0lg&r=y76TMtVUYsFGbfDq9nJS0WbZeQ76UT6_9yhZbHeshcQ&m=e6KY6F0bru3Z9Lc4PqtZJxd74EsKcVo-jt4VvEFhWfOx0lyO44QyKpwS742pQqOA&s=col8K08LErlgmScXQUfXQxQoQ8Fcyal04ZNEqm27bew&e= 


a <- 0.7
cols <- c( col.alpha(1,a) , col.alpha(2,a) )

N <- 1000
X <- rnorm(N)
Z <- rbern(N,inv_logit(X))
Y <- rnorm(N,(2*Z-1))

print(plot(X,Y,col=cols[Z+1],pch=16, main="Pipe"))
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)
abline(lm(Y[Z==0]~X[Z==0]),col=1,lwd=3)
abline(lm(Y~X),lwd=3,lty=3)


#https://urldefense.proofpoint.com/v2/url?u=https-3A__www.oreilly.com_radar_what-2Dis-2Dcausal-2Dinference_-23-3A-7E-3Atext-3DForks-2520and-2520confounders-2CWe-2520call-2520this-2520a-2520fork&d=DwIGaQ&c=5rLNXN0mp_7LMh3Fds96xpjyD06ZuE2RU7zikolS0lg&r=y76TMtVUYsFGbfDq9nJS0WbZeQ76UT6_9yhZbHeshcQ&m=e6KY6F0bru3Z9Lc4PqtZJxd74EsKcVo-jt4VvEFhWfOx0lyO44QyKpwS742pQqOA&s=FbIaSh2_gcKJ4getisKXSytwgM0MSuXEHr9ySzlggWg&e= .
# Variable Z has a common effect on both X and Y, and X and Y seems to be correlated
# However when we look X and Y independently, we see no effect
N <- 1000
Z <- rbern(N)
X <- rnorm(N,2*Z-1)
Y <- rnorm(N,(2*Z-1))

plot(X,Y,col=cols[Z+1],pch=16,main="Fork")
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3) # Check for correlation between X and Y for positive cases
abline(lm(Y[Z==0]~X[Z==0]),col=1,lwd=3) # Check for correlation between X and Y for negative cases
abline(lm(Y~X),lwd=3,lty=3) # Check general correlation for X and Y




# Variable X and Y has common effect on Z.
# Imagine going to hospital (Z) because of Covid or Other disease
# Basically we can see negative correlation between Covid and other disease
# Since if number of patients going to hospital beacuse of Covid is high
# other disease's patients will be low.

# However in reality this is not true, There is no correlation between covid and other disease

N <- 1000
X <- rnorm(N) # Covid
Y <- rnorm(N) # Other Disease
Z <- rbern(N,inv_logit(2*X+2*Y-2)) # Going to Hospital

plot(X,Y,col=cols[Z+1] , pch=16, main="Collider")
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)
abline(lm(Y[Z==0]~X[Z==0]),col=1,lwd=3)
abline(lm(Y~X),lwd=3,lty=3)

