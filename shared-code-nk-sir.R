
#------------------------Start regR.r----------------------------#
#Correlation
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor(x,y)
#Correlation test
cor.test(x, y, method = "kendall", alternative = "greater")
cor.test(x, y, method = "kendall", alternative = "greater",
         exact = FALSE) # using large sample approximation
cor.test(x, y, method = "spearm", alternative = "g")
cor.test(x, y,                    alternative = "g")
data(trees)
cor(trees)
# Correlations with significance levels
library(Hmisc)
rcorr(as.matrix(trees)) 

################
###Regression###
fit1= lm(trees$Volume ~ trees$Height) # yi  = a + bxi + ei
fit1
plot(trees$Height,trees$Volume)
abline(fit1$coef)
attach(trees)
fit1= lm(Volume ~ Height) #yi  = a + bxi + ei
fit1= lm(Volume ~ Height, data=trees)# yi  = a + bxi + ei
fit2= lm(Volume ~ Height + Girth, data=trees) # y=a+b1x1+b2x2+e
fit3= lm(Volume ~ Height + Girth + Height:Girth, data=trees) # y=a+b1x1+b2x2+b3x1x2+e 
fit3= lm(Volume ~ Height * Girth, data=trees) # y=a+b1x1+b2x2+b3x1x2+e
fit4= lm(Volume ~ Height*Girth - Height, data=trees) # y=a+b2x2+b3x1x2+e
fit4= lm(Volume ~ Girth + Girth:Height, data=trees) # y=a+b2x2+b3x1x2+e
fit5= lm(Height ~ Girth - 1, data=trees)  
fit5= lm(Height ~ 0 + Girth, data=trees) # y=bx+e 
fit5=update(fit1,.~.-1)
###
fit6= lm(Volume ~ I(Height / Girth), data=trees) # y=a+b(x1/x2)+e
fit7= lm(Volume ~ I(Girth^2), data=trees)#y=a+bx^2+e
fit8= lm(log(Height) ~ log(Girth), data=trees) # log(y)=a+blog(x)+e

#####################
##extracting output##
names(fit2)
coef(fit2)
fitted(fit2)
resid(fit2)
formula(fit2)
anova(fit2)
summary(fit2)
AIC(fit2)
par(mfrow=c(2,2))
plot(fit2)
dfbetas(fit2)#estimate of parameter deleting each observation at a time
dffits(fit2)#predicted vlue deleting each observation at a time
covratio(fit2)#
cooks.distance(fit2)#multivariate distance of each point
hatvalues(fit2)#leverage values
rstandard(fit2)#standardized residuals
rstudent(fit2)#studentized residuals

####Regression Diagnostic
# Assume that we are fitting a multiple linear regression
# on the MTCARS data
library(car)
attach(mtcars)
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 

##### Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 

## Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
cutoff
plot(fit, which=4, cook.levels=cutoff)

#### Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)

# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)

#### Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?

####Test for Autocorrelated Errors
durbinWatsonTest(fit)




##################
leveragePlots(fit) # leverage plots
avPlots(fit)# added variable plots
### Influence Plot
##influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

####Evaluate Nonlinearity
# component + residual plot
crPlots(fit)
# Ceres plots
ceresPlots(fit)

#####Robust Regression
library(car) # for data
data(Duncan)
attach(Duncan)
Duncan
plot(prestige ~ income)
mod.ls <- lm(prestige ~ income + education, data=Duncan)
summary(mod.ls)
mod.ls1 <- update(mod.ls, subset=-c(6))
summary(mod.ls)
library(MASS)
mod.huber <- rlm(prestige ~ income + education, data=Duncan)
summary(mod.huber)
plot(mod.huber$w, ylab="Huber Weight")
smallweights <- which(mod.huber$w < 0.8)
showLabels(1:45, mod.huber$w, rownames(Duncan), id.method=smallweights)
mod.bisq <- rlm(prestige ~ income + education, data=Duncan, method="MM")
summary(mod.bisq)
plot(mod.bisq$w, ylab="Bisquare Weight")
showLabels(1:45, mod.bisq$w, rownames(Duncan), id.method= which(mod.bisq$w < 0.8))

##### Stepwise Regression
library(MASS)
fit <- lm(y~x1+x2+x3,data=mydata)
step <- stepAIC(fit, direction="both")
step$anova # display results 

#### All Subsets Regression
library(leaps)
attach(mydata)
leaps<-regsubsets(y~x1+x2+x3+x4,data=mydata,nbest=10)
# view results
summary(leaps)
#------------------------End regR.r----------------------------#




#------------------------Start GLM.r----------------------------#
###### GLM-Logistic regression #####
data(iris)
iris1<-iris[1:98,]
logistic_model <- glm(Species ~ Sepal.Length + Sepal.Width+Petal.Length+ Petal.Width, 
                      data = iris1, 
                      family = "binomial")
logistic_model
summary(logistic_model)
predict_reg <- predict(logistic_model, 
                       iris[c(22,98,99),], type = "response")
predict_reg  


##### ordered logistic Regression###
library(MASS)
View(housing)
house.plr <- polr(Sat ~ ., weights = Freq, data = housing)
house.plr
predict_reg <- predict(house.plr, 
                       housing[1:10,], type = "p")
predict_reg

##### Multinomial logistic Regression###
library("nnet")
mlog<-multinom(Sat~ ., data = housing)
predict_reg <- predict(mlog, 
                       housing[1:10,])

predict_reg
#------------------------End GLM.r----------------------------#


#------------------------Start RegressionDiagnosis.r----------------------------#
###Cooks Distance:
library(robustbase)
data(hbk)

linReg<-lm(Y~.,data=hbk)
cooksd<-cooks.distance(linReg)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red") 
text(x=1:length(cooksd), y=cooksd,labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""),col=2)

###Standardized pearson residuals:
linReg<-lm(Y~.,data=hbk)
resid<-residuals(linReg, type="pearson")
plot(residuals(linReg, type="pearson"),ylab="Standardized Pearson Residuals")
abline(h=c(2,-2), col=c("red" ,"red"),lwd=2)
obs<-which(resid>2 | resid<(-2))
text(obs,resid[obs],labels=obs, cex= 1,pos=2)
obs

#### Model Selection
lmMod <- lm(Y ~ . , data = hbk)
selectedMod <- step(lmMod)
summary(selectedMod)

###### GLM-Logistic regression #####
install.packages("dplyr")
library(dplyr)
#------------------------End RegressionDiagnosis.r----------------------------#


#------------------------Start NK_Sir_code.r----------------------------#
##################################################################
# In a course 20% students got A grade. 5 students are          ##
# randomly selected from this group of students. Find the       ##
# probability that out of 5 students (i) 2 students got A,      ##
# (ii) at least one student got A (iii) at best 1 student got A ##
##################################################################

#i#
p<-dbinom(2,size=5,prob=0.2)

#ii##
p1<-1-pbinom(0,size=5,prob=0.2)
x<-c(1:5)
p11<-sum(dbinom(x,size=5,prob=0.2))

#iii##
p2<-pbinom(1,size=5,prob=0.2)


##########################################################################
##In a telephone exchange 5 wrong calls are received out of 100 calls   ##
##In an hour 250 calls are received. Find the probability that, in that ##
##hour (i)there are 2 wrong calls, (ii)there are at least 3 wrong calls,## 
##(iii) there are at best 3 wrong calls.                                ##
##########################################################################

#i#
lambda<-250*5/100
p12<-dpois(2,lambda)

#ii##
p22<-1-ppois(2,lambda)
x<-c(3:250)
p221<-sum(dpois(x,lambda))

#iii##
p23<-ppois(3,lambda)


##################################################################################
##The loaf volume of a bakery follows normal distribution with mean =10 cft     ##
##and standard deviation 0.8 c.f.t Find the probability that a randomly selected##
##loaf has the volume (i) more than 12 cft (ii) 8 to 12 cft.                    ##
##################################################################################

##1##
pg12<-1-pnorm(12,10,0.8)

#ii#
p8t12<-pnorm(12,10,0.8)-pnorm(8,10,0.8)


#########################
######################################################
# Children's IQ scores are normally distributed with a
# mean of 100 and a standard deviation of 15. What
# proportion of children are expected to have an IQ between
# 80 and 120?
######################################################

mean=100; sd=15
lb=80; ub=120
x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)
plot(x, hx, type="l", xlab="IQ Values", ylab="",
     main="Normal Distribution", axes=FALSE)
i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(40, 160, 20), pos=0) 


###########################################################
###  Parametric Test
###########################################################

# one sample t-test #

snacks <- c(87.7,80.01,77.28,78.76,81.52,74.2,80.71,79.5,77.87,81.94,80.7,
            82.32,75.78,80.19,83.91,79.4,77.52,77.62,81.4,74.89,82.95,
            73.59,77.92,77.18,79.83,81.23,79.28,78.44,79.01,80.47,76.23,
            78.89,77.14,69.94,78.54,79.7,82.45,77.29,75.52,77.21,75.99,
            81.94,80.41,77.7)
#### Test: mu=80 ###
length(snacks)

n <- length(snacks)

snack.mean <- mean(snacks)
snack.sd <- sd(snacks)
snack.se <- snack.sd/sqrt(n)
snack.T <- (snack.mean-80)/snack.se
snack.T
pt(snack.T,df=n-1)

##########################3#######################333333
t.test(x=snacks,mu=80,alternative="less")

#two sample t-test ##
snacks2 <- c(80.22,79.73,81.1,78.76,82.03,81.66,80.97,81.32,80.12,78.98,
             79.21,81.48,79.86,81.06,77.96,80.73,80.34,80.01,81.82,79.3,
             79.08,79.47,78.98,80.87,82.24,77.22,80.03,79.2,80.95,79.17,81)

snack2.mean <- mean(snacks2)
snack2.sd <- sd(snacks2)

t.test(x=snacks2,y=snacks,alternative="two.sided",conf.level=0.9)

##check Manually
######################################################
####Chi Square
######################################################
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))

Xsq <- chisq.test(M)
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null

######################################################
#### F Test
######################################################

A = c(16, 17, 25, 26, 32, 34, 38, 40, 42)
B = c(600, 590, 590, 630, 610, 630)
# var test in R
var.test(A, B, alternative = "two.sided")

#################################################
data <- data.frame(values=c(18, 19, 22, 25, 27, 28, 41, 45, 51, 55,
                            14, 15, 15, 17, 18, 22, 25, 25, 27, 34),
                   group=rep(c('A', 'B'), each=10))


var.test(values~group, data=data)

#########################################################
### Wilcoxon Test
#########################################################

women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Create a data frame
my_data <- data.frame(group = rep(c("Woman", "Man"), each = 9),
                      weight = c(women_weight,  men_weight))

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )
boxplot(my_data$weight~my_data$group)

boxplot(my_data$weight~my_data$group, col=c(2,3))

wilcox.test(weight ~ group, data = my_data, 
            exact = FALSE, alternative = "less")


wilcox.test(weight ~ group, data = my_data,
            exact = FALSE, alternative = "greater")

###########################################################
####    Fitting Univariate Distribution                ####
###########################################################

x.norm<-rnorm(n=200,m=10,sd=2)
hist(x.norm,main="Histogram of observed data")

plot(density(x.norm),main="Density estimate of data")
plot(ecdf(x.norm),main="Empirical cumulative distribution function")

z.norm<-(x.norm-mean(x.norm))/sd(x.norm) 
qqnorm(z.norm) 
abline(0,1)

#######################################
x.poi<-rpois(n=200,lambda=2.5)
hist(x.poi,main="Poisson distribution")
#######################################
#------------------------End NK_Sir_code.r----------------------------#





