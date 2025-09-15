# ====================================
# Statistics: An Introduction Using R
# by Michael J. Crawley
# ====================================

# ====================================
# code 1   Fundamentals: page 1 to 23
# ====================================


#  draw the figures illustrating maximum likelihood

x<-c(1,3,4,6,8,9,12)
y<-c(5,8,6,10,9,13,12)
windows(14,6)
par(mfrow=c(1,3))
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(0,0.6793,col="red")
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(8,0.6793,col="red")
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(lm(y~x))
abline(lm(y~x),col="blue")



plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(4.8273,1.5,col="red")
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(4.8273,0.2,col="red")
plot(x,y,pch=21,bg="blue",ylim=c(0,15))
abline(lm(y~x))
abline(lm(y~x),col="blue")


# randomizing treatments for experimental design

treatments <- c("aloprin","vitex","formixin","panto","allclear")

#   use sample to shuffle them for the active insects in dishes 1 to 5

sample(treatments)


# this produces a warning message because the same variable name
# appears in two attached dataframes

first.frame <- read.csv("data/test.pollute.csv")
second.frame <- read.csv("data/ozone.data.csv")
attach(first.frame)
attach(second.frame)

# this is how you should avoid this kind of problem

first.frame <- read.csv("data/test.pollute.csv")
second.frame <- read.csv("data/ozone.data.csv")
attach(first.frame)

# ........    
#  this is where you work on the information from first.frame. 
#  Then when you are finished
# ........   

detach(first.frame)
attach(second.frame)

# ====================================
# code 2   Dataframes: page 23 to 34
# ====================================

# read data from a file called worms.csv to create a 
# dataframe called worms

worms <- read.csv("data/worms.csv")
names(worms)

attach(worms)
worms

# all rows, just columns 1 to 3

worms[,1:3]

#  all columns just rows 5 to 15

worms[5:15,]

# all columns but only selected rows (area > 3 and slope < 3)

worms[Area>3 & Slope <3,]
# sort the rows by increasing area
worms[order(Area),]
# only the columns with numeric data
worms[order(Area),c(2,3,5,7)]
# sort into decending order with just two columns
worms[rev(order(worms[,5])),c(5,7)]

# using tapply with a specified dataframe

with(worms,tapply(Worm.density,Vegetation,mean))

# using aggregate to summarise multiple variables by facor levels

aggregate(worms[,c(2,3,5,7)],list(Vegetation),mean)

aggregate(worms[,c(2,3,5,7)],list(Community=Vegetation),mean)

# multiple explanatory variables

aggregate(worms[,c(2,3,5,7)],
          list(Moisture=Damp,Community=Vegetation),mean)

# aggregate and tapply compared

with(worms,tapply(Slope,list(Damp,Vegetation),mean))

# plotting your data

data <- read.csv("data/das.csv")
attach(data)
head(data)

# finding the identity of the outlier

which(y > 10)
y[50]

# plots with categorical explanatory variables

yields <- read.csv("data/fertyield.csv")
attach(yields)
head(yields)
table(treatment)
which(treatment == "nitogen")
detach(yields)

# ====================================
# code 5   Single Samples: page 66 to 80
# ====================================

# single samples

data <- read.csv("data/example.csv")
attach(data)
names(data)

summary(y)

boxplot(y)

hist(y)

# rug plot

length(table(y))
plot(range(y),c(0,10),type="n",xlab="y values",ylab="")
for (i in 1:100) lines(c(y[i],y[i]),c(0,1),col="blue")

# designing a histogram

(max(y)-min(y))/10

diff(range(y))/11

# the game of craps

score <- 2:12

ways <- c(1,2,3,4,5,6,5,4,3,2,1)

( game <- rep(score,ways) )

sample(game,1)

outcome <- numeric(10000)
for (i in 1:10000) outcome[i] <- sample(game,1)
hist(outcome,breaks=(1.5:12.5))

mean.score <- numeric(10000)
for (i in 1:10000) mean.score[i] <- mean(sample(game,3))
hist(mean.score,breaks=(1.5:12.5))

mean(mean.score)
sd(mean.score)

xv <- seq(2,12,0.1)
yv <- 10000*dnorm(xv,mean(mean.score),sd(mean.score))
hist(mean.score,breaks=(1.5:12.5),ylim=c(0,3000),col="yellow", main="")
lines(xv,yv,col="red")

# standard normal disribution

standard.deviations <- seq(-3,3,0.01)
pd <- dnorm(standard.deviations)
plot(standard.deviations,pd,type="l",col="blue")

pnorm(-2)
pnorm(-1)
1-pnorm(3)

qnorm(c(0.025,0.975))

# shading the tails of the standard normal distribution

xv<-seq(-3,3,0.01)
yv<-dnorm(xv)
plot(c(-3,3),c(0,0.3),xlim=c(-3,3),ylim=c(0,0.4),type="n",ylab="pd",xlab="standard deviations")
polygon(c(1.96,1.96,-1.96,-1.96,xv[105:496]),c(yv[496],0,0,yv[105],yv[105:496]),col="green")
polygon(c(-1.96,-1.96,xv[1],xv[1:104]),c(yv[104],0,0,yv[1:104]),col="red")
polygon(c(xv[601],xv[601],1.96,1.96,xv[497:601]),c(yv[601],0,0,yv[496:601]),col="red")
text(0,0.2,"95%",cex=2)
lines(xv,yv,col="blue")


# calculations with the sandard normal distribution


ht <- seq(150,190,0.01)
plot(ht,dnorm(ht,170,8),type="l",col="brown",
     ylab="Probability density",xlab="Height")

pnorm(-1.25)

pnorm(1.875)
1 - pnorm(1.875)

pnorm(1.25) - pnorm(-0.625)

# drawing a panel of four normal distributions

par(mfrow=c(2,2))

ht <- seq(150,190,0.01)
pd <- dnorm(ht,170,8)

plot(ht,dnorm(ht,170,8),type="l",col="brown",
     ylab="Probability density",xlab="Height")

plot(ht,dnorm(ht,170,8),type="l",col="brown",
     ylab="Probability density",xlab="Height")
yv <- pd[ht<=160]
xv <- ht[ht<=160]
xv <- c(xv,160,150)
yv <- c(yv,yv[1],yv[1])
polygon(xv,yv,col="orange")


plot(ht,dnorm(ht,170,8),type="l",col="brown",
     ylab="Probability density",xlab="Height")
xv <- ht[ht>=185]
yv <- pd[ht>=185]
xv <- c(xv,190,185)
yv <- c(yv,yv[501],yv[501])
polygon(xv,yv,col="blue")

plot(ht,dnorm(ht,170,8),type="l",col="brown",
     ylab="Probability density",xlab="Height")
xv <- ht[ht>=160 & ht <= 180]
yv <- pd[ht>=160 & ht <= 180]
xv <- c(xv,180,160)
yv <- c(yv,pd[1],pd[1])
polygon(xv,yv,col="green")

# plots for skewness

data <- read.csv("data/skewdata.csv")
attach(data)
qqnorm(values)
qqline(values,lty=2)

# speed of light data

light <- read.csv("data/light.csv")
attach(light)
names(light)
hist(speed)
summary(speed)



# ====================================
# code 7   Regression: 114 to 134
# ====================================

# Please read the book chapter 7, pages: 114-134 and run the code

# text figure

plot(c(0,10),c(0,100),xlab="",ylab="",type="n")
lines(c(0,10),c(80,10),lwd=2)

# intercept = 80

lines(c(0,0),c(0,80),col="green")
lines(c(0,-10),c(80,80),col="red")

# slope = -7

lines(c(2,8),c(24,24),col="brown")
lines(c(2,2),c(66,24),col="blue")

# tannin example

reg.data <- read.csv("data/tannin.csv")
attach(reg.data)
names(reg.data)

plot(tannin,growth,pch=21,bg="blue")

lm(growth~tannin)
abline(lm(growth~tannin),col="green")

fitted <- predict(lm(growth~tannin))
fitted
lines(c(0,0),c(12,11.7555555))

# residuals

for (i in 1:9) 
  lines (c(tannin[i],tannin[i]),c(growth[i],fitted[i]),col="red")

# estimating the maximum likelihood slope

b <-  seq(-1.43,-1,0.002)
sse <- numeric(length(b))
for (i in 1:length(b)) {
  a <- mean(growth)-b[i]*mean(tannin)
  residual <- growth - a - b[i]*tannin
  sse[i] <- sum(residual^2)
}

plot(b,sse,type="l",ylim=c(19,24))
arrows(-1.216,20.07225,-1.216,19,col="red")
abline(h=20.07225,col="green",lty=2)
lines(b,sse)

b[which(sse==min(sse))]


# corrected sums of squares

SSX <- sum(tannin^2)-sum(tannin)^2/length(tannin)
SSY <- sum(growth^2)-sum(growth)^2/length(growth)
SSXY <- sum(tannin*growth)-sum(tannin)*sum(growth)/length(tannin)

# box 7.5 figure

plot(c(0,10),c(0,10),xlab="",ylab="",type="n")
abline(h=5,lty=2)
lines(c(0,10),c(8,2))
text(2,6.2,expression(hat(y) - bar(y)))
text(2,8.45,expression(y - hat(y)))
arrows(7,5,7,9.5,code=3,length=0.1)
arrows(1,5,1,7.4,code=3,length=0.1)
arrows(1,9.5,1,7.4,code=3,length=0.1)
points(1,9.5,pch=16)
text(8,7.4,expression(y - bar(y)))
text(0.2,5,expression(bar(y)))
text(.2,7.4,expression(hat(y)))
text(.2,9.5,"y")


# regreesion model in R


model <- lm(growth~tannin)
summary(model)
summary.aov(model)

par(mfrow=c(2,2)) 
plot(model)


# ================================================================
# ================== further reading =============================
# optional reading: pages 135-150
# ================================================================

# a non-linear relationship

par(mfrow=c(1,1))
data <- read.csv("data/decay.csv")
attach(data)
names(data)

plot(time,amount,pch=21,col="blue",bg="green")

abline(lm(amount~time),col="red")
summary(lm(amount~time))

plot(time,log(amount),pch=21,col="blue",bg="red")
abline(lm(log(amount)~time),col="blue")

model <- lm(log(amount)~time)
summary(model)

par(mfrow=c(1,1))
plot(time,amount,pch=21,col="blue",bg="green")
xv <- seq(0,30,0.25)
yv <- 94.38536 * exp(-0.068528 * xv)
lines(xv,yv,col="red")

# shapes of quadratic relationships

par(mfrow=c(2,2))
curve(4+2*x-0.1*x^2,0,10,col="red",ylab="y")
curve(4+2*x-0.2*x^2,0,10,col="red",ylab="y")
curve(12-4*x+0.3*x^2,0,10,col="red",ylab="y")
curve(4+0.5*x+0.1*x^2,0,10,col="red",ylab="y")

model2 <- lm(amount~time)
model3 <- lm(amount~time+I(time^2))
summary(model3)
AIC(model2,model3)
anova(model2,model3)


# non-linear regression using nls

deer <- read.csv("data/jaws.csv")
attach(deer)
names(deer)
par(mfrow=c(1,1))
plot(age,bone,pch=21,bg="lightgrey")

model <- nls(bone~a-b*exp(-c*age),start=list(a=120,b=110,c=0.064))
summary(model)

model2 <- nls(bone~a*(1-exp(-c*age)),start=list(a=120,c=0.064))
anova(model,model2)

av <- seq(0,50,0.1)
bv <- predict(model2,list(age=av))
lines(av,bv,col="blue")
summary(model2)

null.model <- lm(bone ~ 1)
summary.aov(null.model)


#  generalized additive models GAM

library(mgcv)
hump <- read.csv("data/hump.csv")
attach(hump)
names(hump)

model <- gam(y~s(x))

plot(model,col="blue")
points(x,y-mean(y),pch=21,bg="red")

summary(model)





