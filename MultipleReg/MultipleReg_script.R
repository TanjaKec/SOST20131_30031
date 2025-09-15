# If you don't have carData installed yet, uncomment and run the line below
# install.packages(carData)
library(carData)
data(Salaries)
attach(Salaries)
head(Salaries)
summary(Salaries)

boxplot(salary, col=c('chartreuse4'), main="distributions")
means <- sapply(salary, mean)
points(means, col="gray", pch=22, lwd=7)

boxplot(Salaries[,3:4], col=c('brown1', 'steelblue'), main="distributions")
means <- sapply(Salaries[,3:4], mean)
points(means, col="gray", pch=22, lwd=7)

# scatter plot salary versus yrs.since.phd
plot(salary ~ yrs.since.phd, cex=.6, main="salary vs yrs.since.phd", xlab="yrs.since.phd", ylab="salary")
# fit the model
model1 <- lm(salary ~ yrs.since.phd)
# add the line of the best fit on the scatter plot
abline(model1, lty=2, col=2)
summary(model1)

# scatter plot salary versus yrs.since.phd
plot(salary ~ yrs.service, cex=.6, main="salary vs yrs.service", xlab="yrs.service", ylab="salary")
# fit the model
model2 <- lm(salary ~ yrs.service)
# add the line of the best fit on the scatter plot
abline(model2, lty=2, col=2)
summary(model2)

mr_model <- lm(salary ~ yrs.since.phd + yrs.service)
summary(mr_model)

qf(0.95, 2, 394)

qt(0.95, 394)

qt(0.95, 394)

qt(0.975, 394)
