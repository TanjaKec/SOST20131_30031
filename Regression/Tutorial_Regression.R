suppressPackageStartupMessages(library(dplyr))
# read csv file
companyd <- read.csv("https://tanjakec.github.io/mydata/SHARE_PRICE.csv", header=T)
# look at the data
glimpse(companyd)

# convert into factore type variables
companyd[, 5] <- as.factor(companyd[, 5])
companyd[, 6] <- as.factor(companyd[, 6])
companyd[, 7] <- as.factor(companyd[, 7])
# glance at data: what's the difference between the following two functions?
summary(companyd)
glimpse(companyd)

attach(companyd)
names(companyd)
plot(Share_Price ~ Profit, cex = .6, main = "Share Price = f(Profit) + e")
abline

lm(Share_Price ~ Profit)
summary(lm(Share_Price ~ Profit))

# create a scatterplot
plot(Share_Price ~ Profit, cex = .6, main = "Share Price = f(Profit) + e")
# fit the model
model_1 <- lm(Share_Price ~ Profit)
# add the line of the best fit on the scatterplot
abline(model_1, lty = 2, col = 2)

summary(model_1)$r.squared

summary(model_1)

F_crit <- qf(0.95, 1, 58)
F_crit

summary(Profit)

