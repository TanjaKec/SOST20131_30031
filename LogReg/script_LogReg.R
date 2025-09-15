bankrup <- read.csv("https://tanjakec.github.io/mydata/Bankruptcies.csv")
summary(bankrup)

model_lm = lm(Y ~  X1, data = bankrup)
# plot a scatter diagram of Y vs X1
plot(Y ~  X1, data = bankrup, 
     col = "orangered", pch = "|", ylim = c(-0.2, 1.2),
     main = "using linear regression function for binomial Y")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
abline(h = 0.5, lty = 2)
abline(model_lm, lwd = 3, col = "navy")

lgreg_plot <- glm(Y ~  X1, data = bankrup, binomial(logit))
summary(lgreg_plot)
range(bankrup$Y)
range(bankrup$X1)
x1re_ta <- seq(-309, 69, 3)
yx1 <- predict(lgreg_plot, list(X1 = x1re_ta), type="response")

plot(Y ~  X1, data = bankrup, 
     col = "orangered", pch = "|", ylim = c(-0.2, 1.2),
     main = "using logistic regression function for binomial Y ")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
lines(x1re_ta, yx1, lty = 1, lwd = 3, col = "navy")

# Exampe

suppressPackageStartupMessages(library(dplyr))

bankrup <- read.csv("https://tanjakec.github.io/mydata/Bankruptcies.csv")
summary(bankrup)

glimpse(bankrup)


set.seed(123)
split_idx = sample(nrow(bankrup), 53)
bankrup_train = bankrup[split_idx, ]
bankrup_test = bankrup[-split_idx, ]

glimpse(bankrup_train)
summary(as.factor(bankrup_train$Y))
summary(bankrup_train)

model <- glm(Y ~ X1 + X2 + X3, data = bankrup_train, family = binomial(logit))
model

summary(model)

round(exp(coef(model)), 4)

G_calc <- model$null.deviance - model$deviance
Gdf <- model$df.null - model$df.residual
pscl::pR2(model)
G_calc
qchisq(.95, df = Gdf) 
1 - pchisq(G_calc, Gdf)

anova(model, test="Chisq")

model_new <- update(model, ~. -X3, data = bankrup)
summary(model_new)
anova(model_new, test="Chisq")


link_pr <- round(predict(model_new,  bankrup_test, type = "link"), 2)
link_pr

response_pr <- round(predict(model_new,  bankrup_test, type = "response"), 2)

t(bankrup_test$Y)


bankrup_test[c(3, 12),]

round(1/(1+exp(-(-0.5503398 + 0.1573639*(-18.1) + 0.1947428*(-6.5)))), 4)
round(1/(1+exp(-(-0.5503398 + 0.1573639*54.7 + 0.1947428*14.6))), 4)


how_well <- data.frame(response_pr, bankrup_test$Y) %>% 
  mutate(result = round(response_pr) == bankrup_test$Y)

how_well

confusion_matrix <- table(bankrup_test$Y, round(response_pr))
confusion_matrix
coefficients(model_new)


accuracy <- function(x){
  sum(diag(x) / (sum(rowSums(x)))) * 100
}

accuracy(confusion_matrix)