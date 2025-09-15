# If you don't have carData installed yet, uncomment and run the line below
# install.packages(carData)
library(carData)
data(Salaries)
attach(Salaries)
class(sex)
unclass(sex)

options("contrasts")

contrasts(sex)
contrasts(discipline)
contrasts(rank)

# average salary values for each sex group
suppressPackageStartupMessages(library(dplyr))
Salaries %>% 
  select(salary, sex) %>%   
  group_by(sex) %>% 
  summarise(mean=mean(salary))

# regression model 
lm(salary ~  sex)

# If you don't have GGally installed yet, uncomment and run the line below
# install.packages(GGally)
suppressPackageStartupMessages(library(GGally))
ggpairs(Salaries)

# model_1 <- lm(salary ~ yrs.since.phd + yrs.service + discipline + sex + rank, data = Salaries) #long handed way
model_1 <- lm(salary ~ ., data = Salaries) # full stop, . , implies: all other variables in data that do not already appear in the formula
summary(model_1)

#model_1 <- lm(salary ~ yrs.since.phd + yrs.service + discipline + sex + rank, data = Salaries) # long handed method
model_2 <- update(model_1,~. - sex) # refitting by removing the least significant term
summary(model_2)

qt(0.95, 391)

coef(model_2)

model_2_1 <- lm(salary ~  0 + rank + discipline + yrs.since.phd + yrs.service)
summary(model_2_1)

wine = read.csv("https://raw.githubusercontent.com/egarpor/handy/master/datasets/wine.csv")
summary(wine)
ggpairs(wine)

model1 <- lm(Price ~ WinterRain + AGST + HarvestRain + Age + WinterRain * AGST * HarvestRain, data = wine)
summary(model1)

model2 <- update(model1, ~. -WinterRain:AGST:HarvestRain, data =wine)
summary(model2)

model3 <- update(model2, ~. -AGST:HarvestRain, data = wine)
summary(model3)

model4 <- update(model3, ~. -WinterRain:AGST, data = wine)
summary(model4)

model5 <- update(model4, ~. -WinterRain:HarvestRain, data = wine)
summary(model5)



