
## 1.a

library(ISLR)
summary(Carseats)
attach(Carseats)
lm.fit =  lm(Sales ~ Price + Urban + US)
summary(lm.fit)

## 1.b

# Price: suggests a relationship between price and sales given the low p-value of the t-statistic. The coefficient states a negative relationship between Price and Sales: as Price increases, Sales decreases.

# UrbanYes: The linear regression suggests that there is not enough evidence for arelationship between the location of the store and the number of sales based.

# USYes: Suggests there is a relationship between whether the store is in the US or not and the amount of sales. A positive relationship between USYes and Sales: if the store is in the US, the sales will increase by approximately 1201 units.

## 1.c 

# Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes

## 1.d 

# Price and USYes, based on the p-values, F-statistic, and p-value of the F-statistic.

## 1.e

lm.fit2 =  lm(Sales ~ Price + US)
summary(lm.fit2)

## 1.f

# Based on the RSE and R^2 of the linear regressions, they both fit the data similarly, with linear regression from (e) fitting the data slightly better.

## 1.g 

confint(lm.fit2)

## 2.a
library(ISLR)
summary(Auto)
attach(Auto)
mpg01 =  rep(0, length(mpg))
mpg01[mpg>median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

## 2.c

train = (year %% 2 == 0) # if the year is even
test = !train
Auto.train =  Auto[train,]
Auto.test =  Auto[test,]
mpg01.test =  mpg01[test]

## 2.d
# Logistic regression
glm.fit =  glm(mpg01 ~ cylinders + weight + displacement + horsepower,
                data = Auto,
                family = binomial,
                subset = train)
glm.probs =  predict(glm.fit, Auto.test, type = "response")
glm.pred =  rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)

