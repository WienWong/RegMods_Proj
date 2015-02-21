
# Regression Models Project

data(mtcars)
# - `mpg`: Miles per US gallon
# - `cyl`: Number of cylinders
# - `disp`: Displacement (cubic inches)
# - `hp`: Gross horsepower
# - `drat`: Rear axle ratio
# - `wt`: Weight (lb / 1000)
# - `qsec`: 1 / 4 mile time
# - `vs`: V/S
# - `am`: Transmission (0 = automatic, 1 = manual)
# - `gear`: Number of forward gears
# - `carb`: Number of carburetors

dim(mtcars)
# 32 11

# head(mtcars, 4)

# A scatter plots is produced to roughly view the bivariate relationship among the 11 variables
?pairs
pairs(mtcars, panel = panel.smooth, main = "Pair Graph of Motor Trend Car Tests")

# MPG vs. Transmission
boxplot(mtcars$mpg ~ mtcars$am, xlab="Transmission (0 = Automatic, 1 = Manual)", 
        ylab="MPG", main="MPG vs. Transmission", col="red")
# According to the boxplot, manual transmission yields higher values of MPG in general. 

# To confirm the difference, a t-test is performed with the null hypothesis being that there is no difference in the mean MPG
# for automatic and manual transmission.
test <- t.test(mpg~am,data=mtcars)
test$p.value
# The p-value is far less than 0.05 thus the null hypothesis is rejected. 

# summary(lm(mtcars$mpg ~ . - 1, data = mtcars))

# There are 10 predicted variables and some must play minor roles to MPG consumption. Thus analysis of variance is necessary.
?aov
analysis <- aov(mpg ~ ., data = mtcars)
summary(analysis)
# Variables with p-value less than 0.5 are more important. Thereby, 4 fit models are proposed.

fit1 <- lm(mpg ~ cyl + wt, data = mtcars)
summary(fit1)

fit2 <- lm(mpg ~ cyl + wt + disp, data = mtcars)
summary(fit2)

fit3 <- lm(mpg ~ cyl + wt + am, data = mtcars)
summary(fit3)

fit4 <- lm(mpg ~ cyl + wt + disp + am, data = mtcars)
summary(fit4)
# The largest adjusted r-squared value of the fit4 model is 0.8327 thus fit4 is selected as the final multivariate model.

par(mfrow = c(2, 2), mar = c(3,1,2,2))
plot(fit4)
