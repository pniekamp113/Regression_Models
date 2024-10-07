#Regression Models

library(UsingR)
library(reshape)
library(ggplot2)
library(manipulate)
library(dplyr)
library(swirl)
library(GGally)
install_from_swirl("Regression Models")


#Module 1

#load data
data(galton)
head(galton)
long <- melt(galton)
long

myHist <- function(mu) {
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth = 1) +
    geom_vline(xintercept = mu, size =3) +
    ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep= ""))
  g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))



y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
  g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
  g <- g  + scale_size(range = c(2, 20), guide = "none" )
  g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
  g <- g + geom_point(aes(colour=freq, size = freq))
  g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
  g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
  mse <- mean( (y - beta * x) ^2 )
  g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
  g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))


g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")  
g <- g + geom_smooth(method="lm", formula=y~x)
g


#Regression to the mean

x <- rnorm(100)
y <- rnorm(100)
odr <- order(x)
x[odr[100]]
y[odr[100]]
odr


library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)

g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
g = g + geom_abline(intercept = 0, slope = 1)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g


#Quiz

x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

mod1 <- lm(x~w)
summary(mod1)


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

mod <- lm(x~y)
summary(mod)

data(mtcars)

mod2 <- lm(mtcars$mpg ~ mtcars$wt)
summary(mod2)  

plot(mtcars$mpg, mtcars$wt)



x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
x_normalized <- (x - mean(x)) / sd(x)
x_normalized[1]


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

mod <- lm(y ~x)
summary(mod)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)


#Inference in regression

library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n-2)) 
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")

coefTable
fit <- lm(y~x)
summary(fit)$coef
summary(fit)

sumCoef <- summary(fit)$coefficients
sumCoef

#getting the confidence interval

sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) / 10

#predictions

newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) 
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g



#Qiuz

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y~x)
summary(fit)

data(mtcars)

fit1 <- lm(mtcars$wt ~ mtcars$mpg)
summary(fit1)
confint(fit1, level = .95)
?confint

fit2 <- lm(mtcars$wt ~ mtcars$mpg)
summary(fit2)



fit1 <- lm(mpg ~ wt, data = mtcars)
new_car <- data.frame(wt = 3)

prediction <- predict(fit1, newdata = new_car, interval = "prediction", level = 0.95)
prediction


#short ton = 2,000, wt = 2

fit1 <- lm(mpg ~ wt, data = mtcars)
# Get the confidence intervals for the coefficients
confint_vals <- confint(fit1)
confint_vals
# Extract the confidence interval for the 'wt' coefficient
# Multiply by 2 to convert to short tons (since 'wt' is in 1,000 lbs)
ci_short_ton <- confint_vals["wt", ] * 2
ci_short_ton

# Fit the model with only the intercept
model_intercept <- lm(mpg ~ 1, data = mtcars)
model_full <- lm(mpg ~ wt, data = mtcars)
# Calculate the sum of squared errors for both models
sse_intercept <- sum(residuals(model_intercept)^2)  # TSS
sse_full <- sum(residuals(model_full)^2)  # RSS
# Calculate the ratio
ratio <- sse_full / sse_intercept
ratio



model <- lm(mpg~wt, data = mtcars)
avg_weight <- mean(mtcars$wt)
new_data <- data.frame(wt = avg_weight)
predict(model, newdata = new_data, interval = "confidence", level = 0.95)


#Multivariant analysis
#Multivariable regression analysis
#exclude confounding factors (e.g. only compare all smokers)

n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
x
x2
?rnorm

y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
plot(x, y)

ey = resid(lm(y~x2 + x3))
ex = resid(lm(x ~x2 + x3))
sum(ey * ex) /  sum(ex^2)
coef(lm(y~x+x2+x3))

#interpretations of a multivariate regression coefficient is the expected change in the response per unit change in the regressor, while holding all
#other regressors fixed


require(datasets)
data(swiss)
?swiss

head(swiss)
g = ggpairs(swiss, lower = list(continuous = "smooth"))
g

summary(lm(Fertility ~ . , data = swiss))
cor(swiss)
summary(lm(Fertility ~ Agriculture , data = swiss))

#Simpsons paradox

#How can adjustment reverse the sign of an effect? Let's try a simulation.

n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)

summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef


require(datasets);data(InsectSprays); require(stats); require(ggplot2)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

summary(lm(count ~ spray, data = InsectSprays))$coef

head(swiss)

hist(swiss$Catholic)
swiss <- mutate(swiss, CatholicBin = 1 * (Catholic > 50))
head(swiss)


g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("% in Agriculture") + ylab("Fertility")
g

#Two lines -> one for catholic and one for protestant. 
fit = lm(Fertility ~ Agriculture, data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit) [1], slope = coef(fit)[2], size = 2)
g1
summary(fit)


fit1 = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
summary(fit1)

fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)
g1

fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2, colour = "blue")
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], 
                      slope = coef(fit)[2] + coef(fit)[4], size = 2, colour = "red")
g1
summary(fit)

#Adjustment, is the idea of putting regressors into a linear model to investigate the role of a third variable on the relationship between another two. 


t
x

set.seed(2)
a <- runif(1000, -1, 1)
b <- rnorm(1000, 0, 1)
a
b
mean(a)
mean(b)
hist(a)
hist(b)

n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));

beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)



###

data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)


n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)

x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))

fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)


x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)    

round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)


###

n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2], 
    coef(lm(y ~ x1 + x2))[2], 
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)
s
plot(x1)
plot(x2)
plot(x3)
plot(x1, x2)


library(car)
fit <- lm(Fertility ~ ., data = swiss)
vif(fit)
sqrt(vif(fit))


data("mtcars")
head(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)
fit <- lm(mpg ~ wt + cyl, data = mtcars)
summary(fit)

prediction <- predict(fit, newdata = mtcars, interval = "prediction", level = 0.95)
prediction

mtcars$predicted_mpg <- predict(fit)
mtcars

# Create a ggplot
ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point(size = 3) +  # Plot original data points
  geom_line(aes(y = predicted_mpg), size = 1) +  # Add predicted mpg line
  labs(title = "Relationship Between Weight, Number of Cylinders, and MPG",
       x = "Weight (1000 lbs)",
       y = "Miles Per Gallon (MPG)",
       color = "Number of Cylinders") +
  theme_minimal()

fit_adjust <- lm(mpg ~ cyl + wt, data = mtcars)
fit_unadjust <- lm(mpg ~ cyl, data = mtcars)

summary(fit_adjust)
summary(fit_unadjust) #estimates are higher when wt is excluded (constant)

#Question 3

fit1 <- lm(mpg ~ cyl + wt, data = mtcars)
fit2 <- lm(mpg ~ cyl * wt, data = mtcars) #considers interaction between cyl and wt

summary(fit1)
summary(fit2)

# Perform a likelihood ratio test using anova()
lrt_result <- anova(fit1, fit2)
lrt_result

#Question 4

model <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
predict_new <- predict(model)
mtcars$predict_new <- predict(model)
mtcars

# Create a ggplot# Creamodel_interceptte a ggplot
ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point(size = 3) +  # Plot original data points
  geom_line(aes(y = predict_new), size = 1) +  # Add predicted mpg line
  labs(title = "Relationship Between Weight, Number of Cylinders, and MPG",
       x = "Weight (1000 lbs)",
       y = "Miles Per Gallon (MPG)",
       color = "Number of Cylinders") +
  theme_minimal()

mtcars
summary(model)


#Question 5

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

plot(x,y)

fit <- lm(y~x)
hatvalues(fit)


#Question 6

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

plot(x,y)

dfbetas(fit)


#Question 4


lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)


