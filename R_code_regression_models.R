#Regression Models

library(UsingR)
library(reshape)
library(ggplot2)
library(manipulate)
library(dplyr)
library(swirl)
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
