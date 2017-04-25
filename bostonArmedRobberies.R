
setwd("C:/Users/Kantapon/Documents/UC Davis/Winter 2017/STA137/Project")
data <- read.table("bostonArmedRobberies.txt")
names(data) <- c("Date", "ArmedRobberies")
attach(data)

par(mfrow = c(1,1))

plot(data, main="Date vs. ArmedRobberies")

hist(ArmedRobberies, main="Histogram of ArmedRObberies")

library(forecast)
library(tseries)
library(astsa)
x <- ArmedRobberies
time = 1:length(x)
ts.plot(x, main='Armed Robberies vs. Time', ylab="Number of Armed Robberies")

par(mfrow = c(2,2))
ts.plot(log(x), main = 'log(x)')
ts.plot(sqrt(x), main = 'sqrt(x)')
ts.plot(1/x, main = '1/x')
ts.plot(x**(1/3), main = 'x^(1/3)')

par(mfrow = c(1,1))
Acf(log(x))

ndiffs(log(x))
x1 <- diff(log(x))
par(mfrow = c(1,1))
ts.plot(x1, xlab="Scaled Number of Armed Robberies (Log)", main="Armed Robberies vs. Time (Logarithmic Transformation)")

adf.test(x1, c("stationary"))


par(mfrow = c(2,1))
Acf(x1)

Pacf(x1)
?Arima()

par(mfrow = c(3,1))

n = 118
m = floor(n/2)

pgrm.raw <- spec.pgram(x1, log='no')$spec

# vector of candidate L values for smoothing
spans = (1:(m-1))*2+1

# vector to store criterion values for each L
Q = numeric(length(spans))

# go through the L values and compute Q for each
for(j in 1:length(spans)){
  L = spans[j]
  pgrm.smooth = spec.pgram(x1, spans=L,log='no', plot=F)$spec
  Q[j] = sum((pgrm.smooth - pgrm.raw) ^ 2) + sum((pgrm.raw)^2)/(L-1)
}

# plot the values
plot(x=spans, y=Q, type='b')

# figure out which L is best
L = spans[which.min(Q)]; L

par(mfrow=c(3,1))
arma.spec(ar=c(0.3, -0.1), log='no', main='Spectral Density')
spec.pgram(x1, log='no')
spec.pgram(x1, spans=17, log='no')
par(mfrow=c(1,1))

par(mfrow= c(1,1))
model <- Arima(x1,order=c(2,0,1))
plot(model$fitted, main="Fitted Values vs. Observed Values", ylim=c(-0.65,0.65))
lines(x1, col="blue")
legend(90, 0.56, legend=c("Fitted Values", "Observed Values"), lty=c(1,1),col=c("black","blue"))

summary(model)

par(mfrow=c(2,1))
Acf(model$residuals, main = 'ACF of residuals')
Pacf(model$residuals, main = 'PACF of residuals')
Box.test(model$residuals, lag = 10, type = 'Ljung-Box')

par(mfrow=c(1,1))
hist(model$residuals, main = 'Histogram of residuals')
qqnorm(model$residuals)
qqline(model$residuals)

par(mfrow=c(1,1))
model1 = auto.arima(x1,max.p=8, max.q = 8, max.d=2)
plot(model1$fitted, main="Auto ARIMA Model")

summary(model1)

par(mfrow=c(2,1))
Acf(model1$residuals, main = 'ACF of residuals')
Pacf(model1$residuals, main = 'PACF of residuals')
Box.test(model1$residuals, lag = 10, type = 'Ljung-Box')

par(mfrow=c(1,1))
hist(model1$residuals, main = 'Histogram of residuals')
qqnorm(model1$residuals)
qqline(model1$residuals)

par(mfrow = c(3,1))

n = 118
m = floor(n/2)

pgrm.raw <- spec.pgram(model1$fitted, log='no')$spec

# vector of candidate L values for smoothing
spans = (1:(m-1))*2+1

# vector to store criterion values for each L
Q = numeric(length(spans))

# go through the L values and compute Q for each
for(j in 1:length(spans)){
  L = spans[j]
  pgrm.smooth = spec.pgram(model1$fitted, spans=L,log='no', plot=F)$spec
  Q[j] = sum((pgrm.smooth - pgrm.raw) ^ 2) + sum((pgrm.raw)^2)/(L-1)
}

# plot the values
plot(x=spans, y=Q, type='b')

# figure out which L is best
L = spans[which.min(Q)]; L

par(mfrow=c(3,1))
arma.spec(ar=c(0.3, -0.1), log='no', main='Spectral Density')
spec.pgram(model1$fitted, log='no')
spec.pgram(model1$fitted, spans=17, log='no')
par(mfrow=c(1,1))

str(data)

dataReduced <- data[1:108,2]
dataReducedTransform <- log(dataReduced)
modelReduced <- auto.arima(dataReducedTransform)

par(mfrow=c(1,1))
plot(modelReduced$fitted, main="Auto ARIMA Model of First 108 Observations")

summary(modelReduced)

par(mfrow=c(2,1))
Acf(modelReduced$residuals, main = 'ACF of residuals')
Pacf(modelReduced$residuals, main = 'PACF of residuals')
Box.test(modelReduced$residuals, lag = 10, type = 'Ljung-Box')

par(mfrow=c(1,1))
hist(modelReduced$residuals, main = 'Histogram of residuals')
qqnorm(modelReduced$residuals)
qqline(modelReduced$residuals)

par(mfrow=c(2,1))

plot(forecast(exp(modelReduced$x), h=10, level=95))
lines(data[,2])

plot(forecast(exp(modelReduced$x), h=10, level=95), xlim=c(109,118))
lines(data[,2])