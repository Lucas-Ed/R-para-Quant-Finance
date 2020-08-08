
library(neuralnet)
library(quantmod)
library(DMwR)
library(zoo)
library(forecast)

getSymbols("IBM", src='yahoo',from = '2014-01-01', to = '2017-12-31')

names(IBM)<-c("open","high","low","close", "volume","ajusted")
dat = data.frame(Cl(IBM))

dat['closem1'] = Lag(Cl(IBM),1)
dat['closem2'] = Lag(Cl(IBM),2)
dat['closem3'] = Lag(Cl(IBM),3)
dat['closem4'] = Lag(Cl(IBM),4)
dat['closem5'] = Lag(Cl(IBM),5)

print(dat)

dat = na.fill(dat, "extend")

dat_scale = scale(dat)

nn = neuralnet(close  ~ closem1 + closem2 + closem3 + closem4 + closem5, data=dat_scale, hidden=c(2,3),threshold =1,stepmax= 1000)

print(nn)
plot(nn)

prev = predict(nn,dat_scale ) 

prev = unscale(prev,dat_scale)

print(prev)

plot(as.vector( Cl(IBM))  , type='l')
lines(prev,col='red')

accuracy(as.vector(prev),Cl(IBM))




