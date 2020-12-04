#### Session 4
####模拟 AR 和 MA 序列####
opar<-par(no.readonly=T) 
par(mfrow=c(2,2)) 
acf(arima.sim(n=100,list(ar=0.9)),10,col="red")
acf(arima.sim(n=100,list(ar=0.1)),10,col="red")
acf(arima.sim(n=100,list(ar=-0.9)),10,col="red")
acf(arima.sim(n=100,list(ar=-0.1)),10,col="red")
par(opar) 
#### 模型定阶 ####
##case1模型定阶 _ 相关图法
m<-read.csv("exam4-1.csv",header=F)
n<-as.vector(t(m))
data1<-ts(n,start=1950,end=2008)
plot.ts(data1,type="o",col="red",ylab="",xlab="Year",main=" 我国邮路及农村投递线路年新增量 ")

for(i in 1:2)
  print(Box.test(data1, lag=6*i, type="Ljung-Box"))

acf(data1,lag.max=20,main=" 自相关图 ",ci.col="red")
pacf(data1,lag.max=20,main=" 偏自相关图 ",ci.col="red")
##case2模型定阶 _ 相关图法
m<-read.csv("exam4-2.csv",header=F)
n<-as.vector(t(m))
data2<-ts(n,start=1,end=57)
plot.ts(data2,type="o",col="red",ylab="",xlab="Day",main="OVERSHORT 序列时序图 ")

for(i in 1:2)
  print(Box.test(data2, lag=6*i, type="Ljung-Box"))

acf(data2,lag.max=20,main=" 自相关图 ",ci.col="red")
pacf(data2,lag.max=20,main=" 偏自相关图 ",ci.col="red")

## case1模型定阶 _ 最佳准则法
library(zoo)
library(forecast)
m<-read.csv("exam4-1.csv",header=F)
n<-as.vector(t(m))
data1<-ts(n,start=1950,end=2008)
auto.arima(data1)
## case 2_ 模型定阶 _ 最佳准则法
#library(zoo)
#library(forecast)
m<-read.csv("exam4-2.csv",header=F)
n<-as.vector(t(m))
data2<-ts(n,start=1,end=57)
auto.arima(data2)

####参数估计####
##Case1 参数估计
m<-read.csv("exam4-1.csv",header=F)
n<-as.vector(t(m))
data1<-ts(n,start=1950,end=2008)
m1=arima(data1, order = c(2,0,0),method="ML")
m1
##case 2—— 参数估计
m<-read.csv("exam4-2.csv",header=F)
n<-as.vector(t(m))
data2<-ts(n,start=1,end=57)
m2=arima(data2, order = c(0,0,1),method="CSS")
m2
##case 3—— 参数估计
m<-read.csv("exam4-3.csv",header=F)
data3<-ts(m,start=1880)
plot.ts(diff(data3),type="o",col="red",ylab="",xlab="Year",main=" 全球气表平均温度改变值差分 ")

for(i in 1:2)
  print(Box.test(diff(data3), lag=6*i, type="Ljung-Box"))

acf(diff(data3),lag.max=20,main="acf",ci.col="red")
pacf(diff(data3),lag.max=20,main="pacf",ci.col="red")
auto.arima(diff(data3))
####模型检验####
##case 1— 模型检验
m<-read.csv("exam4-1.csv",header=F)
n<-as.vector(t(m))
data1<-ts(n,start=1950,end=2008)
m1=arima(data1, order = c(2,0,0),method="ML")
e1=m1$residuals
for(i in 1:2)
  print(Box.test(e1, lag=6*i, type="Ljung-Box"))
tsdiag(m1)
m1
pt(0.7185/0.1083,df=56,lower.tail = F)
pt(-0.5294/0.1067,df=56,lower.tail = T)
pt(11.0223/3.0906,df=56,lower.tail = F)
##case2— 模型检验
m<-read.csv("exam4-2.csv",header=F)
n<-as.vector(t(m))
data2<-ts(n,start=1,end=57)
m2=arima(data2, order = c(0,0,1),method="CSS")
e2=m2$residuals
for(i in 1:2)
  print(Box.test(e2, lag=6*i, type="Ljung-Box"))
tsdiag(m2)
m2
pt(-0.8208/0.0996,df=55,lower.tail = T)
pt(-4.4095/1.1655,df=55,lower.tail = T)
##case 3— 模型检验
m<-read.csv("exam4-3.csv",header=F)
data3<-ts(m,start=1880)
m3=arima(diff(data3), order = c(1,0,1),method="CSS")
e3=m3$residuals
for(i in 1:2)
  print(Box.test(e3, lag=6*i, type="Ljung-Box"))
tsdiag(m3)
m3
pt(0.4046/0.1247,df=102,lower.tail = F)
pt(-0.8954/0.0666,df=102,lower.tail = T)
pt(0.0049/0.0023,df=102,lower.tail = F)
####模型优化####
m<-read.csv("exam4-4.csv",header=F)
n<-as.vector(t(m))
data4<-ts(n)
plot.ts(data4,type="o",col="red",ylab="",xlab="Time",main=" 化学反应过程时序图 ")
for(i in 1:2)
  print(Box.test(data4, lag=6*i, type="Ljung-Box"))
acf(data4,lag.max=20,main="acf",ci.col="red")
pacf(data4,lag.max=20,main="pacf",ci.col="red",ylim=c(-0.4,1))
m41=arima(data4,order=c(0,0,2),method="CSS")
m41
pt(-0.3228/0.1163,df=67,lower.tail = T)
pt(0.3102/0.1235,df=67,lower.tail = F)
pt(51.1730/1.2565,df=67,lower.tail = F)
e41<-residuals(m41)
for(i in 1:2)
  print(Box.test(e41, lag=6*i, type="Ljung-Box"))
tsdiag(m41)
m42=arima(data4,order=c(1,0,0),method="CSS")
m42
pt(-0.4249/0.1138,df=68,lower.tail = T)
pt(51.2921/0.9117,df=68,lower.tail = F)
e42<-residuals(m42)
for(i in 1:2)
  print(Box.test(e42, lag=6*i, type="Ljung-Box"))
tsdiag(m42)
aic2=-2*-266.32+2*3
bic2=-2*-266.32+log(70)*3
aic1=-2*-265.23+2*4
bic1=-2*-265.23+log(70)*4
####模型预测####
##case 1—— 模型预测
m<-read.csv("exam4-1.csv",header=F)
n<-as.vector(t(m))
data1<-ts(n,start=1950,end=2008)
m1=arima(data1, order = c(2,0,0),method="ML")
library(forecast)
predict=forecast(m1,h=5)
plot(predict)
lines(fitted(m1),col=2,lty=2)
##case 2— 模型预测
library(forecast)
m<-read.csv("exam4-2.csv",header=F)
n<-as.vector(t(m))
data2<-ts(n,start=1,end=57)
m2=arima(data2, order = c(0,0,1),method="CSS")
predict=forecast(m2,h=5)
plot(predict)
lines(fitted(m2),col=2,lty=2)
##case 3—— 模型预测
m<-read.csv("exam4-3.csv",header=F)
data3<-ts(m,start=1880)
m3=arima(diff(data3), order = c(1,0,1),method="CSS")
predict=forecast(m3,h=5)
plot(predict)
lines(fitted(m3),col=2,lty=2)
