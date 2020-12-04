library(ggplot2)
####随机性检验####
white_noise<-rnorm(1000)
whith_noise<-ts(white_noise)
ts.plot(white_noise,col='red')
abline(h=0)
abline(h=c(-2,-1,1,2),lty=3)
#### exam 2.1 ####
data<-read.csv("exam2-1.csv",head=T)
plot(data,ylab=" 纱产量 ",xlab=" 年份 ",type="o",col="blue")

acf(data$ 纱产量 ,lag.max=20,main=" 自相关系数 ")

Box.test(data$ 纱产量 ,lag=c(6),type = c('Box-Pierce'))
Box.test(data$ 纱产量 ,lag=c(12),type = c('Box-Pierce'))
#### exam 2.2####
data<-read.csv("exam2-2.csv",head=T)
z <- ts(data, start = c(1962, 1), frequency = 12)
plot(z,type="o",col="red")

data<-read.csv("exam2-2.csv",head=T)
z <- ts(data, start = c(1962, 1), frequency = 12)
acf(z)

data<-read.csv("exam2-2.csv",head=T)
z <- ts(data, start = c(1962, 1), frequency = 12)
for(i in 1:2)
  print(Box.test(z,lag=c(6*i),type = c('Box-Pierce')))
#### exam 2.3####
data<-read.csv("exam2-3.csv",head=T)
plot(data,ylab=" 温度 ",xlab=" 年份 ",type="o",col="red",xlim=c(1949,1998))
acf(data$ 温度 ,lag.max=20,main=" ")

data<-read.csv("exam2-3.csv",head=T)
for(i in 1:2)
  print(Box.test(data$温度 ,lag=6*i, type="Ljung-Box"))


