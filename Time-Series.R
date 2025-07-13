data4a <- read.table(header = TRUE, "E:/d/tatat/zhdata/Question4a.txt")
data4b <- read.table(header = TRUE, "E:/d/tatat/zhdata/Question4b.txt")
data5 <- read.table(header = TRUE, "E:/d/tatat/zhdata/Question5.txt")
data5a <- read.table(header = TRUE, "E:/d/tatat/zhdata/Question5a.txt")
data5b <- read.table(header = TRUE, "E:/d/tatat/zhdata/Question5b.txt")
data6a <- read.table(header = TRUE, "E:/d/tatat/zhdata/Question6a.txt")
data6b <- read.table(header = TRUE, "E:/d/tatat/zhdata/Question6b.txt")

##4
#(a)
Consumption4a = subset(data4a, select = -c(1) )
lnConsumption4a=log(Consumption4a)
lnConsumption4a <-ts(lnConsumption4a, frequency=4, start=c(1955))
dlnConsumption4a=diff(lnConsumption4a,lag=1,difference=1)
dlnConsumption4a

library(forecast)
estimated4a<-auto.arima(dlnConsumption4a,ic="aic", stationary = TRUE, stepwise = FALSE,
           approximation = FALSE)
#best model ARIMA(0,0,3)(2,0,0) MA(3)for non seasonal part and AR(2) for seasonal part

Consumption4b = subset(data4b, select = -c(1) )
lnConsumption4b=log(Consumption4b)
lnConsumption4b <-ts(lnConsumption4b, frequency=4, start=c(2009))
dlnConsumption4b=diff(lnConsumption4b,lag=1,difference=1)

fore4b<-auto.arima(dlnConsumption4b,ic="aic", stationary = TRUE, stepwise = FALSE,
                        approximation = FALSE)
#best model ARIMA(0,0,3)

library(ggfortify)
ggtsdiag(estimated4a)

#(b)
fore4b
forecast4a <- forecast(estimated4a,h=36)
forecast4a

#(c)
library(slider)
rollingforecast4b<-slide(dlnConsumption4b, ~.x, .before = 1)

#(d)
autoplot(forecast4a)
autoplot(rollingforecast4b)

#(e)
accuracy(forecast4b)
#accuracy(rollingforecast4b)

##5
#(a)
Emp5 = subset(data5, select = -c(1) )
Emp5 <-ts(Emp5, frequency=12, start=c(1975))
dEmp5=diff(Emp5,lag=1,difference=1)
lnEmp5=log(Emp5)
lnEmp5 <-ts(lnEmp5, frequency=12, start=c(1975))
dlnEmp5=diff(lnEmp5,lag=1,difference=1)
d12lnEmp5=diff(lnEmp5,lag=12,difference=1)
dd12lnEmp5=diff(d12lnEmp5,lag=1,difference=1)
plot.ts(Emp5)
plot.ts(dlnEmp5)
plot.ts(d12lnEmp5)
plot.ts(dd12lnEmp5)

Emp5a = subset(data5a, select = -c(1) )
Emp5a <-ts(Emp5a, frequency=12, start=c(1975))
dEmp5a=diff(Emp5a,lag=1,difference=1)
lnEmp5a=log(Emp5a)
lnEmp5a <-ts(lnEmp5a, frequency=12, start=c(1975))
dlnEmp5a=diff(lnEmp5a,lag=1,difference=1)
d12lnEmp5a=diff(lnEmp5a,lag=12,difference=1)
dd12lnEmp5a=diff(d12lnEmp5a,lag=1,difference=1)
plot.ts(Emp5a)
plot.ts(dlnEmp5a)
plot.ts(d12lnEmp5a)
plot.ts(dd12lnEmp5a)

Emp5b = subset(data5b, select = -c(1) )
Emp5b <-ts(Emp5b, frequency=12, start=c(1975))
dEmp5b=diff(Emp5b,lag=1,difference=1)
lnEmp5b=log(Emp5b)
lnEmp5b <-ts(lnEmp5b, frequency=12, start=c(2015))
dlnEmp5b=diff(lnEmp5b,lag=1,difference=1)
d12lnEmp5b=diff(lnEmp5b,lag=12,difference=1)
dd12lnEmp5b=diff(d12lnEmp5b,lag=1,difference=1)
plot.ts(Emp5b)
plot.ts(dlnEmp5b)
plot.ts(d12lnEmp5b)
plot.ts(dd12lnEmp5b)


#(b)
ggseasonplot(dEmp5)
ggseasonplot(dlnEmp5)

#(c)
acf(lnEmp5)
acf(dlnEmp5)
acf(d12lnEmp5)
acf(dd12lnEmp5)
pacf(lnEmp5)
pacf(dlnEmp5)
pacf(d12lnEmp5)
pacf(dd12lnEmp5)

#(d)
library(urca)
ur.df(lnEmp5, type ="trend", selectlags = "AIC")
ur.df(d12lnEmp5, type ="trend", selectlags = "AIC")
ur.df(dd12lnEmp5, type ="trend", selectlags = "AIC")

ur.kpss(lnEmp5, type ="tau", lags = "long")
ur.kpss(d12lnEmp5, type ="tau", lags = "long")
ur.kpss(dd12lnEmp5, type ="tau", lags = "long")

#(e)

PQmax=6

AICs<-matrix(nrow=PQmax+1,ncol=PQmax+1)
rownames(AICs)<-c('AR0','AR1','AR2','AR3','AR4','AR5','AR6')
colnames(AICs)<-c('MA0','MA1','MA2','MA3','MA4','MA5','MA6')

for(ar in 0:PQmax){
  for(ma in 0:PQmax){
    fit <- arima(dd12lnEmp5a, c(ar,0,ma))
    AICs[ar+1,ma+1] <-AIC(fit)
  }}

minimizer = which(AICs==min(AICs), arr.ind =TRUE)
minimizer

library(lmtest)
coeftest(arima(dd12lnEmp5a, order=c(5,0,0)))
ggtsdiag(arima(dd12lnEmp5a, order=c(5,0,0)))

#(f)
auto.arima(dd12lnEmp5a,ic="aic", stationary = TRUE, stepwise = FALSE,
           approximation = FALSE)
ggtsdiag(auto.arima(dd12lnEmp5a,ic="aic", stationary = TRUE, stepwise = FALSE,
                    approximation = FALSE))

#(g)
Emp5bh<-ts(Emp5a, frequency=12, start=c(2015))
rollingforecast5bh<-slide(Emp5bh, ~.x, .before = 1)

#(h) #(i)
autoplot(rollingforecast5bh)
######## put data & autoarimadata??

#(j)
########Construct and plot the forecast errors for Et and for ???Et

#6.
#(a)

data6 <- merge(data6a, data6b, by=c("observation_date"), all.x = TRUE)

bivar6 = subset(data6, select = -c(1) )

lnbivar6=log(bivar6)
lnbivar6 <-ts(lnbivar6, frequency=4, start=c(1947))
dlnbivar6=diff(lnbivar6,lag=1,difference=1)

#ur.df(dlnbivar6, type ="trend", selectlags = "AIC")

ur.kpss(dlnbivar6, type ="tau", lags = "long")

#(b)
library(vars)
VARselect(dlnbivar6,lag.max=8, type="const")  
print(digits=4)

