data = read.csv("CBA.AX.csv")

dr = (data$Close-data$Open)/data$Open
date = data$Date
date_d = as.Date(data$Date)
date_m = cut(date_d, breaks = "month")
date_m = data.frame(date_m)


daily_return = cbind(date,date_m, dr)

daily_return

CBA = data.frame(daily_return)
CBA[CBA<=0]=NA
CBA = na.omit(CBA)
CBA

class(CBA)

#And we are interested at the monthly minimum daily return, and for easier computation afterwards, 
#we will break the date into month and year, such that if we have 12-9-21, 
#and after the R computation, the date will recognize as 9-21. 


library(dplyr)

minimum_return_raw = CBA %>% group_by(date_m) %>% summarise(minreturn = min(dr))%>%arrange(date_m)
minimum_return_raw

############################################################################################################
minimum_return = data.frame(minimum_return_raw)
minimum_return

losses = numeric()

for (i in 1:121){losses[i]=minimum_return[i,2]}


dgumbel = function(x,m,s){1/s*exp(-(x-m)/s)*exp(-exp(-(x-m)/s))}

f = function(p){-sum(log(dgumbel(losses,p[1],p[2])))}

iv = c(0.01,0.01)

nlm(f,iv)


pgumbel <- function(x,m,s) { exp(-exp(-(x-m)/s)) }
plot((c(1:121)-0.5)/180,pgumbel(sort(losses),0.0006656139, 0.0006159132),xlim=c(0,1),ylim=c(0,1),
     pch=16,xlab="sample cdf",ylab="fitted cdf",main="Gumbel")
lines(c(0,1),c(0,1))



qgumbel <- function(p,m,s) { m-log(-log(p))*s }
c<-numeric()
for (i in 1:14) { c[i]=qgumbel(i/15,0.0006656139, 0.0006159132) }
c[15]=100
cn<-numeric()
for (i in 1:15) {
  cn[i]=length(losses[losses<=c[i]])
  }
n<-numeric()
n[1]=cn[1]
for (i in 2:15) { n[i]=cn[i]-cn[i-1] }
sum((n-121/15)^2/121*15)
qchisq(0.95,12)

max(abs(pgumbel(sort(losses),0.0006656139, 0.0006159132)-
          c(1:121)/121),abs(pgumbel(sort(losses),0.0006656139, 0.0006159132)-c(0:120)/121))
1.36/sqrt(121)

#####################################################################################
dfrechet = function(x,m,s,xi) { 1/s*(1+xi*(x-m)/s)^(-1/xi-1)*exp(-((1+xi*(x-m)/s)^(-1/xi))) }

f = function(p) { -sum(log(dfrechet(losses,p[1],p[2],p[3]))) }
iv = c(0.01,0.01,0.01)
nlm(f,iv)

pfrechet <- function(x,m,s,xi) { exp(-((1+xi*(x-m)/s)^(-1/xi))) }
plot((c(1:121)-0.5)/121,pfrechet(sort(losses),0.0005225270, 0.0004421402, 0.5041440692),xlim=c(0,1),
     ylim=c(0,1),pch=16,xlab="sample cdf",ylab="fitted cdf",main="Frechet")
lines(c(0,1),c(0,1))

qfrechet <- function(p,m,s,xi) { m+((-log(p))^(-xi)-1)/xi*s }
c<-numeric()
for (i in 1:14) { c[i]=qfrechet(i/15,0.0005225270, 0.0004421402, 0.50414406921) }
c[15]=100
cn<-numeric()
for (i in 1:15) {
  cn[i]=length(losses[losses<=c[i]]) }
n<-numeric()
n[1]=cn[1]
for (i in 2:15) { n[i]=cn[i]-cn[i-1] }
sum((n-121/15)^2/121*15)
qchisq(0.95,12)

max(abs(pfrechet(sort(losses), 0.0005225270, 0.0004421402, 0.50414406921)-
          c(1:121)/121),abs(pfrechet(sort(losses),0.0005225270, 0.0004421402, 0.50414406921)-c(0:120)/121))
1.36/sqrt(121)
####################################################################################
hist(losses)

plot(seq(0,0.09,0.0001),dgumbel(seq(0,0.09,0.0001),0.0006656139, 0.0006159132),xlab
     ="x",ylab="f(x)",type="l",lty=3) 
lines(seq(0,0.09,0.0001),dfrechet(seq(0,0.09,0.0001),0.0005225270, 0.0004421402, 0.50414406921),type="l")


####################################################################################
dl = cbind(data$Date,data$Adj.Close)
daily_losses = data.frame(dl)
daily_losses
daily_losses$X2 = as.numeric(daily_losses$X2)
losses_d = numeric()

for (i in 1:2532) {losses_d[i]=-(daily_losses[i,2]/daily_losses[i+1,2]-1) }

quantile(losses_d, probs = 0.9, na.rm = T)
quantile(losses_d, probs = 0.95, na.rm = T)
quantile(losses_d, probs = 0.99, na.rm = T)

e<-numeric()
for (i in 1:100) {
  exceedances=losses_d-i/1000
  e[i]=mean(exceedances[exceedances>0], na.rm = T) }
plot(c(1:100)/1000,e,xlab="u",ylab="e(u)",type="l")


dgpd0 <- function(x,m,s) { 1/s*exp(-(x-m)/s) }
exceedances=losses_d-0.013
exceedances[is.na(exceedances)]=0
f <- function(p) { -sum(log(dgpd0(exceedances[exceedances>0],0,p))) }
iv=0.1
nlm(f,iv)

exceedances=losses_d-0.04
exceedances[is.na(exceedances)]=0
f <- function(p) { -sum(log(dgpd0(exceedances[exceedances>0],0,p))) }
iv=0.1
nlm(f,iv)

sigma<-numeric()
for (i in 1:100) {
  exceedances=losses_d-i/1000
  exceedances[is.na(exceedances)]=0
  f <- function(p) { -sum(log(dgpd0(exceedances[exceedances>0],0,p))) }
  iv=0.1
  sigma[i]=nlm(f,iv)$estimate }
plot(c(1:100)/1000,sigma,xlab="u",ylab="sigma",type="l")

exceedances=losses_d-0.013
length(exceedances[exceedances>0])

exceedances=losses_d-0.04
length(exceedances[exceedances>0])

exceedances=losses-0.09
length(exceedances[exceedances>0])

exceedances=losses_d-0.013
pgpd0 <- function(x,m,s) { 1-exp(-(x-m)/s) }
plot((c(1:264)-0.5)/264,
     pgpd0(sort(exceedances[exceedances>0]),0,sigma[13]),
     xlim=c(0,1),ylim=c(0,1),pch=16,xlab="sample cdf",
     ylab="fitted cdf",main="GPD xi=0 u=1.3%")
lines(c(0,1),c(0,1))

qgpd0 <- function(p,m,s) { m-s*log(1-p) }
c<-numeric()
for (i in 1:12) { c[i]=qgpd0(i/13,0,sigma[13]) }
c[13]=10
exceedances=losses_d-0.013
temp=exceedances[exceedances>0]
cn<-numeric()
for (i in 1:13) {
  cn[i]=length(temp[temp<=c[i]]) }
n<-numeric()
n[1]=cn[1]
for (i in 2:13) { n[i]=cn[i]-cn[i-1] }
sum((n-264/13)^2/264*13)
qchisq(0.95,11)

max(abs(pgpd0(sort(exceedances[exceedances>0]),0,sigma[13])-c(1:264)/264),
    abs(pgpd0(sort(exceedances[exceedances>0]),0,sigma[13])-c(0:263)/264))
1.36/sqrt(264)

####################################################################################
exceedances=losses_d-0.04
plot((c(1:13)-0.5)/13,pgpd0(sort(exceedances[exceedances>0]),0,sigma[40]),xlim=c(0,1),ylim=c(0,1),
     pch=16,xlab="sample cdf",ylab="fitted cdf",main="GPD xi=0 u=4%")
lines(c(0,1),c(0,1))

c<-numeric()
for (i in 1:3) { c[i]=qgpd0(i/4,0,sigma[40]) }
c[4]=10
exceedances=losses_d-0.04
temp=exceedances[exceedances>0]
cn<-numeric()
for (i in 1:4) {
  cn[i]=length(temp[temp<=c[i]]) }
n<-numeric()
n[1]=cn[1]
for (i in 2:4) { n[i]=cn[i]-cn[i-1] }
sum((n-13/4)^2/13*4)
qchisq(0.95,2)

max(abs(pgpd0(sort(exceedances[exceedances>0]),0,sigma[40])-c(1:13)/13),
    abs(pgpd0(sort(exceedances[exceedances>0]),0,sigma[40])-c(0:12)/13))
1.36/sqrt(13)

###################################################################################
###################################################################################