############################ Clayton Copula #######################################
tau = 0.5
z=1000
theta =  2*tau/(1-tau)
rho=sin(pi*tau/2)
D<-array(NA,c(2,2)); D[1,1]=1; D[2,2]=1; D[1,2]=rho; D[2,1]=rho
x1<-numeric(); x2<-numeric()

corrC=numeric()
cmstatGC=numeric()
ksstatGC=numeric()
cmstatTC=numeric()
ksstatTC=numeric()
cmstatCC=numeric()
ksstatCC=numeric()

for(l in 1:100){
  for (n in 1:z) {
    s=runif(1); q=runif(1)
    f <- function(w) { abs((w-((w^(theta+1)-w)/theta))-q) }
    t=nlminb(q,f)$par
    v1=(1+s*((t^-theta)-1))^(-1/theta); v2=(1+(1-s)*((t^-theta)-1))^(-1/theta)
    x1[n]=qunif(v1); x2[n]=qunif(v2) }
  
  corrC[l] = cor(x1,x2,method="kendall")
  
  
  u = rank(x1)/z; v=rank(x2)/z
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=pmvnorm(lower=-Inf,upper=c(qnorm(u[n]),qnorm(v[n])),sigma=D)[1]
    cmstatG=cmstatG+(a-b)^2 }
  cmstatGC[l]=cmstatG
  cmstatG=0
  
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=pmvnorm(lower=-Inf,upper=c(qnorm(u[n]),qnorm(v[n])),sigma=D)[1]
    ksstatG=max(ksstatG,abs(a-b)) }
  ksstatGC[l]=ksstatG
  ksstatG=0 
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=pmvt(lower=-Inf,upper=c(qt(u[n],4),qt(v[n],4)),df=4,sigma=D)[1]
    cmstatT=cmstatT+(a-b)^2 }
  cmstatTC[l]=cmstatT
  cmstatT=0
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=pmvt(lower=-Inf,upper=c(qt(u[n],4),qt(v[n],4)),df=4,sigma=D)[1]
    ksstatT=max(ksstatT,abs(a-b)) }
  ksstatTC[l]=ksstatT
  ksstatT=0
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=(u[n]^(-theta)+v[n]^(-theta)-1)^(-1/theta)
    cmstatC=cmstatC+(a-b)^2 }
  cmstatCC[l]=cmstatC
  cmstatC=0
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=(u[n]^(-theta)+v[n]^(-theta)-1)^(-1/theta)
    ksstatC=max(ksstatC,abs(a-b)) }
  ksstatCC[l]=ksstatC
  ksstatC=0
}
plot(x1,x2,xlim=c(0,1),ylim=c(0,1))
status=c()
GTC=cbind(cmstatGC, cmstatTC, cmstatCC, ksstatGC, ksstatTC, ksstatCC, status )
GTC=data.frame(GTC)
GTC

for(l in 1:100){
  if(GTC$cmstatGC[l]<GTC$cmstatCC[l]|GTC$cmstatTC[l]<GTC$cmstatCC[l]|GTC$ksstatGC[l]<GTC$ksstatCC[l]|GTC$ksstatTC[l]<GTC$ksstatCC[l]){GTC$status[l]="Reject"}
  else{GTC$status[l]="Accept"}}
GTC
table(GTC$status)
############################ Joe Copula #######################################
tau=0.9
z=1000
rho=sin(pi*tau/2)
D<-array(NA,c(2,2)); D[1,1]=1; D[2,2]=1; D[1,2]=rho; D[2,1]=rho
temp=0; theta=0
while(temp<tau){
  theta = theta+0.01
  integral=0
  for(t in 1:9999){
    integral=integral+(1-(1-t/10000)^theta)*log(1-(1-t/10000)^theta)/(1-t/10000)^(theta-1)/10000 
  }
  temp = 1+4*integral/theta
}

x1<-numeric(); x2<-numeric()
corrJ=numeric()
cmstatGJ=numeric()
ksstatGJ=numeric()
cmstatTJ=numeric()
ksstatTJ=numeric()
cmstatJJ=numeric()
ksstatJJ=numeric()

for(l in 1:100){
  for (n in 1:z) {
    s=runif(1); q=runif(1)
    f = function(w){abs(w-(1-(1-w)^theta)*log(1-(1-w)^theta)/(theta*(1-w)^(theta-1))-q)}
    t=nlminb(q,f)$par
    v1 = 1-(1-exp(s*log(1-(1-t)^theta)))^(1/theta);
    v2 = 1-(1-exp((1-s)*log(1-(1-t)^theta)))^(1/theta)
    x1[n]=qunif(v1); x2[n]=qunif(v2)
  }
  corrJ[l]=cor(x1,x2,method="kendall")
  u = rank(x1)/z; v=rank(x2)/z
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=pmvnorm(lower=-Inf,upper=c(qnorm(u[n]),qnorm(v[n])),sigma=D)[1]
    cmstatG=cmstatG+(a-b)^2 }
  cmstatGJ[l]=cmstatG
  cmstatG=0
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=pmvnorm(lower=-Inf,upper=c(qnorm(u[n]),qnorm(v[n])),sigma=D)[1]
    ksstatG=max(ksstatG,abs(a-b)) }
  ksstatGJ[l]=ksstatG
  ksstatG=0 
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=pmvt(lower=-Inf,upper=c(qt(u[n],4),qt(v[n],4)),df=4,sigma=D)[1]
    cmstatT=cmstatT+(a-b)^2 }
  cmstatTJ[l]=cmstatT
  cmstatT=0
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=pmvt(lower=-Inf,upper=c(qt(u[n],4),qt(v[n],4)),df=4,sigma=D)[1]
    ksstatT=max(ksstatT,abs(a-b)) }
  ksstatTJ[l]=ksstatT
  ksstatT=0
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=1-((1-u[n])^theta+(1-v[n])^theta-(1-u[n])^theta*(1-v[n])^theta)^(1/theta)
    cmstatJ=cmstatJ+(a-b)^2 }
  cmstatJJ[l]=cmstatJ
  cmstatJ=0
  
  for (n in 1:z) {
    a=0
    for (m in 1:z) {
      if ((u[m]<u[n])&(v[m]<v[n])) { a=a+1/z } }
    b=1-((1-u[n])^theta+(1-v[n])^theta-(1-u[n])^theta*(1-v[n])^theta)^(1/theta)
    ksstatJ=max(ksstatJ,abs(a-b)) }
  ksstatJJ[l]=ksstatJ
  ksstatJ=0
}
plot(x1,x2,xlim=c(0,1),ylim=c(0,1))
status=c()
GTJ=cbind(cmstatGJ, cmstatTJ, cmstatJJ, ksstatGJ, ksstatTJ, ksstatJJ, status )
GTJ=data.frame(GTJ)

for(l in 1:100){
  if(GTJ$cmstatGJ[l]<GTJ$cmstatJJ[l]|GTJ$cmstatTJ[l]<GTJ$cmstatJJ[l]|GTJ$ksstatGJ[l]<GTJ$ksstatJJ[l]|GTJ$ksstatTJ[l]<GTJ$ksstatJJ[l]){GTJ$status[l]="Reject"}
  else{GTJ$status[l]="Accept"}}
GTJ
table(GTJ$status)
