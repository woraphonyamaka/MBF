# This is an  function named 'Maximum Bayes Factor'
# Written by Dr.Worphon Yamaka,
# Center of excellence in Econometrics, Faculty of Economics,
# Chiang Mai University
#
#
## Main Function

MBF=function(t,method){

if (method=="pvalue"){
# P-value
pv=2*(1-pnorm(t))
round(pv,4)
}
if (method=="goodman"){
# goodman(1996)
mbf=exp(-0.5*(t^2))
round(mbf,4)
}
if (method=="edwards"){
# Edwards et al. (1963)
if (abs(t)>1) mbf= abs(t)*exp((-t^2)/2)*sqrt(exp(1))
if (abs(t)<1) mbf=1
round(mbf,4)
}
if (method=="sellke1"){
#Sellke et al. (2001),
p=Pval(t)+0.000000000000000000000000000000000000000001
if (p<(1/exp(1))) mbf=-exp(1)*(p*log(p))
if (p>(1/exp(1)))  mbf=1
mbf

}
if (method=="sellke2"){
#Sellke et al. (2001),
p=Pval(t)+0.000000000000000000000000000000000000000001
q=1-p
if (p<(1-(1/exp(1)))) mbf= -exp(1)*(q*log(q))
if (p>(1-(1/exp(1)))) mbf=1
mbf
}
#H0=beta=0
#H1=beta not equal zero


#MBF2=function(t){
#t=(mean(x)-mu)/(sd(x)/sqrt(length(x)))
#pv=Pval(t)
#P0=dnorm(t)+0.00000000001
#P1=dnorm(0)+0.00000000001
#H=list(H01=(P0/P1),H10=(P1/P0),BFratio01=1/(P0/P1),BFratio10=1/(P1/P0),prob=pv)
#H
#}
names(mbf)=method
mbf
}

#example
#t=2  # t-statistic
#MBF(t,method="goodman")
