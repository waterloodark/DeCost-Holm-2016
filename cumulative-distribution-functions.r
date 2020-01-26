plot(function(x){
  return(plnorm(x,meanlog=(log(0.1)-0.5^2/2),sdlog=0.5))
},xlim=c(0,0.6))
0.1 = exp(mulog+sdlog^2/2)
log(0.1) = mulog + sdlog^2/2
mulog = log(0.1) - sdlog^2/2
log(0.1)-0.5^2/2

plot(function(x){
  return(plnorm(x,meanlog=(log(0.1)-0.6^2/2),sdlog=0.6))
},xlim=c(0,0.6),add=TRUE)

plot(function(x){
  return(pnorm(x,mean=0.1,sd=0.025))
},xlim=c(0,0.6),add=TRUE)

plot(function(x){
  return(plnorm(x,meanlog=(log(0.1)-0.25^2/2),sdlog=0.25))
},xlim=c(0,0.6),add=TRUE)

plot(function(x){
  return(plnorm(x,meanlog=(log(0.12)-0.5^2/2),sdlog=0.5))
},xlim=c(0,0.6),add=TRUE)

plot(function(x){
  return(plnorm(x,meanlog=(log(0.12)-0.6^2/2),sdlog=0.6))
},xlim=c(0,0.6),add=TRUE)

mu.1 = 0.12
mu.2 = exp(2*(log(0.12)-0.5^2/2)+0.5^2)*(exp(0.5^2)-1) + mu.1^2
mu.1^2/(mu.2)
nlminb(1, function(x) {return((mu.1^2*gamma(1+2/x)-mu.2*gamma(1+1/x)^2)^2)})
plot(function(x) {return(((mu.1^2*gamma(1+2/x)-mu.2*gamma(1+1/x)^2)^2))},
     xlim=c(1,4))
m = 1.957465
eta = 0.12/gamma(1+1/m)
plot(function(x){
  return(pweibull(x,shape=m,scale=eta))
},xlim=c(0,0.6),add=TRUE)

mu.1 = 0.12
mu.2 = exp(2*(log(0.12)-0.6^2/2)+0.6^2)*(exp(0.6^2)-1) + mu.1^2
mu.1^2/(mu.2)
nlminb(1, function(x) {return((mu.1^2*gamma(1+2/x)-mu.2*gamma(1+1/x)^2)^2)})
plot(function(x) {return(((mu.1^2*gamma(1+2/x)-mu.2*gamma(1+1/x)^2)^2))},
     xlim=c(1,4))
m = 1.551332
eta = 0.12/gamma(1+1/m)
plot(function(x){
  return(pweibull(x,shape=m,scale=eta))
},xlim=c(0,0.6),add=TRUE)
