#Q1
r<- 1- punif(45,min=0, max=60)
print(r)

r<- punif(45,min=0, max=60,lower.tail=FALSE)
print(r)

r<- punif(15,min=0, max=60)
print(r)

#waiting time between 20 and 30 min
#F(30)-F(20)
#P(X<=30)-P(X<=20)
punif(30,min=0, max=60)- punif(20,min=0, max=60)


#Q3
##For continuous range of points: pexp
## for a specific point: dexp

##in our question labda=1/2

##(a)
    P3 <- dexp(3, rate=1/2)
    P3
##(b) 
    x <- seq(0, 5, 0.2)
    px<-dexp(x,rate=0.5)
    plot(x,px, xlab="x", ylab="y", main="pdf of exp dist at lambda=0.5")
##(c) ## to find x<=3 we keep lower.tail=TRUE
    c2<-pexp(3,rate=0.5)
    print( c2)
    ##since we need a continuous value we use pexp'
##(d)
    Fx<- pexp(x,rate=0.5)##calculating the values of quantile for each value of c2
    plot(x,Fx, xlab="x", ylab="y", main="Plot")
##(e) 
    n<-1000
    x_sim<-rexp(n,rate=0.5)
    ## rexp iS used yo generate a random sample for exp distribution
    plot(density(x_sim),xlab="x", ylab="density", main="simulated data from exp distribution at lambda=0.5")
    hist(x_sim,probability = TRUE,xlab="x", ylab="density", main="simulated data from exp distribution at lambda=0.5")
    

#Q3
    alpha<-2
    beta<-1/3
    #a
  a3_i<-dgamma(3, shape=alpha, scale=beta)
  print(a3_i)
  a3_ii<-pgamma(1, shape=alpha, scale=beta, lower.tail=FALSE)
  print(a3_ii)
  #b we use qgamma when we need to find something wrt a probability
  #the pth quantile is the smallest value of Gamma random variable X such that P(X<=x)>=p.
  #we need to find the smallest value of c such that P(X<=c)>=0.70, so P=0.70
  #here we use quantile function gamma qgamma
  prob<-0.70
  b3<-qgamma(0.70, shape=alpha, scale=beta)
  print(b3)
    