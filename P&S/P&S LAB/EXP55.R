#q1
punif(45, min = 0, max = 69, lower.tail = FALSE)

1-punif(45, min-0, max=60)
punif(15, min = 0, max = 60)


#(b) Waiting time between 20 and 30 min
#F(30)-F(20)
#P(x<=30)-P(x<=20)
punif(30, min=0, max=60) - punif(20, min = 0, max = 60)

#2
#(a)
dexp(3, rate = 1/2)

#(b)
x<- seq(0,5, by=0.02)
px<-dexp(x,rate=1/2)
plot(x,px,xlab="x", ylab="f(x)", main="pdf of exponential distribution at lambda=1/2")

#(c)
#F(3)
#P(X<=3)
c2= pexp(3,rate=0.5)
print(c2)

#(d)
Fx<-pexp(x,rate=1/2)
plot(x,Fx,xlab="x", ylab="F(x)", main="cdf of exponential distribution at lambda=1/2")

#(e)
n<-1000
x_sim<-rexp(n,rate=1/2)
#rexp is used to generate random sample of exp dist
plot(density(x_sim), xlab="simulated x", ylab="density", main="simulated data for exp dist at lambda=1/2")
hist(x_sim, probability=TRUE, xlab="simulated x", ylab="density", main="simulated data for exp dist at lambda=1/2")


#Q3
#(a)
alpha<-2
beta<-1/3
a3_i<- dgamma(3,shape=alpha, scale=beta)
print(a3_i)
a3_ii<-pgamma(1,shape=alpha, scale=beta, lower.tail=FALSE)
print(a3_ii)

#(b)
#the pth quantile is type smallest value of gamma random variable x such that P(x<=x)>=p
#we need to find smallest value of c such that P(x<=c)>=0.7, so p=0.7
#here we use quantile function gamma
prob<-0.7
b3<-qgamma(0.7, shape=alpha, scale=beta)
print(b3)

