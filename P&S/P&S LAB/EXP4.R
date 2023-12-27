
#Q1:

#Using weighted.mean()
x <- c(0, 1, 2, 3, 4)
p_x <- c(0.41, 0.37, 0.16, 0.05, 0.01)
mean_imperf <- weighted.mean(x, p_x)
cat("The average number of imperfections per 10 meters of fabric is:", mean_imperf)

#Using sum()
x <- c(0, 1, 2, 3, 4)
p_x <- c(0.41, 0.37, 0.16, 0.05, 0.01)
mean_imperf <- sum(x * p_x)
cat("The average number of imperfections per 10 meters of fabric is:", mean_imperf)


#Q2.
pdf <- function(t){
  t*0.1*exp(-0.1*t)
}

expected_value <- integrate(pdf, lower = 0, upper = Inf)$value
cat("The expected value of T is:", expected_value)

#Q3.
#Y = (12X+(3-X)2 - (6*3)) = (10X-12)
x<-c(0,1,2,3)
probab<-c(0.1,0.2,0.2,0.5)
print(weighted.mean(x,probab))
expval<-(10*weighted.mean(x,probab))-12
print(expval)

cat("The expected value of Y (net revenue) is:", expval)

#or

x<-c(0,1,2,3)
probabx<-c(0.1,0.2,0.2,0.5)
y<-10*x-12
probaby<-probabx
expval<-sum(y*probaby)
cat("The expected value of Y (net revenue) is:", expval)


#Q4.

f1<-function(x){
  x*0.5*exp(-abs(x))
}

f2<-function(x){
  x^2*0.5*exp(-abs(x))
}

moment1<-integrate(f1,1,10)
moment2<-integrate(f2,1,10)

print(moment1$value)
print(moment2$value)

meanval<-moment1$value
print(meanval)

f3<-function(m1,m2){
  return (m2-(m1^2))
}
print(meanval)

varval<-f3(moment1$value,moment2$value)
print(varval)



#Q5.
yf<-function(y){
  (3/4)*(1/4)^(sqrt(y)-1)
}
x<-as.integer(readline(prompt="Enter the value of x"))
y=x^2
proby<-yf(y)
print(proby)

x<-c(1,2,3,4,5)
y<-x^2
proby<-yf(y)
print(proby)

expval<-sum(y*proby)
print(expval)

m<-expval
y1<-(y-m)^2
proby1<-yf(y1)
print(proby1)
varval<-sum(y1*proby1)
print(varval)









