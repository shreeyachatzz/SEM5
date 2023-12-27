cond_prob<-function(a,b,agivenb){
  bgivena<-(agivenb*b)/a
  return(bgivena)
}
print(cond_prob(0.4,0.2,0.85))

mode_calc<-function(n){
  mod <- numeric(max(n) - min(n)+ 1)
  for(i in 1:length(n)){
    mod[n[i]]<-mod[n[i]]+1
  }
  return(which.max(mod))
}

vec<-c(1,2,1,2,3,3,3,4,5,334,223,334,223,223,334,334,334)
print(mode_calc(vec))


### LAB 4 ###
#Q1
x<-c(0,1,2,3,4)
px<-c(0.41,0.37,0.16,0.05,0.01)

avg_num<-sum(x*px)
print(avg_num)

weighted_mean<-weighted.mean(x,px)
print(weighted_mean)

#Q2
pdf<-function(x){
  x*0.1*exp(-0.1*x)
}

exp_val<-integrate(pdf,lower=0,upper=Inf)$value
print(exp_val)

#Q3
#Y=(12X+2(3-X)-6*3)=10X-12
costfun<-function(x){
  10*x-12
}
x<-c(0,1,2,3)
px<-c(0.1,0.2,0.2,0.5)

exp_X<-weighted.mean(x,px)
exp_y<-10*exp_X-12
print(exp_y)


#Q4
pdf1<-function(x){
  x*0.5*exp(-abs(x))
}

pdf2<-function(x){
 (x^2)*0.5*exp(-abs(x))
}

moment1<-integrate(pdf1,lower=1,upper=10)
moment2<-integrate(pdf2,lower=1,upper=10)

print(moment1$value)
print(moment2$value)

cat("Mean",moment1$value)
cat("Variance: ",moment2$value-(moment1$value)^2)

#Q5
pdf<-function(y){
  (3/4)*((1/4)^(sqrt(y)-1))
}
f<-function(y){
  y*(3/4)*((1/4)^(sqrt(y)-1))
}

cat("Prob of Y when X=3: ", pdf(9))

x<-c(1,2,3,4,5)
y<-x^2
prob_y<-pdf(y)
print(prob_y)

exp_y<-integrate(f,1,5)
print(exp_y$value)

expval<-sum(y*prob_y)
print(expval)



