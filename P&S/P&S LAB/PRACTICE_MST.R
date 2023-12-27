##EXP5
#1
punif(45,min=0,max=60,lower.tail=FALSE)
punif(30,min=0,max=60)-punif(20,min=0,max=60)
#2
#a
dexp(3,rate=1/2)
#b
px<-dexp(seq(0,5,by=0.02),rate=1/2)
plot(seq(0,5,by=0.02),px)
#c
pexp(3,rate=1/2)
#d
px<-pexp(seq(0,5,by=0.02),rate=1/2)
plot(seq(0,5,by=0.02),px)
#e
n<-1000
rx<-rexp(n,rate=1/2)
plot(density(rx))
hist(rx, probability = TRUE)

#3
#a
pgamma(1,shape=2,scale=1/3,lower.tail = FALSE)
#b
qgamma(0.70,shape=2,scale=1/3)

##EXP6
#1
#i
f=function(x,y){2*(2*x+3*y)/5}
integral2(f,0,1,0,1)$Q
#ii
gx=function(y){f(1,y)}
integral(gx,0,1)
#iii
hy=function(x){f(x,0)}
integral(hy,0,1)
#iv
gxy=function(x,y){x*y*f(x,y)}
integral2(gxy,0,1,0,1)$Q

#2
f=function(x,y){(x+y)/30}
#i
M1<-matrix(c(f(0,0:2),f(1,0:2),f(2,0:2),f(3,0:2)),nrow =4,ncol = 3, byrow = TRUE)
#ii
sum(M1)
#III
hx<-apply(M1,1,sum)
#iv
hy<-apply(M1, 2, sum)
#v
M1[1,2]/hy[2]
#vi
x<-seq(0,3,by=1)
y<-seq(0,2,by=1)

Ex<-sum(x*hx)
print(Ex)

Ey<-sum(y*hy)
print(Ey)

gxy=function(x,y){x*y*(x+y)/30}
M2=matrix(c(gxy(0,0:2),gxy(1,0:2),gxy(2,0:2),gxy(3,0:2)),nrow=4,ncol=3,byrow=TRUE)
print(M2)

Exy<-sum(M2)
print(Exy)

varx<-sum(x^2*hx)-Ex^2
print(varx)

vary<-sum(y^2*hy)-Ey^2
print(vary)

covxy=Exy-Ex*Ey
print(covxy)

r=covxy/sqrt(varx*vary)
print(r)

#3
#1
sample<-rt(100,n-1)
hist(sample)
#2
cs1<-rchisq(100,2)
cs2<-rchisq(100,10)
cs3<-rchisq(100,25)
hist(cs1)
hist(cs2)
hist(cs3)
#3
x<-seq(-6,6,length.out=100)
df<-c(1,4,10,30)
t1<-dt(x,df[1])
t2<-dt(x,df[2])
t3<-dt(x,df[3])
t4<-dt(x,df[4])
plot(x,t1,col="blue",type="l")
lines(x,t2,col="red")
lines(x,t3,col="green")
lines(x,t4,col="purple")

#4
#i
qf(0.95,10,20)
#ii
pf(1.5,10,20,lower.tail = FALSE)
1-pf(1.5,10,20,lower.tail = FALSE)
#iii
