##EXP-5
#q1
punif(45, min = 0, max = 60, lower.tail = FALSE)

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

##EXP6

library('pracma')
install.packages('pracma')

#q1
##(i) to check JPDF or not
f=function(x,y){2*(2*x+3*y)/5}
I=integral2(f,xmin=0,xmax=1,ymin=0,ymax=1)
print(I$Q)

##(ii) to find marginal distribution
gx_1= function(y){f(1,y)}
gx1= integral(gx_1,0,1)
print(gx1)

##(iii) find marginal of y at 0 for h(y)
hy_0= function(x){f(x,0)}
hy0= integral(hy_0,0,1)
print(hy0)

##(iv) find the expected walue of g(x,y)=xy
f_xy=function(x,y){x*y*f(x,y)}
E_xy= integral2(f_xy,0,1,0,1)
print(E_xy$Q)

#Q2 JPMF is given

##(i)displaying the JPMF in a rectangluar form
f=function(x,y){(x+y)/30}
x=c(0:3)
y=c(0:2)
M1= matrix(c(f(0,0:2),f(1,0:2),f(2,0:2),f(3,0:2)), nrow=4,ncol=3,byrow=TRUE)
##if we do by column then we have to make bycol=TRUE and the matrix would be written as f(0:3,0),f(0:3,1) 
##make sure you correlate with the function and the pmf that you make on paper and try to replicate that table 
##in this code matrix that you are generating
print(M1)
##(ii) checking Joint Mass Function
sum(M1)
##(iii) finding the marginal distribution g(x) at x=0,1,2,3
gx=apply(M1,1,sum)
cat("The marginal probabilities are")
print((gx))
print(sum(gx))
##(iv) finding the marginal distribution h(y) at y=0,1,2
hy=apply(M1,2,sum)
cat("The marginal probabilities are")
print((hy))
print(sum(hy))
##(v) find the conditional probability at x = 0 given y = 1.
p_x0_y1=M1[1,2]/hy[2]
print(p_x0_y1)
##(vi) find E(x), E(y), E(xy), V ar(x), V ar(y), Cov(x, y) and its correlation coefficient.
#expectation of x
E_x= sum(x*gx)
print(E_x)
#expectation of y
E_y=sum(y*hy)
print(E_y)
#variance of x and y
E_x2=sum(x^2*gx)
E_y2= sum(y^2*hy)
print(E_x2)
print(E_y2)
Var_X= E_x2-(E_x)^2
print(Var_X)
Var_Y= E_y2-(E_y)^2
print(Var_Y)
#expectation of xy
x=c(0:3)
y=c(0:2)
f1=function(x,y){x*y*(x+y)/30}
M2= matrix(c(f1(0,0:2),f1(1,0:2),f1(2,0:2),f1(3,0:2)),nrow=4,ncol = 3, byrow=TRUE)
print(M2)
#expectation is nothing but the sum of all the eleemtns in the matrix that was 
#just generated
E_xy=(sum(M2))
print(sum(M2))
#Covariance of x,y
Cov_xy= E_xy - E_x*E_y
print(Cov_xy)
#R
r_xy=Cov_xy/sqrt(Var_X*Var_Y)
print(r_xy)


##EXP7
#Q1
#set the parameters
n<-100
df <- n-1
#Generate random samples from the t-distribution
t_samples<- rt(n,df)
print(t_samples)
#Plot a histogram of the generated data
hist(t_samples, main="t-Distribution Histogram", xlab="Value", ylab="Frequency", col="lightpink", border="black")

#Q2
#Set the parameters
n<-100
dfs<-c(2,10,25)#degrees of freedom

s1=rchisq(n,dfs[1])
s2=rchisq(n,dfs[2])
s3=rchisq(n,dfs[3])

mean(s1)
var(s1)

mean(s2)
var(s2)

mean(s3)
var(s3)
#Generate random samples from the chi-square distribution for each df
#chi_squared_samples<-lapply(dfs, function(df) rchisq(n,df))

#create the histograms for each set of samples
par(mfrow=c(1,3))#Arrange plots in a row

hist(s1, main = "Chi-Square (df = 2)", xlab = "Value", ylab = "Frequency", col = "lightblue")
hist(s2, main = "Chi-Square (df = 10)", xlab = "Value", ylab = "Frequency", col = "lightgreen")
hist(s3, main = "Chi-Square (df = 25)", xlab = "Value", ylab = "Frequency", col = "lightpink")


#Q3
#To generate a vector of 100 values between -6 and 6
x<-seq(-6,6,length.out=100)
#Degrees of freedom
df<-c(1,4,10,30)
colors<-c("blue","pink","orange","green")
#Calculate the t-distribution values
t_dist_df1<-dt(x,df[1])
t_dist_df2<-dt(x,df[2])
t_dist_df3<-dt(x,df[3])
t_dist_df4<-dt(x,df[4])
#Plot the comaprision of t-distributions
plot(x, t_dist_df4, type="l",xlab="t values", ylab="density", main="Comparision of t-distributions", col= colors[4])

#Add lines for other degrees of freedom
lines(x,t_dist_df1, col=colors[1])
lines(x,t_dist_df2, col=colors[2])
lines(x,t_dist_df3, col=colors[3])

#Add a legend to the plot
legend("topright", legend=c("df=1","df=4","df=10","df=30"))


#Q4
v1<-10
v2<-20

#(i) find the 95th percentile of the f-distribution

percentile_95<-qf(0.95,df1=v1,df2=v2)
cat("95th percentile of the F-distribution :",percentile_95,"\n")

#(ii)calculate the area under the curve for the given intervals
area_interval_1<-pf(1.5,df1 =v1,df2=v2,lower.tail = TRUE) #[0,1.5]
area_interval_2<-1-area_interval_1
cat("Area under the curve for [0,1.5]:",area_interval_1,"\n")
cat("Area under the curve for [1.5,+inf]:",area_interval_2,"\n")

#(iii) calculate the quantiles for diff probab
quantile_25<-qf(0.25,df1=v1,df2=v2)
quantile_50<-qf(0.5,df1=v1,df2=v2)
quantile_75<-qf(0.75,df1=v1,df2=v2)
quantile_999<-qf(0.999,df1=v1,df2=v2)

cat("Quantile for p = 0.25",quantile_25,"\n")
cat("Quantile for p = 0.25",quantile_50,"\n")
cat("Quantile for p = 0.75",quantile_75,"\n")
cat("Quantile for p = 0.999",quantile_999,"\n")

#(iv) generate random values and plot a histogram
#set.seed(123) for reproducability

x<-rf(1000,df1=v1,df2=v2)
hist(x,breaks='scott',freq=FALSE,xlim=c(0,3),ylim=c(0,1),xlab="", col="orange")

##EXPERIMENT 8

# (a) Import the csv data file in R
library(readr)
Clt_data <- read_csv("C:/Users/shree/Downloads/Clt-data.csv")
View(Clt_data)

# (b) Validate data for correctness
# Count number of rows
n_rows <- nrow(Clt_data)
print(paste("Number of rows: ", n_rows))
# View the top ten rows of the dataset
head(Clt_data, 10)
# Data about the column names
colnames(Clt_data)
str(Clt_data)


# (c) Calculate the population mean
population_mean <- mean(Clt_data$`Wall Thickness`)
print(population_mean)

# Plot the histogram of the observations
hist(Clt_data$`Wall Thickness`, breaks = 20, col = "lightblue", xlab = "Wall Thickness", main = "Histogram of Wall Thickness")
# Add a vertical line for population mean
abline(v = population_mean, col = "red", lwd = 2)

# Function to draw sufficient samples and plot histograms
draw_samples_and_plot <- function(sample_size) {
  # Generate 1000 samples of specified size
  sample_means <- replicate(1000, mean(sample(Clt_data$'Wall Thickness', size = sample_size, replace = TRUE)))
  
  # Plot histogram of sample means
  hist(sample_means, breaks = 30, col = "lightgreen", xlab = "Sample Means", main = paste("Sampling Distribution (Sample Size =", sample_size, ")"))
  
  # Add a vertical line for the population mean
  abline(v = population_mean, col = "red", lwd = 2)
}

# (a) Draw samples of size 10 and plot their means
draw_samples_and_plot(10)

# (b) Repeat for sample sizes 50, 500, and 9000
draw_samples_and_plot(50)
draw_samples_and_plot(500)
draw_samples_and_plot(9000)

