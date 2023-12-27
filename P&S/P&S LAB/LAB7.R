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
