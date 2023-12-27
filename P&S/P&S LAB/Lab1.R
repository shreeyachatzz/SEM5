1+3
2/9
4/99

#variables
a=9+8
a=2+1

#assignment operators
a=4+9#first variable then value
a<-4#first variable then value (most often used)
9->a#first value then variable

#data type: numeric, character, logical, complex
a<-6
class(a)#displays the data type
b<-"she"
class(b)
c1<-TRUE
C2<-FALSE
class(c1)
e=2-3i#only this format is VALID no other format is VALID
class(e)

#DATA STRUCTURES
#Vectors- (indexing from 1)
v1<-c(1,2,3,4,5)
v2<-c(1,'a','b',TRUE)#c for combine/compile
#extract
v1[2]
v2[2:5]
max(v1)
min(v1)
max(v2)
min(v2)
factorial(3)
#PLOTTING
#xlab:label X Axis, ylab:for YAxis Labelling
plot(4,5,xlab="x",ylab="y",)
##inputting:
n=as.integer(readline("number"))
n=as.integer(readline(prompt("number")))

#ASS1

##Q1
v1<-c(5,10,15,20,25,30)
max(v1)
min(v1)

##Q2
factorial_result <- 1
n <- as.integer(readline(prompt = "Enter integer: "))

if (n <= 0) {
  print('Error')
} else {
  for (i in 1:n) {
    factorial_result <- factorial_result * i
  }
  print(paste("Factorial of", n, "is", factorial_result))
}

