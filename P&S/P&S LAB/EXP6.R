
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
  
  