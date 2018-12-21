#Trapezoidal Rule
f<- function(x)
{
  1/(1+x*x)
}
n=6
h=1/6
x<-c(0,1/6,2/6,3/6,4/6,5/6,1)
y<-c(0,0,0,0,0,0,0)
for(i in 1:n+1)
{
  y[i]=f(x[i])
}
print(x)
print(y)

sum=y[1]+y[n+1]
for(i in 2:n)
{
  sum=sum+2*y[i]
}
sum=sum*h/2
print(sum)

#Simpson Rule
x<-c(0,0.5,1.0,1.5,2.0)
y<-c(0.3989,0.3521,0.2420,0.1296,0.0540)
h=x[2]-x[1]
sum=y[1]+y[5]
for(i in 2:4)
{
  if(i%%2!=0)
    sum=sum+2*y[i]
  else
    sum=sum+4*y[i]
}
sum=sum*h/3
print(sum)