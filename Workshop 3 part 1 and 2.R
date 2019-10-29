#part 1 of workshop 3
rm(list=ls())  # Clears workspace
D<-10^-6         # Diffusion coefficient (in m^2/s)
dt<-1e-4         # Time step (in seconds)
total_time<-100  # Total time that simulation will run (in seconds)

dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)

dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.

n<-total_time/dt #n represents 100/1e-4 which results in 1,000,000 steps in 100 seconds
for(i in 1:n)
{
  distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
  disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
  
  anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
  angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
  
  dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
  dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
  
  
}

library(ggplot2)
p<-data.frame(dots_x,dots_y) #convert it to a data frame
png(filename = "Difussionplot1.png", width = 7, height=7)
ggplot(p,aes(x=dots_x,y=dots_y)) + geom_point()
 #ggplot scatterplot 
dev.off() 


 
#part 2?                      
   diffusion <- function(x,y) {
   d=sqrt(sqrt((x[2]-x[1])^2 + (y[2]-y[1])^2))
 }
 x <-c(10^-4,10^-5,10^-7,10^-8,10^-9)
 y <-c(10^-3, 10^-4, 10^-2,10^-1)
                       

 