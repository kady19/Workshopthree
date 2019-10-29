rm(list=ls())  # Clears workspace
DiffusionModel<- fucntion(DiffExp, TimeExp,total_time, Molecules=100, Track=FALSE) {
  
D<-10^-6         # Diffusion coefficient (in m^2/s)
dt<-10^2         # Time step (in seconds)
total_time<-10  # Total time that simulation will run (in seconds)
TimeSteps <- total_time/dt #caculates number of steps given dt and total time

dots_x<-rep(0,Molecules)   # Creates 100 molecules to follow (x component of position)
dots_y<-rep(0,Molecules)   # Creates 100 molecules to follow (y component of position)

dots_x_tracking <- dots_x
dots_y_tracking <- dots_y

dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.

for(i in 1:TimeSteps){
distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line

anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line

dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)

dots_x_tracking <- cbind(dots_x_tracking, dots_x)
dots_y_tracking <- cbind(dots_y_tracking, dots_y)
}
if(Track==TRUE) {
plot(dots_x,dots_y, main="Post-Diffusion Coordinates of Molecules Starting at the Origin",
     xlab="Final X Coordinate", ylab="Final Y Coordinate")  # Plots the positions of the molecules
  for(i in 1:Molecules) {
    for(j in 1:Timesteps) {
      segments(dots_x_tracking[i,j], dots_y_tracking[i,j], dots_x_tracking[i,j+1], dots_y_tracking[i,j+1])
    }
  }
} else{
  library(ggplot2)
  png(filename = "Difussionplot4.png", width = 7, height = 7)
  qplot(dots_x, dots_y, main = "Post-Difussion Coordinates of Molecules Starting at the Origin", 
        xlab = "FInal X Coordinate", ylab="Final Y Coordinate") #plots the position of the molecules
  dev.off()
  }}
 DiffusionModel(-6,-2,10,100, Track= TRUE)
  #I got help from Krista for this code
  