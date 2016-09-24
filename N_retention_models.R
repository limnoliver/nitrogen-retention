
library(colorRamps)
#########################
# N retention model
# From Verburg et al 2013
# Equation originally from Harrison et al 2009
# Rn=1-exp((-9.92tau)/z)
# Rn = proportion of N retained
# tau = residence time (years)
# z = mean depth (m)
# Vf = settling velocity (m year-1)

# Set parameter estimates
z = c(2, 5,10)
Vf = c(-13.66, -9.92, -5.66) 

# plot curve
pdf("N_retention.pdf")
par(cex=1)

table<-as.data.frame(matrix(ncol=4, nrow=length(Vf)*length(z)) )
names(table)<-c("Vf", "z", "color", "lty")

colors<-blue2green2red(n=length(z))
lty<-seq(1, length(Vf), 1)

for (i in 1:(length(z))){
  for (j in 1:(length(Vf))){
    z1 = z[i]
    Vf1 = Vf[j]

    row<-j+length(z)*(i-1)
    
    table[row,1]<- Vf1
    table[row,2]<- z1
    table[row,3]<- colors[i]
    table[row,4]<- lty[j]
    
    
    if (i ==1 & j==1){
      # empty plot containing labels. Plot this first so vertical lines appear behind data
      curve(1-(exp((Vf1*x)/z1)), .001, 100, log = "x", ylab="N Retention", xlab = "Residence Time (y)", type="n", las=1)
      # add vertical lines to denote day, month, year, etc
      abline(v=c(1,7,30,365)/365, col="darkgrey", lty=1, lwd=0.5)
      mtext(c("Day", "Week", "Month", "Year"), side=3, outer=FALSE, at = c(1,7,30,365)/365, col="darkgrey", cex=1)
      }
    # Plot all curves
    curve(1-(exp((Vf1*x)/z1)), .001, 100, log = "x", col = colors[i], lty=lty[j], lwd=2, add=T)
  }
}

legend("bottomright", inset=0.02, lwd = 2,legend = paste( "z=",  table[,2], "  Vf=" ,table[,1]), col = table[,3], lty=table[,4])

box(which='plot')

dev.off()

