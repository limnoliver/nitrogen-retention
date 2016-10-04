
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

par(mfrow=c(1,1))

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

    row<-j+length(Vf)*(i-1)
    
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

# ============================================
# Same model but let Vf vary and be the x-axis
# ============================================

# Set parameter estimates; #Years (Lake Pepin and Pool 8, examples)
z = c(1.7, 4.5)
tau = c(9/365, 1.7/365) 

vfrange<-c(-1, 3) #log scale

# plot curve
pdf("N_retention_vf.pdf")
par(cex=1)

table<-as.data.frame(matrix(ncol=4, nrow=length(tau)*length(z)) )
names(table)<-c("tau", "z", "color", "lty")

colors<-blue2green2red(n=length(z))
lty<-seq(1, length(tau), 1)

for (i in 1:(length(z))){
  for (j in 1:(length(tau))){
    z1 = z[i]
    tau1 = tau[j]
    
    row<-j+length(tau)*(i-1)
    
    table[row,1]<- tau1
    table[row,2]<- z1
    table[row,3]<- colors[i]
    table[row,4]<- lty[j]
    
    
    if (i ==1 & j==1){
      # empty plot containing labels. Plot this first so vertical lines appear behind data
      curve(1-(exp(((-1)*tau1*x)/z1)), 10^vfrange[1],10^vfrange[2], ylab="N Retention", log = "x", xlab = expression(paste(V[f], " (m year"^"-1", ")")), type="n", las=1, xaxt="n")
      # add vertical lines to denote day, month, year, etc
      abline(v=10^seq(vfrange[1],vfrange[2], by=1), col="darkgrey", lty=1, lwd=0.5)
      axis(1, at=10^seq(vfrange[1],vfrange[2], by=1), labels=10^seq(vfrange[1], vfrange[2], by=1))

    }
    # Plot all curves
    curve(1-(exp(((-1)*tau1*x)/z1)), 10^(vfrange[1]-1),10^(vfrange[2]+1), log = "x", col = colors[i], lty=lty[j], lwd=2, add=T)
  }
}

legend("topleft", inset=0.02, lwd = 2,legend = paste( "z=",  table[,2], "m;   WRT=" ,round(table[,1]*365), " d"), col = table[,3], lty=table[,4])

box(which='plot')

dev.off()

