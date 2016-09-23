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
Vf = c(-13.66, -9.92, -5.66) 
z = c(2, 5,10)

# plot curve
pdf("N_retention.pdf")
par(cex=1)

table<-as.data.frame(matrix(ncol=2, nrow=length(Vf)*length(z)) )
names(table)<-c("Vf", "z")

colors<-rainbow(n=nrow(table), start = 0.1, end = 0.9)


for (i in 1:(length(Vf))){
  for (j in 1:(length(z))){
    Vf1 = Vf[i]
    z1 = z[j]
    row<-j+length(z)*(i-1)
    
    table[row,1]<- Vf1
    table[row,2]<- z1
    
    if (i ==1 & j==1){
    curve(1-(exp((Vf1*x)/z1)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = colors[row])
    } else {
    curve(1-(exp((Vf1*x)/z1)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = colors[row], add=T)
    }
# 
# curve(1-(exp((Vf*x)/z)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = "blue", add = TRUE)
# curve(1-(exp((Vf*x)/z)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", add = TRUE)

  }
}

legend("bottomright", inset=0.02, lty = 1,lwd = 2,legend = paste("Vf=" ,table[,1], "  z=",  table[,2]), col = colors)

#add vertical lines to denote day, month, year, etc
# day
abline(v=1/365, col="red", lty=2)
mtext("Day", side=3, outer=FALSE, at = 1/365, col="red", cex=1)
# week
abline(v=7/365, col="red", lty=2)
mtext("Week", side=3, outer=FALSE, at = 7/365, col="red",cex=1)
# month
abline(v=30/365, col="red", lty=2)
mtext("Month", side=3, outer=FALSE, at = 30/365, col="red",cex=1)
# year
abline(v=1, col = "red", lty = 2)
mtext("Year", side=3, outer=FALSE, at = 1, col="red",cex=1)
dev.off()

