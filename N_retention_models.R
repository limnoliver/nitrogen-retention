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
Vf = (-9.92) 
z = c(5,10,20)

# plot curve
pdf("N_retention.pdf")
par(cex=1)
curve(1-(exp((Vf*x)/z[1])), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = "lightblue")
curve(1-(exp((Vf*x)/z[2])), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = "blue", add = TRUE)
curve(1-(exp((Vf*x)/z[3])), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", add = TRUE)
legend("topleft", lty = 1,lwd = 2,legend = paste(z, " m"), col = c("lightblue", "blue", "black"))

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

