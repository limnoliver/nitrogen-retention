#########################
# N retention model
# From Verburg et al 2013
# Equation originally from Harrison et al 2009
# Rn=1-exp((-9.92tau)/z)
# Rn = proportion of N retained
# tau = residence time (years)
# z = mean depth (m)

# plot curve
pdf("N_retention.pdf")
par(cex=3)
curve(1-(exp((-9.92*x)/5)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = "lightblue")
curve(1-(exp((-9.92*x)/10)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", col = "blue", add = TRUE)
curve(1-(exp((-9.92*x)/20)), .001, 1000, log = "x", ylab="N Retention", xlab = "Residence Time (y)", add = TRUE)
legend("topleft", lty = 1,lwd = 2,legend = c("5 meters", "10 meters", "20 meters"), col = c("lightblue", "blue", "black"))


dev.off()

