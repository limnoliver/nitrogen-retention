#WRT distribution #days
WRT<-matrix(ncol=3, nrow=10000)
#Channel Only
WRT[,1]<-rnorm(10000, sd=0.01, mean=1)
hist(WRT[,1])
abline(v=mean(WRT[,1]), lwd=3)

#Lake Pepin Only
WRT[,2]<-rnorm(10000, sd=1, mean=10)
hist(WRT[,2])
abline(v=mean(WRT[,2]), lwd=3)

#Pool 8 example
WRT[,3]<-rchisq(10000, df=4)/2 
hist(WRT[,3])
abline(v=mean(WRT[,3]), lwd=3)

#vf (m yr-1) distribution independent of WRT for x=1,2; but dependent on x=3
vf<-WRT
vf[,]<-NA
vf[,1]<-rnorm(10000, sd=0.1, mean=10)
vf[,2]<-rnorm(10000, sd=0.1, mean=10)
vf[,3]<-WRT[,3]*2
# vf[,3]<-rnorm(10000, sd=0.1, mean=10)

#z (m) distribution dependent of WRT for x=1,2; but independent on x=1
z<-WRT
z[,]<-NA
z[,1]<-rnorm(10000, sd=0.05, mean=3)
z[,2]<-rnorm(10000, sd=0.05, mean=6)
z[,3]<-(3.5-WRT[,3])
z[which(z[,3]<=0.1),3]<-runif(length(z[which(z[,3]<=0.1),3]), 0.001, 1.5)

hist(z[,3])
abline(v=mean(z[,3]), lwd=3)


exponent<-WRT/365*vf/z*(-1)

R<-1-exp(exponent)
hist(R[,1])
hist(R[,2])
hist(R[,3])

summary(R[,1])
summary(R[,2])
summary(R[,3])

list<-list(WRT, z, vf, R)
names<-c("Channel", "Lake Pepin", "Pool 8")
matrix_names<-c("WRT (d)", "Depth (m)", "Vf (m yr-1)", "R")
matrix=1
example=1
for (matrix in 1:4){
  for (example in 1:ncol(WRT)){
    if (matrix==1 & example==1){
      png("N_Model/NModel2.png", width=11, height=8.5,res=200, units="in")
      par(mfcol=c(ncol(WRT),4))
      par(oma=c(2,2,0,2))
      par(mar=c(3,3,0.5,0.5))
    }

   
     if(matrix==4){
        # hist(list[[matrix]][,example], main="", xlab="", ylab="", xaxs="i", col="grey")
        c<-list[[matrix]][,example]
        d<-density(list[[matrix]][,example])
        plot(d, main="", xlab="", ylab="", xaxs="i", yaxs='i',col="black")
        polygon(d, col="grey", border="black")
        box(which='plot')
        mtext(names[example], 4, 1)
        legend("topright", inset=0.02, bty="n", paste("R=", round(mean(c), 3), " +/- ", round(sd(c),3)))}
    
    else {
      # hist(list[[matrix]][,example], main="", xlab="", ylab="", xaxs="i", col="grey")
      d<-density(list[[matrix]][,example])
      plot(d, main="", xlab="", ylab="", xaxs="i", yaxs='i',col="black")
      polygon(d, col="grey", border="black")
      box(which='plot')
    }
    if (example==ncol(WRT)){
      mtext(matrix_names[matrix], 1, 3)
    }
  }

}
mtext("Density", 2, 0, outer=T)
dev.off()
