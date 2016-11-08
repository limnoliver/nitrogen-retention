
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


# =============================
# Model for WRR publication
# Different from above in that it only plots one curve for each Pool, rather than looping through all possable z and tau
# =============================
Pooldata<-readRDS(file = "UMR_Pool_Summary_Table.rds")
Pooldata2<-Pooldata[!is.na(Pooldata$Volume),]
Modeldata<-Pooldata2[Pooldata2$Pool %in% c("Pepin", "p8", "p26"), ]

# Set parameter estimates; (Lake Pepin and Pool 8, examples)
pool_names<-sub("p", "Pool ", Modeldata$Pool)
pool_names<-sub("PePool in", "Pepin", pool_names)
z = Modeldata$Z_mean_m #mean depth (m)
tau = round(Modeldata$WRT_d,digits=2) /365 #WRT (Years)
R = Modeldata$RNO3
vfrange<-c(0, 3) #log scale (m yr-1)

# plot curve
png("E:/Dropbox/FLAME_MississippiRiver/N_Model/N_retention_WRR.png", res=200, width=4,height=4, units="in")
par(cex=1)
par(mfrow=c(1,1))
par(mar=c(4,4,0.5,0.5), oma=c(0,0,0,0))

table<-as.data.frame(matrix(ncol=4, nrow=length(tau)*length(z)) )
names(table)<-c("tau", "z", "color", "lty")

colors<-blue2green2red(n=length(z))
lty<-seq(1, length(tau), 1)
Vf_calc<-rep(NA, length(z))
for (i in 1:(length(z))){
    z1 = z[i]
    tau1 = tau[i]
    R1<-R[i]
    Vf_calc1<-((-1)*z1/tau1 * log(1-R1))
    Vf_calc[i]<-Vf_calc1
    
    table[i,1]<- tau1
    table[i,2]<- z1
    table[i,3]<- colors[i]
    # table[i,4]<- lty[i]
    
    
    if (i ==1){
      # empty plot containing labels. Plot this first so vertical lines appear behind data
      curve(1-(exp(((-1)*tau1*x)/z1)), 10^vfrange[1],10^vfrange[2], ylab="", log = "x", xlab ="", type="n", las=1, xaxt="n")
      # add vertical lines to denote day, month, year, etc
      # abline(v=10^seq(vfrange[1],vfrange[2], by=1), col="darkgrey", lty=1, lwd=0.5)
      

      
      axis(1, at=10^seq(vfrange[1],vfrange[2], by=1), labels=10^seq(vfrange[1], vfrange[2], by=1))
      mtext(expression(paste(NO[3], " Retention")), 2, 2.5)
      mtext(expression(paste(V[f], " (m year"^"-1", ")")), 1, 2.5)
    }
    # Plot all curves
    curve(1-(exp(((-1)*tau1*x)/z1)), 10^(vfrange[1]-1),10^(vfrange[2]+1), log = "x", col = colors[i], lty=1, lwd=2, add=T)
    polygon(c(10^(vfrange[1]-1), 10^(vfrange[1]-1), Vf_calc1, Vf_calc1), c(-1, R1 ,R1 , -1), border=colors[i], col=NA, lty=2, lwd=1)

}

legend("topleft", inset=0.02, lwd = 2,legend = paste(pool_names, " (", tau*365, " d)", sep=""), col = table[,3], lty=1, cex=0.8)

box(which='plot')

dev.off()

#Predict WRT/z for pools with no depth data
Depthmodel<-lm(Pooldata$Z_mean_m~Pooldata$MC_Area)
Pooldata$Zguess<-Pooldata$Z_mean_m
for (pool in 1:nrow(Pooldata)){
  if (is.na(Pooldata$Zguess[pool])){
    Pooldata$Zguess[pool]<-Pooldata$MC_Area[pool]*Depthmodel$coefficients[2]+Depthmodel$coefficients[1]
  }
}
Pooldata$WRTguess<-Pooldata$Zguess*Pooldata$TotalArea/Pooldata$Q_cms*(1000000/3600/24)

#Simple linear models of Retention across UMR Pools

Gooddata<-Pooldata[!Pooldata$Pool %in% c('p18', 'p19', 'p20', 'p21', 'p22', 'p23', 'p24', 'All Pools'),]

png("E:/Dropbox/FLAME_MississippiRiver/N_Model/N_retention_Drivers.png", res=200, width=5,height=5, units="in")
cex=0.8
par(cex=cex)

par(mfrow=c(2,2))
par(mar=c(3,1,0.5,0.5), oma=c(0,3,0,0))
par(mgp=c(3,.5,0))
par(tck=-0.03)
plot(Gooddata$RNO3~ Gooddata$TotalArea, las=1, pch=16, cex.axis=cex)
mtext(expression(paste('Total Area (', 'km'^'2', ')')),1,2, cex=cex)
axis(2, labels=NA)
abline(h=0)
plot(Gooddata$RNO3~ rowSums(data.frame(Gooddata$BWc_Area, Gooddata$I_Area)), yaxt="n", pch=16, cex.axis=cex)
axis(2, labels=NA)
abline(h=0)
mtext(expression(paste('Non-Channel Area (%)')),1,2, cex=cex)

plot(Gooddata$RNO3~ Gooddata$NO3_start, las=1,pch=16, cex.axis=cex)
axis(2, labels=NA)
abline(h=0)
mtext(expression(paste('Incoming ', NO[3], " (mg N L"^"-1", ")")),1,2, cex=cex)
plot(Gooddata$RNO3~ Gooddata$WRT_d, yaxt="n", pch=16, cex.axis=cex)
points(Gooddata$RNO3~ Gooddata$WRTguess, yaxt="n", pch=1)
mtext(expression(paste('WRT (d)')),1,2, cex=cex)
axis(2, labels=NA)
abline(h=0)
legend("topleft", inset=0.01, c('Calculated', 'Modeled'), pch=c(16,1), bty="n")

mtext(expression(paste(NO[3], ' Retention (%)')),2,1, outer=T, cex=cex)

dev.off()



Modela<-lm(Gooddata$RNO3~Gooddata$TotalArea)
summary(Modela)
Modelb<-lm(Gooddata$RNO3~rowSums(data.frame(Gooddata$BWc_Area, Gooddata$I_Area)))
summary(Modelb)
Modelc<-lm(Gooddata$RNO3~Gooddata$NO3_start)
summary(Modelc)
Modeld<-lm(Gooddata$RNO3~Gooddata$WRT_d)
summary(Modeld)


null<-lm(Gooddata$RNO3~1)
Model1<-lm(Gooddata$RNO3~Gooddata$Z_mean_m + Gooddata$I_Area + Gooddata$BWc_Area + Gooddata$WRT_d + Gooddata$NO3_start)

null<-lm(Pooldata2$RNO3~1)
Model1<-lm(Pooldata2$RNO3~Pooldata2$Z_mean_m + Pooldata2$I_Area + Pooldata2$BWc_Area + Pooldata2$WRT_d + Pooldata2$NO3_start)
summary(Model1)
anova(Model1)
anova(Model1)
Model2<-step(Model1, scope=list(lower=null, upper=Model1), direction='backward')
summary(Model2)
anova(Model2)

Model_NO3<-lm(Pooldata2$RNO3~Pooldata2$NO3_start)
summary(Model_NO3)
anova(Model_NO3)

Model_Z<-lm(Pooldata2$RNO3~Pooldata2$Z_mean_m)
summary(Model_Z)
anova(Model_Z)

Model_WRT<-lm(Pooldata2$RNO3~Pooldata2$WRT_d)
summary(Model_WRT)
anova(Model_WRT)

Model_I<-lm(Pooldata2$RNO3~Pooldata2$I_Area)
summary(Model_I)
anova(Model_I)

Model_BW<-lm(Pooldata2$RNO3~Pooldata2$BWc_Area)
summary(Model_BW)
anova(Model_BW)

anova(Model_NO3, Model2)



null<-lm(Pooldata$Vf~1)
Model1<-lm(Pooldata$Vf~ Pooldata$I_Area + Pooldata$BWc_Area  + Pooldata$NO3_start)
summary(Model1)
anova(Model1)


Model_NO3<-lm(Pooldata$Vf~Pooldata$NO3_start)
summary(Model_NO3)
anova(Model_NO3)

anova(Model_NO3, Model1)

plot(Pooldata$Vf~Pooldata$RNO3)
simple<-lm(Pooldata$Vf~Pooldata$RNO3)
summary(simple)

Pooldata$C_Area<-Pooldata$MC_Area+Pooldata$SC_Area

Cordata<-Pooldata[,c(27, 12, 21:23, 18)]
M<-cor(Cordata, use="pairwise.complete.obs")
corrplot(M, method='circle')

labels<-sub("p", "", Pooldata$Pool)
labels<-sub("Pein", "Pepin", labels)
labels<-labels[-length(labels)]

png("E:/Dropbox/FLAME_MississippiRiver/N_retention_PerPool.png", res=200, width=3,height=4, units="in")
cex=0.7
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)

par(mar=c(2.5,0.5,0.5,3), oma=c(0,0,0,0))

barplot(rev(Pooldata$RNO3[-nrow(Pooldata)]), col="darkgrey", xlim=extendrange(Pooldata$RNO3[-nrow(Pooldata)], f=0.05), las=1, space=0, horiz=T, xaxt="n")
abline(v=0, lwd=1)
polygon( c(par('usr')[1:2], par('usr')[2:1]), c(2, 2, 8, 8), col="lightgrey", border=NA)
barplot(rev(Pooldata$RNO3[-nrow(Pooldata)]), col="darkgrey", xlim=extendrange(Pooldata$RNO3[-nrow(Pooldata)], f=0.05), las=1, space=0, horiz=T, add=T, xaxt="n")
axis(1, mgp=c(3,0.3,0))
axis(4, at=seq(1:length(Pooldata$RNO3[-nrow(Pooldata)]))-0.5, labels=rev(labels), las=1, mgp=c(3,0.5,0))
mtext("Pool", 4, 1.8)
mtext(expression(paste(NO[3], " Retention")), 1, 1.5)

text(x=0, par('usr')[4]-0.6, 'Production', pos=2, cex=cex)
text(x=0, par('usr')[4]-0.6, 'Retention', pos=4,cex=cex)
arrows(x0=c(-.2, .2), y0=c(par('usr')[4]-1.5), x1=c(-.4, .4), y1=c(par('usr')[4]-1.5), lwd=2, length=0.1)
text(x=par('usr')[1], y=5, "Non-baseflow conditions",cex=cex, pos=4, offset=0.1)

box(which='plot')

dev.off()



png("E:/Dropbox/FLAME_MississippiRiver/N_retention_PerPool_vertical.png", res=200, width=4,height=2.5, units="in")
cex=0.6
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)

par(mar=c(2.5,3,0.5,0.5), oma=c(0,0,0,0))

barplot((Pooldata$RNO3[-nrow(Pooldata)]), col="grey50", ylim=extendrange(Pooldata$RNO3[-nrow(Pooldata)], f=0.05), las=1, space=0, yaxt="n")
abline(h=0, lwd=1)
polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col="grey90", border=NA)
barplot((Pooldata$RNO3[-nrow(Pooldata)]), col="grey50", ylim=extendrange(Pooldata$RNO3[-nrow(Pooldata)], f=0.05), las=1, space=0, yaxt="n", add=T)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-1,.5, by=0.5), labels=seq(-100, 50, by=50))
axis(1, at=seq(1:length(Pooldata$RNO3[-nrow(Pooldata)]))-0.5, labels=NA, mgp=c(3,0.2,0), las=0)
text(seq(1:length(Pooldata$RNO3[-nrow(Pooldata)]))+0.5, par("usr")[3] - 0.05, labels = labels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)
mtext(expression(paste(NO[3], " Retention (%)")), 2, 1.5)

text(x=mean(par('usr')[1:2]), y=0, 'Production', pos=1, cex=cex, offset=2.2)
text(x=mean(par('usr')[1:2]),  y=0, 'Retention', pos=3, cex=cex, offset=2.2)
arrows(y0=c(-.2, .2), x0=mean(par('usr')[1:2]), y1=c(-.38, .38), x1=mean(par('usr')[1:2]), lwd=2, length=0.08)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=18, y=-1.2, "High tributary flows", cex=cex, pos=2)
arrows(y0=-1.2, x0=17.5, y1=-1.2, x1=19, lwd=2, length=0.08)

box(which='plot')

dev.off()

