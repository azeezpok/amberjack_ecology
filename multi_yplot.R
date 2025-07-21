#_________________________________________________________________________________________________
#### Data ###
#_________________________________________________________________________________________________
setwd("D:/")
ddata<- read.csv("Trend analysis.csv")
str(ddata)
View(ddata)
head(ddata$Month)
ddata$Month <- factor(ddata$Month, levels=c("Jan-14", "Feb-14", "Mar-14", "Apr-14", "May-14",
                                            "Jun-14","Jul-14","Aug-14","Sep-14","Oct-14",
                                            "Nov-14","Dec-14", "Jan-15", "Feb-15", "Mar-15",
                                            "Apr-15", "May-15","Jun-15","Jul-15","Aug-15",
                                            "Sep-15", "Oct-15", "Nov-15","Dec-15",
                                            "Jan-16", "Feb-16", "Mar-16", "Apr-16", "May-16",
                                            "Jun-16","Jul-16","Aug-16","Sep-16","Oct-16",
                                            "Nov-16","Dec-16",
                                            "Jan-17", "Feb-17", "Mar-17", "Apr-17", "May-17",
                                            "Jun-17","Jul-17","Aug-17","Sep-17","Oct-17",
                                            "Nov-17","Dec-17",
                                            "Jan-18", "Feb-18", "Mar-18", "Apr-18", "May-18",
                                            "Jun-18","Jul-18","Aug-18","Sep-18","Oct-18",
                                            "Nov-18","Dec-18",
                                            "Jan-19", "Feb-19", "Mar-19", "Apr-19", "May-19",
                                            "Jun-19","Jul-19","Aug-19","Sep-19","Oct-19",
                                            "Nov-19","Dec-19"))
head(ddata)
attach(ddata)
# Plot the data

plot(x=ddata$Sr.no,y=ddata$CPUE, type ='l', ylab = "CPUE",xlab="Months", col='blue',lwd=2)

# Add the second y-axis
plot(x=ddata$Sr.no,y=ddata$CPUE, type ='l', ylab = "CPUE",xlab="Months", col='blue',lwd=2)
par(new = TRUE)
plot(x=ddata$Sr.no,y=ddata$Zeu, type ='l', ylab = "Zeu",xlab="Months", col='green',lwd=2)

# updated plot
plot(x=ddata$Sr.no,y=ddata$CPUE, type ='l', ylab = "CPUE",xlab="Months", col='blue',lwd=2)
par(new = TRUE)
plot(x=ddata$Sr.no,y=ddata$Zeu, type ='l', xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col='green',lwd=2)

# Overlay y1&y2 plot
par(mar = c(5, 10, 3, 5)+ 0.1)
plot(x=ddata$Sr.no,y=ddata$CPUE, type ='l', ylab = "CPUE",xlab="Months", col='blue',lwd=2)
par(new = TRUE)
plot(x=ddata$Sr.no,y=ddata$Zeu, type ='l', xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col='green',lwd=2,lty = 2)
axis(side = 4)
mtext("Zeu", side = 4, line = 3)
legend("topleft", c("CPUE", "Zeu","SST"),
       col = c("blue", "green","red"), lty = c(1, 2))

#_________________________________________________________________________________________________
#### plot multiple y axis ###
#_________________________________________________________________________________________________

head(ddata)
#jpeg("Timeseries_multiparameter.jpg", res = 600,height = 5,width = 13,units = "in")

#Define Margins. The trick is to use give as much space possible on the left margin (second value)
par(mar=c(5, 10, 3, 10) + 0.1)

#Plot the first time series(CPUE). Notice that you don’t have to draw the axis nor the labels
#line=space from margin, lwd=line width, margin side= 2(left)& 4(right)
plot(Sr.no, SST, axes=F, ylim=c(min (SST),30), xlab="", ylab="",type="l",col="blue", 
     main="",lwd=2)
axis(2, ylim=c(min (SST),30),col="blue",lwd=2)
mtext(2,text="SST",line=2)

par(new=T)
plot(Sr.no, SSS, axes=F, ylim=c(31,35), xlab="", ylab="", 
     type="l",main="",col="brown",lwd=2)
axis(2, ylim=c(31,35),col="brown",lwd=2,line=3.5)
mtext(2,text="SSS",line=5.5)

par(new=T)
plot(Sr.no, SSH, axes=F, ylim=c(0.50,0.75), xlab="", ylab="", 
     type="l", main="",col="red",lwd=2)
axis(2, ylim=c(0.50,0.75),col="red",lwd=2,line=7)
mtext(2,text="SSH",line=9)

par(new=T)
plot(Sr.no, CHL, axes=F, ylim=c(0.1,0.8), xlab="", ylab="", 
     type="l", main="",col="green3",lwd=2)
axis(4, ylim=c(0.1,0.8),col="green3",lwd=2)
mtext(4,text="CHL",line=2)

par(new=T)
plot(Sr.no, MLD, axes=F, ylim=c(min (MLD),max(MLD)), xlab="", ylab="", 
     type="l",main="",col="black",lwd=2)
axis(4, ylim=c(min (MLD),max(MLD)),col="black",lwd=2,line = 3.5)
mtext(4,text="MLD",line=5.5)

par(new=T)
plot(Sr.no, EKE, axes=F, ylim=c(0.00,0.20), xlab="", ylab="", 
     type="l",main="",col="purple",lwd=2)
axis(4, ylim=c(0.00,0.20),col="purple",lwd=2,line=7)
mtext(4,text="EKE",line=9)

axis(1,at=as.factor(ddata$Month),labels = ddata$Month, las=2)
mtext("Month",side=1,col="black",line=4)
dev.off()

#legend("topleft", c("CPUE", "Zeu","SST","sqrt(depth)","SSHa"),
#      col = c("blue", "green","red","black","brown"),  lty = c(1, 2,3,4,5),
#      horiz = F,ncol = 2)

#_________________________________________________________________________________________________
#### THE END ###
#_________________________________________________________________________________________________
