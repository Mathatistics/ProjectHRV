#------------------------------------------------
# 2015-10-13 Veronika Lindberg
#
# NTNU TTK19 Read RR intervals and convert to Heart Rate (beats per minute)
#------------------------------------------------
rm(list=ls()) # clean up workspace
pardefault <- par(no.readonly = T) # save plot settings

# Load data file
Dataset <- 
  read.table("",
             header=FALSE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)
#RR <- Dataset$V1;
Dataset <- 
  read.table("C:/Users/Veronika/Dropbox/NTNU/aas/HRVdata/RR_row_sec/RRdata4_sec.txt", #RRdataNightPerson1_sec.txt",
             header=FALSE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
Dataset <- as.numeric(Dataset)
str(Dataset)
list(Dataset[[1,1:length(Dataset)]])
if (length(Dataset)==1) 
  {
  RR <- Dataset$V1
} else 
  {  
  RR <- as.numeric(Dataset)
  }


str(RR);
# If RR interval is given in msec, convert it to seconds 
mRR <- mean(RR);

if (mean(RR) > 100) {
  RR <- RR/1000;
  mRR <- mean(RR);
}
RRcol <- 1; #col(RR)
RRlength <- length(RR);

# write data back to file with RR intervals in seconds
#write.table(RR, 
#            "C:/Users/Veronika/Dropbox/NTNU/aas/HRVdata/RR_col_sec/aksel24h_sec.txt",
#            dec=".",sep="",row.names=FALSE)

# create a variable to hold the time axis
T <- 1:RRlength;

# add time intervals (now in seconds)
T[1]= RR[1];
for (i in 2:RRlength) {
  T[i]= RR[i]+T[i-1];
}

T = T / 60; # Time axis in minutes


HR <- 1:RRlength;
for (i in 1:RRlength) {
  HR[i]=(1/RR[i])*60;
}
mHR <- mean(HR)

plot(RR,main="RR event series")
abline(a=mRR,b=0,col = "gray50",lty="dotted")
#mtext ("RR event series",side=3,line=0, cex=1.0) #side 3 = top, below main title
text(0.8,mRR, "Mean", col = "blue", pos=3)


plot(T,RR,'l',
     ylab=list("RR interval (seconds)", cex=1.0),
     xlab=list("Time (minutes)",cex=1.0),
     main = "RR time series")
abline(a=mRR,b=0,col = "gray50",lty="dotted")
text(0.8,mRR, "Mean", col = "blue", pos=3)

#lm1 <- lm(RR~T)
#plot(lm1)

plot(T,HR,'l',
     ylab=list("Heart Rate (beats per minute)", cex=1.0),
     xlab=list("Time (minutes)",cex=1.0),
     main = "HR time series")
abline(a=mHR,b=0,col = "gray50",lty="dotted")
text(0.8,mHR, "Mean", col = "blue", pos=3)

#lm2 <- lm(HR~T)
#plot(lm2)

# write data back to file with beats per minute, T axis is in minutes
write.table(round(HR,3),
            "C:/Users/Veronika/Dropbox/NTNU/aas/HRVdata/HR_col_sec/HR_test.txt",
            dec=".",sep="",row.names=FALSE, col.names=FALSE)
