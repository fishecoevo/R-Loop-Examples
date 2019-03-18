# Model and plot growth curves for a variety of fish populations using a loop

# Author and copyright P.J. Kennedy 3/18/2019

rm(list=ls())
setwd("C:\\Users\\Patrick James\\Documents\\FWIN Data Analysis\\csv files")
FWIN<- read.table("Northern Pike Full.csv", header = TRUE, sep=",", na.strings=".") 
str(FWIN) 
head(FWIN)

#renaming columns
FWIN$fork= FWIN$FLEN.C.4
FWIN$total= FWIN$TLEN.C.4
FWIN$weight= FWIN$RWT.C.7
FWIN$sex= FWIN$SEX.C.1
FWIN$mat=FWIN$MAT.C.1
FWIN$age=FWIN$AGE.C.3
FWIN$prj=FWIN$PRJ_CD.C.12

#creating a new, more simple data frame
fwin= data.frame(FWIN[,c("prj", "fork", "total", "weight", "age", "sex",
                         "mat")])
str(fwin)
head(fwin)

###############################################################################################
# loop for VBGMs by sex
ageplus1= (fwin$age+1)
fwin2<-data.frame(fwin, ageplus1)
fwin2=subset(fwin2, sex<3)
fwin2$sex=as.character(fwin2$sex)
fwin2$sex[fwin2$sex=="1"]="Male"
fwin2$sex[fwin2$sex=="2"]="Female"
unique(fwin2$sex)
str(fwin2)
head(fwin2)
table(fwin2$ageplus1)

outlist=NULL
for (i in levels(fwin2$prj)){
  sub.fwin=subset(fwin2, fwin2$prj == i) #for each level of prj
  males=subset(sub.fwin, sub.fwin$sex == "Male") 
  females=subset(sub.fwin, sub.fwin$sex == "Female")  
  if(nrow(males) > 19) { #nrow counts number of rows for each level of sex and prj
      fwin.st=c(L = 800, k = 0.1) #starting values for parameter estimate
      fwin.mal= nls(fork~L*(1-exp(-k*(ageplus1-0))), data = males, 
                   start = fwin.st, trace = T) #equation for VBGM with fork lengths
      fwin.fem= nls(fork~L*(1-exp(-k*(ageplus1-0))), data = females, 
                  start = fwin.st, trace = T)
      #means for variables that we will need for plotting the VBGM fit
      meanmal<-tapply(males$fork, males$ageplus1, mean, na.rm=TRUE)
      meanfem<-tapply(females$fork, females$ageplus1, mean, na.rm=TRUE)
      #saving means as a data frame 
      meanmale<-as.data.frame(meanmal)
      meanfemale<-as.data.frame(meanfem)
      fwin.num<-as.numeric(rownames(meanmal)) #changing means to num structure
      forfwin<-data.frame(age=fwin.num, meanmal) 
      ages<-c(0:20) #setting what age the plot x-axis should go to
      options(graphics.record = TRUE) 
      #plotting out the fit for the VBGM
      #setting a new working directory for plots to be sent to
      setwd("C:/Users/Patrick James/Documents/FWIN Data Analysis/FWIN VBGMs/Male vs. Female")
      #saving plots 
      jpeg(paste(i,".jpeg",sep=""),res=200,width=20,height=20,units="cm")
      plot(forfwin[,1], forfwin[,2], pch=16, ylab = "fork length (mm)", xlab = "age (years)",
           ylim = c(0, 1200), yaxs = "i", xlim = c(0,18), xaxs = "i", main = i)
      points(males$fork ~ males$ageplus1, col="black") #plotting actual data points
      points(females$fork ~ females$ageplus1, col="gray30") #plotting actual data points
      lines(ages, predict(fwin.mal, list (ageplus1 = ages)), lwd =2, col ="black") #curve
      lines(ages, predict(fwin.fem, list (ageplus1 = ages)), lwd =2, col ="gray30") #curve
      dev.off() #must close with this function for plots to process 
      outlist<-c(outlist, i) 
    }
}
