#########################
# loopex.R              #
# example of looping    #
# a function in R       #
#                       #
#########################

#the simplest loop ever


for (i in 1:10)
	{
	print (i)
	}

#now let's do something useful

set.seed(12345) #make our example reproducible; when you actually start running loops, you'll want to comment this bit out

#start by building a dataset to use in our example; you'll likely have your own already...

var1<-c(rep("A", 6), rep("B", 6), rep("C", 6))
var2<-sample(0:100, 18, replace = TRUE)
var3<-sample(20:60, 18, replace = TRUE)

allvars.dat<-data.frame(var1, var2, var3)
allvars.dat

allvars.dat$var1<-factor(allvars.dat$var1)

#make sure that var1, our grouping variable, is recognized as a factor
#THIS WILL BE IMPORTANT IF YOUR GROUPING VARIABLE IS NUMERIC

is.factor(allvars.dat$var1)

#Now build the loop

outlist=NULL

for (i in levels(allvars.dat$var1))
	{
        sub.dat<-subset(allvars.dat, allvars.dat$var1 == i) #for each level of var1
        m1<-mean(sub.dat$var2) #estimate the mean of var2
        mx2<-max(sub.dat$var2) #estimate the max of var2
        l3<-length(sub.dat$var3) #estimate the length of var3	
	outlist<-c(outlist, i, m1, mx2, l3) #stick our results together, tack it on to the previous output
	}

#now get the output into a matrix of useable, analyzable data

outdat<-matrix(outlist,ncol=4,byrow=TRUE) #make number of columns equal to the number of variables you extract from each iteration
outdat

#assign column names

colnames(outdat, do.NULL=FALSE)
colnames(outdat)<-c("var1", "m1", "mx2", "l3")

outdat


#turn it into a dataframe that you can use for further analyses

outdat.dat<-data.frame(outdat)
outdat.dat

#or, write it to a text file

write.table(outdat, "output.dat")

#you can find the file you just wrote in whatever working directory you are currently in
#note that summary(dataset) will give you all the info. in the loop we built above (and more), 
#but the above loop gives you the construct to apply any function over data from different levels of a treatment

