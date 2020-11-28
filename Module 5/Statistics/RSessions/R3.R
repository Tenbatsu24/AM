#R script week 3

################################################
#####    analyze fertility rate dataset     ####
################################################


data = read.csv(file.choose(), nrows=229) 
#nrows limits the number of lines that are read
attach(data)

################################
#########Netherlands ###########
################################
posNL=match('Netherlands',Country.or.Area)
data[posNL, ] #gives us row 151

#fertility rates for the Netherlands
fertNL=data[posNL,
            seq(from=2,to=length(data[posNL, ]), by=2 )]
fertNL
summary(fertNL) #fertNL is a table
summary(as.numeric(fertNL)) #fertNL is a vector

#make a plot
1960:2014 #years
plot(1960:2014,fertNL, type = 'l', 
     ylab = "Fertility")
#make a nice plot
plot(1960:2014,fertNL, type = 'l', 
     ylab = "Fertility", xlab="Years", 
     main="Fertility in the Netherlands",
     lwd=2,col="darksalmon")

################################
######### China ################
################################
posCN=match('China',Country.or.Area)
fertCN=data[posCN,seq(from=2,to=length(data[posCN, ]), by=2 )]
plot(1960:2014,fertCN, type = 'l', ylab = "Fertility", xlab="Years", main="Fertility in China",lwd=2,col="darksalmon")

######### In one plot
plot(1960:2014,fertNL, type = 'l', 
     ylab = "Fertility", xlab="Years",lwd=2,
     col="darksalmon",ylim = c(0,max(fertCN)+0.3),
     main="Comparison of fertility rates")
lines(1960:2014,fertCN, type = 'l', lwd=2,col="lawngreen")
#for adding a legend use legend()

################################################################
########find the country with the highest fertility ############
################################################################

sort(X2014, decreasing = F)
order(X2014) #gives us the permutation for sorting X2014
Country.or.Area[order(X2014)]
X2014[order(X2014)]


################################
######### boxplots  ###########
################################

boxplot(X2014, col="hotpink",xlab="2014")
boxplot(X1960, X2014, col=c("hotpink","gold"),xlab=c("1960", "2014"))


################################
######### world map  ###########
################################

#install package via interface or use install.packages()
library(rworldmap)
df=data.frame(Country.or.Area,X2014)
sPDF = joinCountryData2Map(df,joinCode = "NAME",nameJoinColumn = "Country.or.Area")
mapCountryData(sPDF, nameColumnToPlot = "X2014")

detach(data)

###############################################
#########     test for the mean     ###########
###############################################

n=1000
observed=rnorm(n, mean=10, sd=2)
m=mean(observed)
s=sd(observed)
#95% confidence interval for the mean 
#lower bound
m-1.96*s/sqrt(n)
m+1.96*s/sqrt(n)
t.test(observed, mu=10.1, alternative = "greater")
t.test(observed, mu=10.1, alternative = "two.sided")

##########################################################
######### where are the smartest people in the class? ####
##########################################################

#GPA.txt is not a .csv file
data=read.table(file.choose(), sep="\t", header = T) 
#Read GPA.txt into R
attach(data)
names(data)

#missing values
GPA_red=GPA[is.na(GPA)==0]
Seat_red=Seat[is.na(GPA)==0] #is a factor object in R (categorical variable)
levels(Seat)
missingvalues=which(Seat_red=="")
Seat_red=Seat_red[-missingvalues]
GPA_red=GPA_red[-missingvalues]

front=GPA_red[which(Seat_red=="1_Front")] 
middle=GPA_red[which(Seat_red=="2_Middle")]
back=GPA_red[which(Seat_red=="3_Back")]
mean(front)
mean(middle)
mean(back)

#boxplots
boxplot(front,middle,back, col = c("blue","red","yellow"),
        names=c("Front","Middle","Back"), ylab="GPA")

#95% one-sided CI for mean(front)-mean(middle)
#two sample t-test
t.test(front,back,alternative = "greater")
t.test(front,middle,alternative = "greater")
t.test(middle,back,alternative = "greater")

detach(data)