#R script week 2


#distributions 
#pnorm, dnorm, rnorm, qnorm for normal distribution
#pbinom, dbinom, rbinom, qbinom for binomial distribution
#and so on
#check out the help

#determin 3rd quartile of standard normal distribution
qnorm(0.75)

#determine probability that bin(20,0.5) random variable 
#is less than 14
pbinom(14,20,0.5)


################################################
##### compare two estimators from lecture   ####
################################################

#data generation
n=10
lambda=2
data=rpois(n,lambda=lambda)
#generates n independent random variables from
#Poisson distribution with intensity parameter lambda

lambda_hat_1=sum(data==0)/n #relative frequency
lambda_hat_2=exp(-mean(data)) #plug-in
p_true=exp(-lambda)
(lambda_hat_1-p_true)^2 #quadratic loss of lambda_hat_1
(lambda_hat_2-p_true)^2 #quadratic loss of lambda_hat_2

err1=0
err2=0
nr_iter=1000
for(i in 1:nr_iter){
  data=rpois(n,lambda=lambda) 
  lambda_hat_1=sum(data==0)/n #relative frequency
  lambda_hat_2=exp(-mean(data)) #plug-in
  err1=err1+(lambda_hat_1-p_true)^2 #quadratic loss of lambda_hat_1
  err2=err2+(lambda_hat_2-p_true)^2 #quadratic loss of lambda_hat_2
}
av_err1=err1/nr_iter #average loss of rel. frequency
av_err2=err2/nr_iter #average loss of plug-in

#which estimator is better? Method 2

#how to read dataset into R
table=read.csv(file.choose())
attach(table) #do once or use detach()
names(table)
Country
High
Numeric
View(table)

is.na(Display.Value)  #missing values 
is.na(Numeric)
sum(is.na(Numeric)) #TRUE=1 and FALSE = 0

summary(table) #different than summary for vector object

detach(table) # to undo the attach command


################################################
#####    analyze fertility rate dataset     ####
################################################


data = read.csv(file.choose()) #first try
attach(data)
names(data)
Country.or.Area #last entries are no countries
View(data)
detach(data)

data = read.csv(file.choose(), nrows=229) 
#nrows limits the number of lines that are read
attach(data)
names(data)
Country.or.Area #now it works

data$X1960 #is the same as X1960 but does not need attach(), 
#can be used if several datasets with the same names are analyzed

#missing values
X1960[225] #NA stands for missing value
View(data) #look at the data
mean(X1960) #doesnt work
mean(X1960, na.rm = T) #that works because it ignores the NA entries
X1960
is.na(X1960) #indicates which values are missing

hist(X1960) #histogram
hist(X2014)
hist(X2014, freq=F)
hist(X2014, breaks=2, freq=F) #not interesting
hist(X2014, breaks=20, freq=F) #best
hist(X2014, breaks=60, freq=F) #too much variability

#compare with normal distribution
#add a normal p.d.f.
m=mean(X1960, na.rm=T)
stdev=sd(X1960, na.rm=T)
hist(X1960, freq=F)
curve(dnorm(x,mean=m, sd=stdev),col="red",add=TRUE)

################################
#########Netherlands ###########
################################
posNL=match('Netherlands',Country.or.Area)
View(data)
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
plot(1960:2014,fertNL, type = 'l', ylab = "Fertility", xlab="Years",lwd=2,col="darksalmon",ylim = c(0,max(fertCN)+0.3),main="Comparison of fertility rates")
lines(1960:2014,fertCN, type = 'l', ylab = "Fertility", xlab="Years",lwd=2,col="lawngreen")
#for adding a legend use legend()

################################################################
########find the country with the highest fertility ############
################################################################

sort(X2014, decreasing = T)
order(X2014)
Country.or.Area[order(X2014)]
X2014[order(X2014)]


################################
######### boxplots  ###########
################################

boxplot(X2014, col="hotpink",xlab="2014")
boxplot(X1960, X2014, col=c("hotpink","gold"),xlab="2014")


################################
######### world map  ###########
################################

#install package via interface or use install.packages()
library(rworldmap)
df=data.frame(Country.or.Area,X2014)
sPDF = joinCountryData2Map(df,joinCode = "NAME",nameJoinColumn = "Country.or.Area")
mapCountryData(sPDF, nameColumnToPlot = "X2014")

