#R script week 5

#In this exercise, we build a regression model for 
#the height of dinosaurs. 
#The file ”dinosaurs.csv”, 
#contains a list of 114 dinosaurs. 


#Use the command read.csv(file.choose(), header=T) 
#to read the dataset ”dinosaurs.csv” into R.
data_dinos=read.csv(file.choose(), header=TRUE)
attach(data_dinos)
names(data_dinos)

#Scatter plot of the variable height and the
#variable weight
plot(height, weight)

#scatter plot to show that the height of dinosaurs and 
#the weight^(1/3) [cube root of weight] have an approximately 
#linear relationship.
plot(height, weight^(1/3),
     ylab="third root of weight")
#related to BMI and Ponderal index

#Run a regression with height of dinosaurs as output variable 
#using as explanatory variables
#the weight^(1/3)



weight13=weight^(1/3)
plot(weight13,height,col="blue",pch=4)
abline(regr0, col="lawngreen")
#plot residuals
plot(height,residuals(regr0),col="blue",pch=4)

regr0=lm(height~weight13)
summary(regr0)


#run a regression with height of 
#dinosaurs as output variable 
#using as explanatory variables
#the weight^(1/3) and the length
regr1=lm(height~weight13+length)

summary(regr1)
#Interpretation: Both length and weight^(1/3) are significant. 
#Length is even significant with p-value<0.01.
#R-squared is 0.73
#adjusted R-squared is 0.72

############ PREDICTION INTERVAL ############
#compute a 95% prediction interval for height 
#given that weight = 20000 and 
#length = 15.
newdata=data.frame(weight13=20000^(1/3), length = 15)
pred_int=predict(regr1, newdata, interval="predict")

#       fit     lwr      upr
# 5.436069 3.14181 7.730328

#Also use variable how.it.moved 
#as a dummy variable. We regress 
#height on weight^(1/3), length, 
#and how.it.moved.

regr2=lm(height~weight13+length+factor(how.it.moved))
#for categorical variables that are 
#not decoded as numbers, it is not 
#necessary to use factor()

summary(regr2)
#Interpretation: 
#weight^(1/3) and length are significant
#none of the categories of how.it.moved is significant
#2 legs without flying has a positive regression coefficient,
#the other ones negative regression coefficients
#this means that dinosaurs with 2 legs tend to be taller 
#(relative to length and weight) compared with the other catgories

#R-squared is 0.75
#adjusted R-squared is 0.73.
#this is essentially the same as for the 
#regression without the dummy variable
#this means that the how.it.moved variable 
#does not help to explain the height

detach(data_dinos)
