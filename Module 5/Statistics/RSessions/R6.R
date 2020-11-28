#R script week 6

data_dinos=read.csv(file.choose(), header=TRUE)
attach(data_dinos)
names(data_dinos)

#run a regression with height of 
#dinosaurs as output variable 
#using as explanatory variables
#the weight^(1/3) and the length
weight13=weight^(1/3)
regr1=lm(height~weight13+length)

############ PREDICTION INTERVAL ############
#compute a 95% prediction interval for height 
#given that weight = 20000 and 
#length = 15.
newdata=data.frame(weight13=20000^(1/3), length = 15)
predict(regr1, newdata, interval="predict")

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


#We want to use the diet variable as a dummy variable. 
#Notice that there are three dinosaurs 
#for which the diet is unknown. 
#We create new vectors height1, weight1, 
#length1, diet1, which are the 
#same as height, weight, length and 
#diet but with the three entries removed

where_unknown=which(diet %in% "unknown") 
#this finds the positions of the 
#entries that are "unknown". One can
#also use match, but which() is more
#elegant here

height1=height[-where_unknown]
weight1=weight[-where_unknown]
length1=length[-where_unknown]
diet1=diet[-where_unknown]

#Next we do a regression of height1, using 
#weight1^(1/3), length1, diet1 
#as explanatory variables.
weight1_13=weight1^(1/3)
regr3=lm(height1~weight1_13+length1+factor(diet1))
summary(regr3)

#Interpretation: 
#diet is not significant and does not help 
#either to better explain the
#height of dinosaurs.

detach(data_dinos)

#We do the chi^2-test for
#the dataset zwartepietendiscussie.csv

data=read.csv(file.choose())
attach(data)

#we make a two-way table with the following 
#two categorical variables
#1st categorical variable with 2 categories 
#being tweets that contain "#sinterklaas" 
#but not "#zwartepiet" and tweets that contain 
#"#zwartepiet" but not "#sinterklaas#
#2nd categorical variable with three categories 
#being tweets in weeks 19-28, 
#weeks 29-37, weeks 38-47

tweet_type=numeric(length=length(week)) #create vector
tweet_type[(is_piet=="Yes") & (is_sint=="No")]="onlyZP"
tweet_type[(is_piet=="No") & (is_sint=="Yes")]="onlySK"

week_categorical=numeric(length=length(week))
week_categorical[week<29]="Early"
week_categorical[(29<=week)& (week<38)]="Mid"
week_categorical[38<= week]="Late"

temp=table(week_categorical,tweet_type)
our_table=temp[ ,-1]

#Is there a significant change in proportions 
#over the three time intervals?

chisq.test(our_table)
#p-value = 0.0005


#Does the proportion of tweets changes over 
#the three time intervals 
#that either contain the word "traditie" 
#(tradition) or "rasisme" (racism)? 

RTtweets=numeric(length=length(week))
RTtweets[(is_racism=="Yes") & (is_traditie=="No")]="onlyR"
RTtweets[(is_racism=="No") & (is_traditie=="Yes")]="onlyT"

temp2=table(week_categorical,RTtweets)
our_table2=temp2[ ,-1]

chisq.test(our_table2)
chisq.test(our_table2, simulate.p.value = T)

#p-value = 0.07796   -> No