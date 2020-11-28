#R script week 4

##########################################################
######### where are the smartest people in the class? ####
##########################################################

#GPA.txt is not a .csv file
setwd("./AM/Module 5/Statistics/RSessions")
data=read.table("GPA.txt", sep="\t", header = T) 
#Read GPA.txt into R
attach(data)
names(data)

#missing values
GPA_red=GPA[is.na(GPA)==0]
Seat_red=Seat[is.na(GPA)==0]
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

#two sample t-test
t.test(front,back,alternative = "greater")
t.test(front,middle,alternative = "greater")
t.test(middle,back,alternative = "greater")

detach(data)


###############################################
######### analyze sinterclaas data  ###########
###############################################

#Read the dataset zwartepietendiscussie.csv

data=read.csv("zwartepietendiscussie.csv")
attach(data)
names(data)
View(data)
#the datasets contains a sample of Twitter tweets from 2017 related to sinterklaas and zwarte piet.

###################     the variables    ###################
#likes             - number of likes for each tweet
#timestamp         - timestamp for each tweet
#text              - text for each tweet
#week              - calendar week in which the tweet was posted
#month             - month (as a number from 1-12) in which the tweet was posted
#retweets          - number of retweets for each tweet
#replies           - number of replies for each tweet
#user              - username for each tweet
#fullname          - full name for each tweet
#is_piet           - does the tweet mention #zwartepiet
#is_sint           - does the tweet mention #sinterklaas
#is_racism         - does the tweet contain the word "rasisme" (racism)
#is_tradition      - does the tweet contain the word "traditie" (tradition)

##################################################################
##Confidence intervals for the proportion of tweets in the dataset 
#mentioning #zwartepiet in June and in October.
##################################################################

ZP_june=sum((month==6)&(is_piet=="Yes")) #(june is the 6th month)
#146
nr_tweets_june=sum((month==6))
prop.test(ZP_june,nr_tweets_june) #proportion test

#95 percent confidence interval:
#  0.27 0.36

nr_tweets_october=sum((month==10))
ZP_oct=sum((month==10)&(is_piet=="Yes"))
prop.test(ZP_oct,nr_tweets_october)
#95 percent confidence interval:
#  0.34 0.39

#############################################################
#Is the proportion of tweets in the dataset mentioning 
# #zwartepiet in October significantly higher than in June?
#############################################################

prop.test(c(ZP_june,ZP_oct),c(nr_tweets_june,nr_tweets_october),
          alternative = "less")
#p-value = 0.02064

#############################################################
#Is is true that a significantly higher proportion of tweets is liked 
#at least once than retweeted at least once. Make a test
#############################################################

#number of like tweets
nr_likes=sum(likes>0)
#number of retweeted
nr_retweets=sum(retweets>0)
prop.test(c(nr_likes,nr_retweets), c(length(likes),length(likes)), 
          alternative = "greater")

#YES
# p-value < 2.2e-16

