#
#comments are very helpful

library("datasets") #activates package datasets
#before you need to have the package installed

#how to assign a variable
a <- 1
a=2

#basic operations
1+1
3*8
3-9
7/89

6^8
sqrt(9)
9^(1/2)
9^1/2

exp(4) #exponential function
log(exp(4)) #logarithm
sin(6)

#vectors
vec=c(8,-9,0,4.5) #why is that called c()? -> concatenate
length(vec)
1:8
seq()
seq(from =1, to =8, by =1/2) #sequence seq()
#seq(1:8, by=1/2) #does not work
seq(1,8,1/2) #same as seq(from =1, to =8, by =1/2) but likely to cause errors
seq(by =1/2, to =8, from =1)
summary(vec)
sum(vec)
sort(vec)
sort(vec, decreasing = TRUE)
sort(vec, decreasing = T)

mean(vec)
sum(vec)/length(vec)

#vector operations
v=c(8,7,4)
w=c(5,-4,0.005)
v+w
2*v
v-3*w
v*w #is entry-wise product
v %*% w #scalar product (!)

#how to access entries in vectors
data=c(8,4.5,-9,0,5)
data[2]
data[2,5]
data[c(5,2)]
data[c(2,5)]
data[2:5]

data[-3]
data[-c(1,3)]
y=data[data>4]
data>4
data[data>=4.5]

#&=logical "and"
data[data>4 & data<6]
#|=logical "or"
data[data>5 | data<1]

#how many entries =5 are there in the dataset?
sum(data==5) #"logical" ==


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


