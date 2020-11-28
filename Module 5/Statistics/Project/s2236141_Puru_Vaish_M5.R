# Dependencies ----
library(reshape2)
library(car)
library(tidyverse)
library(BSDA)

# Set Working Directory ----
if (getwd() != "D:/Documents/AM/Module 5/Statistics/Project") {
  setwd("./AM/Module 5/Statistics/Project")
}
getwd()

# Get data sets ----
co2_emissions <- read.csv("consumption_co2_emissions_1000_tonnes.csv")
adjusted_gdp <- read.csv("total_gdp_us_inflation_adjusted.csv")
wb_classification <- read.csv("WB_Country_Classification.csv")
population <- read.csv("population_total.csv")

# transform data sets from time series like format to data frame format
columns_to_select <- c("country", paste0("X", seq(1990, 2017)))
## for CO2 consumption emissions we only have data from 1990 to 2017 hence all the data has been truncated for that range

population <- melt(
  population[columns_to_select], "country",
  variable.name='year',
  value.name = 'population'
)
wb_classification <- melt(
  wb_classification[columns_to_select], "country",
  variable.name='year',
  value.name = 'classified_as'
)
co2_emissions <- melt(
  co2_emissions[columns_to_select], "country",
  variable.name='year',
  value.name = 'consumption_co2_1000_tonnes'
)
adjusted_gdp <- melt(
  adjusted_gdp[columns_to_select], "country",
  variable.name='year',
  value.name = 'total_gdp_dollars'
)

# order data properly
population <- population[order(population[,1], population[,2]),]
wb_classification <- wb_classification[order(wb_classification[,1], wb_classification[,2]),]
co2_emissions <- co2_emissions[order(co2_emissions[,1], co2_emissions[,2]),]

# Calculate new columns from existing ----
co2_emissions <- co2_emissions %>%
  group_by(country) %>%
  mutate(consumption_co2_emission_tonnes = 1000*consumption_co2_1000_tonnes) %>%
  mutate(consumption_co2_emission_percent_change = (consumption_co2_1000_tonnes/lag(consumption_co2_1000_tonnes) - 1) * 100) %>%
  select_all()

population <- population %>%
  group_by(country) %>%
  mutate(population_percent_change = (population/lag(population) - 1) * 100) %>%
  select_all()

adjusted_gdp <- adjusted_gdp %>%
  group_by(country) %>%
  mutate(gdp_percent_change = (total_gdp_dollars/lag(total_gdp_dollars) - 1) * 100) %>%
  select_all()


# merging data sets
df <- merge(population, wb_classification, by = c("country", "year"))
df <- merge(df, co2_emissions, by = c("country", "year"))
df <- merge(df, adjusted_gdp, by = c("country", "year"))

# getting new columns from merged data sets

df$consumption_co2_emission_tonnes_per_capita <- df$consumption_co2_emission_tonnes / df$population
df$adjusted_gdp_per_captia <- df$total_gdp_dollars / df$population
df <- df %>%
  group_by(country) %>%
  mutate(growth_per_capita = (adjusted_gdp_per_captia/lag(adjusted_gdp_per_captia) - 1) * 100) %>%
  select_all()
names(df)


# Finding NA's ----
for (name in names(df)) {
  print(c(name, sum(is.na(df[name]))))
  ## calculating percentage change generates NA since for the first data we do not have a preceeding data.
  ## Using this information we can clean the data when we try to do any statistical test
}

# Clean Data where necesssay using this template ----
#df <- df[which(df$classified_as != ".."),]
#df <- df[which(!is.na(df$consumption_co2_emission_percent_change)),]
#df <- df[which(!is.na(df$total_gdp_dollars)),]
#df <- df[which(!is.na(df$growth_per_capita)),]
#df <- df[which(!is.infinite(df$consumption_co2_emission_percent_change)),]


for (name in names(df)) {
  print(c(name, sum(is.na(df[name]))))
  ## all na have been dropped
}

# Descriptive Statistics and Box Blot with two sample t-test ----

unique(df$classified_as)
## "LM" "L"  "UM" "H"
## Lower Middle Income
## Lower Income
## Upper Middle Income
## Higher Income

clean_classes <- df[which(df$classified_as != ".."),]
percent_change_data <- clean_classes[
  which(
    !(is.na(clean_classes$consumption_co2_emission_percent_change)
      &
      !is.infinite(clean_classes$consumption_co2_emission_percent_change)
    )
     ),
]

change_l <- percent_change_data[which(percent_change_data$classified_as == "L"), 'consumption_co2_emission_percent_change']
change_lm <- percent_change_data[which(percent_change_data$classified_as == "LM"), 'consumption_co2_emission_percent_change']
change_um <- percent_change_data[which(percent_change_data$classified_as == "UM"), 'consumption_co2_emission_percent_change']
change_h <- percent_change_data[which(percent_change_data$classified_as == "H"), 'consumption_co2_emission_percent_change']

x <- "consumption_co2_emission_percent_change"
hist(change_l$consumption_co2_emission_percent_change, main = c(x, "Low Income"), xlab = "Percent Change", breaks = sqrt(length(change_l$consumption_co2_emission_percent_change)))
hist(change_h$consumption_co2_emission_percent_change, main = c(x, "High Income"), xlab = "Percent Change", breaks = sqrt(length(change_h$consumption_co2_emission_percent_change)))

boxplot(
  c(change_l, change_lm, change_um, change_h),
  ylim = c(-100, 100),
  col = c("red", "orange", "yellow", "green"),
  names = c("Low", "Lower Mid", "Upper Mid", "High"),
  ylab = "consumption_co2_emission_percent_change",
  xlab = "Income Levels as reported by World Bank"
)

myData <- change_l$consumption_co2_emission_percent_change
hist(myData, breaks = sqrt(length(myData)), freq = FALSE, main = "Consumption CO2 tonnes per capita Distribution in Low Income Countries", xlab = "Consumption CO2 Emissions tonnes per Capita")
param <- MASS::fitdistr(myData, "normal")
x <- seq(min(myData), max(myData), length.out = length(myData))
curve(dnorm(x, mean = param$estimate[1], sd = param$estimate[2]), add = TRUE, col = "red")
legend("topright", legend = "normal", col = "red", lty = 1)


x <- change_l$consumption_co2_emission_percent_change
y <- change_h$consumption_co2_emission_percent_change
length(y) + length(x) - 2
var.test(x, y, alternative = "two.sided", conf.level = 0.99)
z <- mean(x) - mean(y)
print(z)
t.test(x, y, alternative = "greater", conf.level = 0.95, var.equal = TRUE)

clean_classes <- clean_classes[which(!is.na(clean_classes$consumption_co2_emission_tonnes_per_capita) & !is.infinite(clean_classes$consumption_co2_emission_tonnes_per_capita)),]

l_perc <- clean_classes[which(clean_classes$classified_as == "L"), 'consumption_co2_emission_tonnes_per_capita']
lm_perc <- clean_classes[which(clean_classes$classified_as == "LM"), 'consumption_co2_emission_tonnes_per_capita']
um_perc <- clean_classes[which(clean_classes$classified_as == "UM"), 'consumption_co2_emission_tonnes_per_capita']
h_perc <- clean_classes[which(clean_classes$classified_as == "H"), 'consumption_co2_emission_tonnes_per_capita']

boxplot(
  c(l_perc, lm_perc, um_perc, h_perc),
  col = c("red", "orange", "yellow", "green"),
  names = c("Low", "Lower Mid", "Upper Mid", "High"),
  ylab = "consumption_co2_emissions_tonnes per capita",
  xlab = "Income Levels as reported by World Bank",
  horizontal = TRUE
)

myDataY <- h_perc$consumption_co2_emission_tonnes_per_capita
hist(myDataY, breaks = sqrt(length(myDataY)), freq = FALSE, main = "Consumption CO2 tonnes per capita Distribution in High Income Countries", xlab = "Consumption CO2 Emissions tonnes per Capita")
curve(dexp(x, rate = 1/mean(h_perc$consumption_co2_emission_tonnes_per_capita)), add = TRUE, col = "blue")
param <- MASS::fitdistr(myDataY, "gamma")
x <- seq(min(myData), max(myData), length.out = length(myData))
curve(dgamma(x, shape = param$estimate[1], rate = param$estimate[2]), add = TRUE, col = "red")
legend("topright", legend = c("gamma", "exponential"), col = c("red", "blue"), lty = 1)

qqPlot(myDataY, distribution = "exp", ylab = "Consumption CO2 emissions per Capita", main = "High Income Level Countries", xlab = "Exponential Quantiles", envelope = FALSE)
qqPlot(myDataY, distribution = "norm", ylab = "Consumption CO2 emissions per Capita", main = "High Income Level Countries", xlab = "Normal Quantiles", envelope = FALSE)

# Are we emitting more and more CO2 per year, hypothesis test ----
# per_co2 = N(mean(), sd())
# H0 = mean = 0; H1 = mean > 0
testOn <- "consumption_co2_emission_percent_change"
clean_co2_percent_change <- df[which(df[testOn] != Inf & !is.na(df[testOn])), testOn]
summary(clean_co2_percent_change)
alpha <- 0.01 ## we want to be really really sure about this. We don't want fake news.
myData <- clean_co2_percent_change$consumption_co2_emission_percent_change
n <- length(myData)
t.test(myData, mu = 0, alternative = "greater", conf.level = 1-alpha)
power.t.test(n = n, delta = mean(myData), sd = sd(myData), sig.level = alpha, type = "one.sample", alternative = "one.sided")

# Create CLEAN_DF for next part of analysis ----
clean_df <- df[
  which(
    df$consumption_co2_emission_percent_change != Inf &
      !is.na(df$consumption_co2_emission_percent_change) &
      df$gdp_percent_change != Inf &
      !is.na(df$gdp_percent_change) &
      df$population_percent_change != Inf &
      !is.na(df$population_percent_change) &
      df$classified_as != ".."
  ),]

# Sign Test ----
data_2017 <- clean_df[which(clean_df$year == "X2017"),]
data_2016 <- clean_df[which(clean_df$year == "X2016"),]

testOn <- "consumption_co2_emission_tonnes"

shapiro.test(data_2016$consumption_co2_emission_tonnes)
shapiro.test(data_2017$consumption_co2_emission_tonnes)
SIGN.test(data_2017$consumption_co2_emission_tonnes, data_2016$consumption_co2_emission_tonnes, alternative = "greater")

# Multivariable Linear Regression ----
boxpltGDP <- boxplot(clean_df$gdp_percent_change)
boxpltPop <- boxplot(clean_df$population_percent_change)
boxpltCO2 <- boxplot(clean_df$consumption_co2_emission_percent_change)

model_data <- clean_df[which(boxpltGDP$stats[1] < clean_df$gdp_percent_change & clean_df$gdp_percent_change < boxpltGDP$stats[5]),]
model_data <- model_data[which(boxpltPop$stats[1] < model_data$population_percent_change & model_data$population_percent_change < boxpltPop$stats[5]),]
model_data <- model_data[which(boxpltCO2$stats[1] < model_data$consumption_co2_emission_percent_change & model_data$consumption_co2_emission_percent_change < boxpltCO2$stats[5]),]

ggplot(model_data, aes(x = population_percent_change, y = consumption_co2_emission_percent_change, color = classified_as)) +
  geom_point() +
  geom_abline()

ggplot(model_data, aes(x = gdp_percent_change, y = consumption_co2_emission_percent_change, color = classified_as)) +
  geom_point() +
  geom_abline()

simpleLinM1 <- lm(model_data$consumption_co2_emission_percent_change ~ model_data$population_percent_change, data = model_data)
simpleLinM2 <- lm(model_data$consumption_co2_emission_percent_change ~ model_data$gdp_percent_change, data = model_data)
multiLimM1 <- lm(model_data$consumption_co2_emission_percent_change ~ model_data$population_percent_change + model_data$gdp_percent_change)

summary(simpleLinM1)
plot(model_data$population_percent_change, simpleLinM1$residuals, xlab = "Population Percent Change", ylab = "Residuals", main = "Residual Plot")
shapiro.test(simpleLinM1$residuals)

summary(simpleLinM2)
plot(model_data$gdp_percent_change, simpleLinM2$residuals, xlab = "GDP Percent Change", ylab = "Residuals", main = "Residual Plot")
shapiro.test(simpleLinM2$residuals)

summary(multiLimM1)
shapiro.test(multiLimM1$residuals)
plot(model_data$population_percent_change, multiLimM1$residuals, xlab = "Population Percent Change", ylab = "Residuals", main = "Multi Residual Plot")
plot(model_data$gdp_percent_change, multiLimM1$residuals, xlab = "GDP Percent Change", ylab = "Residuals", main = "Multi Residual Plot")
