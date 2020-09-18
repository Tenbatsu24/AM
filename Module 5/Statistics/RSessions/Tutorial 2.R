# Compare two estimators ----

n = 10
lambda = 2

?rpois
data = rpois(n, lambda=lambda)

lambda_hat_relative_freq = sum(data == 0)/n
lambda_hat_sample_mean = exp(-mean(data))

p_true = exp(-lambda)

err_relative_freq = 0
err_sample_mean = 0
nr_iter = 1000

for (i in 1:nr_iter) {
  data = rpois(n, lambda=lambda)
  lambda_hat_relative_freq = sum(data == 0)/n
  lambda_hat_sample_mean = exp(-mean(data))
  err_sample_mean = err_sample_mean + (lambda_hat_sample_mean-p_true)^2
  err_relative_freq = err_relative_freq + (lambda_hat_relative_freq-p_true)^2
}

avg_error_sample_mean = err_sample_mean/nr_iter
avg_error_relative_freq = err_relative_freq/nr_iter

# Read Data ----

table = read.csv(file.choose())
attach(table)
names(table)
head(table)
View(table)

is.na(Display.Value)
is.na(Numeric)
sum(is.na(Numeric))

value_summary = summary(table["Display.Value"])

fertiltiy_df = read.csv(file.choose())
attach(fertiltiy_df)
Country.or.Area
fertiltiy_df[Country.or.Area == "Netherlands", seq(from=2, to=length(names(fertiltiy_df)), by=2)]
