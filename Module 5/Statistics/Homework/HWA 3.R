# Q2 ----
alpha = 0.05
n = 25
p = 0.1
p1 = 0.15

rr = qbinom(1-alpha, n, p)
p_value = pbinom(rr, n, p)
pow = 1-pbinom(rr-1, n, p1)

# Q3 ----
alpha = 0.05
n = 900
p = 0.1
p1 = 0.15

## 1 ----
rr = qbinom(1-alpha, n, p)
p_value = pbinom(rr, n, p)

## 2 ----
t2err = pbinom(rr-1, n, p1)

## 3 ----
test = 100
p_val = pbinom(test, n, p)
p_val = 1 - p_val
## 4 ----

c = qnorm(0.90)+(0.3*qnorm(0.95))/sqrt(0.1275)

# Q4
n = ((1/5)*(30*qnorm(0.95)/sqrt(15*85) - qnorm(0.10))*sqrt(15*85))^2
