# Chapter 3 ----
vec = c(30.1, 32.7,22.5,27.5,27.7, 29.8, 31.4, 31.2, 24.3, 26.4, 22.8, 29.1, 33.4, 32.5, 21.7)

length(vec)
summary(vec)
var(vec)

?sd
mu = mean(vec)
c = 2.145
s = sd(vec)
s_sq = var(vec)
n = 15
df = n - 1

interval = c(mu - c*(s)/sqrt(n), mu + c*(s)/sqrt(n))
interval

(12/(10*0.02))^2
