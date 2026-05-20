source("01_create_first_car_function.R")
source("02_analysis.R")
source("03_adjust_chage_door_odd.R")
source("second_control_var.R")

## 做jacknife

n = length(people)

# 存 jackknife estimate
jack_mean = numeric(n)

# leave-one-out
for(i in 1:n){
  
  jack_sample = people[-i]
  
  jack_mean[i] = mean(jack_sample)
}

mean(jack_mean)

# 算SE
jack_se = sqrt(
  ((n - 1) / n) *
    sum((jack_mean - mean(jack_mean))^2)
)

jack_se

# 算cI
theta_hat = mean(people)

ci_lower = theta_hat - 1.96 * jack_se
ci_upper = theta_hat + 1.96 * jack_se

v1 = c(ci_lower, ci_upper)

xrange <- range(c(jack_mean, v1, theta_hat))

# 畫圖
hist(
  jack_mean,
  probability = TRUE,
  breaks = 30,
  main = "Jackknife Distribution",
  xlab = "Jackknife Mean",
  xlim = xrange
)

# density curve
lines(
  density(jack_mean),
  lwd = 2,
  col = "blue"
)

# 原始 sample mean
abline(
  v = mean(people),
  col = "red",
  lwd = 2
)

# 95% CI
abline(
  v = v1[1],
  col = "darkgreen",
  lwd = 4,
  lty = 2
)

abline(
  v = v1[2],
  col = "darkgreen",
  lwd = 4,
  lty = 2
)

# legend
legend(
  "topright",
  legend = c("Density", "Sample Mean", "95% CI"),
  col = c("blue", "red", "darkgreen"),
  lwd = 2,
  lty = c(1, 1, 2)
)

# 輸出表格
jack_mean_bar <- mean(jack_mean)

jack_bias <- (n - 1) * (jack_mean_bar - theta_hat)
jack_bias


