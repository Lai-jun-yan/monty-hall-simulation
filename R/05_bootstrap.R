source("01_create_first_car_function.R")
source("02_analysis.R")


###有一百個人玩了遊戲，每個人都換門了1000次###

#使用inverse_control做

people = c()

for (i in 1:100){
  
  x = change_door_odd_with_control()
  
  y = as.numeric(x["mean"])
  
  people[i] = y
  
}

people

#library(boot)
# statistic function
boot_fun <- function(data = people, index){
  mean(data[index])
}

result <- boot(
  data = people,
  statistic = boot_fun,
  R = 10000
)

result

# 畫圖
# 95% percentile CI
ci <- quantile(result$t, c(0.025, 0.975))

# 畫圖
hist(
  result$t,
  probability = TRUE,
  breaks = 30,
  main = "Bootstrap Distribution",
  xlab = "Bootstrap Mean"
)

# density curve
lines(
  density(result$t),
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
  v = ci,
  col = "darkgreen",
  lwd = 2,
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





