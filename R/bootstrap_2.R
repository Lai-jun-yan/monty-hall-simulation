source("01_create_first_car_function.R")
source("02_analysis.R")
source("03_adjust_chage_door_odd.R")
source("second_control_var.R")



###有一百個人玩了遊戲，每個人都換門了1000次###

# 換門
change_unknown_control = function(x){
  
  x_1 = x[x==1]
  y_1 = numeric(length(x_1))
  x_2 = x[x!=1]
  open_goat = r_2_control(n = length(x_2))
  y = c(y_1,open_goat)
  
  return(y)
  
}

#MC
n <- 1000
y <- numeric(n)

for(i in 1:n){
  x <- first_car_control_var(n = 1000)
  y[i] <- mean(change_unknown_control(x))
}


people = c()

for (i in 1:100){
  
  n <- 1000
  y <- numeric(n)
  
  for(j in 1:n){
    x <- first_car_control_var(n = 1000)
    y[j] <- mean(change_unknown_control(x))
  }
  
  people[i] = mean(y)
  
}

people

library(boot)
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