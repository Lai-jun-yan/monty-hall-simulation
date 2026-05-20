source("01_create_first_car_function.R")
source("02_analysis.R")
source("03_adjust_chage_door_odd.R")
source("second_control_var.R")

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


mean(y)
var(y)

# 不換門
do_not_change_unknown_control = function(x){
  
  x_1 = x[x==1]
  y_1 = x_1
  x_2 = x[x!=1]
  open_goat = numeric(length(x_2))
  y = c(y_1,open_goat)
  
  return(y)
  
}

#MC
n <- 1000
y <- numeric(n)

for(i in 1:n){
  x <- first_car_control_var(n = 1000)
  y[i] <- mean(do_not_change_unknown_control(x))
}


mean(y)
var(y)

# permutation
change = function(){
  #MC
  n <- 1000
  y <- numeric(n)
  
  for(i in 1:n){
    x <- first_car_control_var(n = 1000)
    y[i] <- mean(change_unknown_control(x))
  }
  
  return(y)
}

do_not_change = function(){
  
  #MC
  n <- 1000
  y <- numeric(n)
  
  for(i in 1:n){
    x <- first_car_control_var(n = 1000)
    y[i] <- mean(do_not_change_unknown_control(x))
  }
  
  return(y)
}

x = change()
y = do_not_change()


# 產生虛無假設
z <- c(as.numeric(x), as.numeric(y))

idx <- sample(length(z), replace = FALSE)

z1 <- z[idx[1:1000]]
z2 <- z[idx[1001:length(z)]]


# 產生分布
permutation = function(){
  result <- c()
  for (i in 1:10000){
    
    idx <- sample(length(z), replace = FALSE)
    
    z1 <- z[idx[1:1000]]
    z2 <- z[idx[1001:length(z)]]
    
    result[i] <- mean(z1) - mean(z2)
  }
  
  return(result)
}

permutation_dist = permutation()

# 計算p-value
obs <- mean(x) - mean(y)

p_value <- mean(abs(permutation_dist) >= abs(obs))
p_value

# 95%機率分布
ci <- quantile(permutation_dist, c(0.025, 0.975))
ci

# 畫圖
xrange <- range(c(permutation_dist, obs, ci))

hist(permutation_dist,
     probability = TRUE,
     breaks = 30,
     main = "Permutation Null Distribution",
     xlab = "Mean Difference",
     xlim = xrange)

lines(density(permutation_dist),
      lwd = 2,
      col = "blue")

abline(v = ci[1], col = "darkgreen", lwd = 3, lty = 2)
abline(v = ci[2], col = "darkgreen", lwd = 3, lty = 2)

abline(v = obs, col = "red", lwd = 3)

text(
  obs,
  max(density(permutation_dist)$y) * 0.9,
  labels = "p = 0",
  col = "red",
  pos = 4
)

legend("topright",
       legend = c("Null density", "95% CI", "Observed"),
       col = c("blue", "darkgreen", "red"),
       lwd = 2,
       lty = c(1, 2, 1),
       bty = "n")

# 表格
mean(permutation_dist)
mean(x - y)
sd(permutation_dist)














