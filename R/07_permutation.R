source("01_create_first_car_function.R")
source("02_analysis.R")

# 不換門而贏的機率

do_not_change_door = function(x){
  
  if(x == 0){
    y = 0 # 因為主持人知道哪裡有羊
  }else if(x == 1){
    y = 1
  }
  
  return(y)
  
}

#看一下不換門之後的勝率
y = c()

x = first_car_control_var(n = 1000)

for (i in c(1:1000)){
  
  y[i] = do_not_change_door(x[i])
  
}

mean(y)
var(y)

###做permutation###

#產生換門的獲勝機率分布
change_door_odd_with_control = function(){
  z = c()
  for (k in 1:1000){
    
    y_con = c()
    
    x_con = first_car_control_var(n = 1000)
    
    for (i in 1:1000){
      
      y_con[i] = change_door(x_con[i])
      
    }
    
    z[k] = mean(y_con)
    
  }
  
  result = list(mean = mean(z), var = var(z), distribution = z)
  
}



x = change_door_odd_with_control()

#產生不換門的獲勝的機率分布
do_not_change_door_odd_with_control = function(){
  z = c()
  for (k in 1:1000){
    
    y_con = c()
    
    x_con = first_car_control_var(n = 1000)
    
    for (i in 1:1000){
      
      y_con[i] = do_not_change_door(x_con[i])
      
    }
    
    z[k] = mean(y_con)
    
  }
  
  result = list(mean = mean(z), var = var(z), distribution = z)
  
}



y = do_not_change_door_odd_with_control()

# 產生虛無假設
z <- c(as.numeric(x$distribution), as.numeric(y$distribution))

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
obs <- mean(x$distribution) - mean(y$distribution)

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
        
# 看一下power，要好好想





