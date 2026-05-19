source("01_create_first_car_function.R")
source("02_analysis.R")
source("03_adjust_chage_door_odd.R")

###第二次控制

# 第二次control

r_2_control = function(n = 1000){
  
  p = 1/2 
  u = runif(n/2)
  u_2 = 1-u 
  U = c(u,u_2)
  x = as.integer(U > 1-p)
  return(x)
  
  
}

change_unknown_control = function(x){
  
  x_1 = x[x==1]
  y_1 = numeric(length(x_1))
  x_2 = x[x!=1]
  open_goat = r_2_control(n = length(x_2))
  y = c(y_1,open_goat)
  
  return(y)
  
}

#(3-2)如果主持人不知跑車在哪，比較三種情況（r內建、inverse method、antithetic）sample mean 分布----
# R 內建 Bernoulli
R_default_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

# inverse function
Inverse_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car(n, p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

# inverse function + controlling var
Control_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

R_default_unknown_control <- R_default_unknown_distribution(n, B, p)
Inverse_unknown_control <- Inverse_unknown_distribution(n, B, p)
Control_unknown_control <- Control_unknown_distribution(n, B, p)

Compare_unknown <- rbind(
  "R內建的函數" = R_default_unknown_control,
  "Inverse function" = Inverse_unknown_control,
  "Inverse function with controlling var" = Control_unknown_control
)

colnames(Compare_unknown) <- c("Mean", "Var")

Compare_unknown

##畫分布圖
#(3-2)如果主持人不知跑車在哪，比較三種情況（r內建、inverse method、antithetic）sample mean----

# R 內建 Bernoulli
R_default_unknown_y_control <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  
  return(y)
}
R_default_unknown_y_control <- R_default_unknown_y_control(n, B, p)
hist(R_default_unknown_y_control,
     main = "Sampling Distribution of the Estimated Winning Probability 
     (R Bernoulli Simulation)",
     xlab = "Estimated winning probability",
     probability = TRUE)

abline(v = mean(R_default_unknown_y_control),
       col = "yellow",
       lty = 2,
       lwd = 2)
text(x = mean(R_default_unknown_y_control) + 0.002,
     y = max(hist(R_default_unknown_y_control, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(R_default_unknown_y_control), 4)),
     pos = 4)


# Inverse function
Inverse_unknown_y_control <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car(n, p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  return(y)
}
Inverse_unknown_y_control <- Inverse_unknown_y_control(n, B, p)
hist(Inverse_unknown_y,
     main = "Sampling Distribution of the Estimated Winning Probability 
     (Inverse function)",
     xlab = "Estimated winning probability",
     probability = TRUE)
abline(v = mean(Inverse_unknown_y_control),
       col = "yellow",
       lty = 2,
       lwd = 2)
text(x = mean(Inverse_unknown_y_control) + 0.002,
     y = max(hist(Inverse_unknown_y_control, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(Inverse_unknown_y_control), 4)),
     pos = 4)


# Inverse function + antithetic variates
Control_unknown_y_control <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  return(y)
}
Control_unknown_y_control <- Control_unknown_y_control(n, B, p)
hist(Control_unknown_y,
     main = "Sampling Distribution of the Estimated Winning Probability
     (Inverse function with control variance)",
     xlab = "Estimated winning probability",
     probability = TRUE)
abline(v = mean(Control_unknown_y_control),
       col = "yellow",
       lty = 2,
       lwd = 2)
text(x = mean(Control_unknown_y_control) + 0.002,
     y = max(hist(Control_unknown_y_control, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(Control_unknown_y_control), 4)),
     pos = 4)

#variance reduction(只控制第一次vs兩次皆控制)
variance_reduction_3 = function(x,y){
  
  result = ((var(x) - var(y))/var(x))*100
  
  cat(result, "%\n")
  
  
}

#只有第1次控制
R_default_unknown_y = R_default_unknown_y(n, B, p)
Inverse_unknown_y <- Inverse_unknown_y(n, B, p)
Control_unknown_y <- Control_unknown_y(n, B, p)

x = list(R_default_unknown_y,Inverse_unknown_y,Control_unknown_y)

#兩次皆有控制
R_default_unknown_y_control <- R_default_unknown_y_control(n, B, p)
Inverse_unknown_y_control <- Inverse_unknown_y_control(n, B, p)
Control_unknown_y_control <- Control_unknown_y_control(n, B, p)

y = list(R_default_unknown_y_control,Inverse_unknown_y_control,Control_unknown_y_control)

##兩兩比較
for(i in 1:3){
  
  variance_reduction_3(x[[i]],y[[i]])
  
}

#畫圖

for (i in 1:3){
  
  d1 <- density(x[[i]])
  d2 <- density(y[[i]])
  plot(d1,
       main = "Comparison of two Probability Distributions",
       xlab = "Probability",
       ylab = "Density",
       col = "red",
       lwd = 2,
       xlim = range(c(d1$x, d2$x)),
       ylim = c(0, max(c(d1$y, d2$y)) * 1.1))
  lines(d2,
        col = "blue",
        lwd = 2,
        lty = 2)
  legend("topright",
         legend = c("use nothing to control variation", "control variation"),
         col = c("red", "blue"),
         lwd = 2,
         lty = c(1, 2))
  
}

###三個皆為第二次控制的分布
d1 <- density(R_default_unknown_y_control)
d2 <- density(Inverse_unknown_y_control)
d3 <- density(Control_unknown_y_control)
plot(d1,
     main = "Comparison of three Probability Distributions",
     xlab = "Probability",
     ylab = "Density",
     col = "red",
     lwd = 2,
     xlim = range(c(d1$x, d2$x, d3$x)),
     ylim = c(0, max(c(d1$y, d2$y, d3$y)) * 1.1))
lines(d2,
      col = "blue",
      lwd = 2,
      lty = 2)
lines(d3,
      col = "darkgreen",
      lwd = 2,
      lty = 3)
legend("topright",
       legend = c("rbinom", "inverse function", "control variance"),
       col = c("red", "blue", "darkgreen"),
       lwd = 2,
       lty = c(1, 2, 3))













