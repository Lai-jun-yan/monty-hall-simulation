source("01_create_first_car_function.R")
source("02_analysis.R")

###將分布視覺化###

## 第一次選擇 ##

# 用內建R函數產生第一次選擇，而選到車的機率之估計值分布

R_defuilt = R_defuilt_distribution()

# 畫出 sample mean 的分布
hist(
  R_defuilt$distribution,
  breaks = 30,
  probability = TRUE,
  main = "Distribution of Sampling Distribution of 
  the Estimated Winning Probability (R Bernoulli Simulation)",
  xlab = "Probability"
)

# 加上理論常態曲線
curve(
  dnorm(
    x,
    mean = p,
    sd = sqrt(p*(1-p)/1000)
  ),
  add = TRUE,
  lwd = 2
)

# 用Inverse function產生第一次選擇，而選到車的機率之估計值分布

Inverse = Inverse_distribution()

hist(
  Inverse$distribution,
  breaks = 30,
  probability = TRUE,
  main = "Distribution of Sampling Distribution of 
  the Estimated Winning Probability(Inverse Function)",
  xlab = "Probability"
)

curve(
  dnorm(
    x,
    mean = p,
    sd = sqrt(p*(1-p)/1000)
  ),
  add = TRUE,
  lwd = 2
)

# 用control產生分布

control = control_distribution()

hist(
  control$distribution,
  probability = TRUE,
  breaks = 30,
  main = "Distribution of Sampling Distribution of 
  the Estimated Winning Probability(Inverse Function with 
  Control Variance)",
  xlab = "Probability"
)

curve(
  dnorm(
    x,
    mean = p,
    sd = sqrt(p*(1-p)/1000)
  ),
  add = TRUE,
  lwd = 2
)

# 畫出三種方法的 density curve來比較

plot(
  density(R_defuilt$distribution),
  lwd = 2,
  col = "red",
  main = "Comparison of three Probability Distributions",
  xlab = "Probability",
  ylim = c(0, 120)
)

lines(
  density(Inverse$distribution),
  lwd = 2,
  lty = 2,
  col = "blue"
)

lines(
  density(control$distribution),
  lwd = 2,
  lty = 3,
  col = "darkgreen"
)

legend(
  "topright",
  legend = c(
    "rbinom",
    "inverse function",
    "control variance"
  ),
  col = c("red", "blue", "darkgreen"),
  lwd = 2,
  lty = c(1,2,3)
)

##--------##

## 第二次選擇 ##

# 換門之後而選到跑車的機率分布

z = change_door_odd_R()

# 畫出 sample mean 的分布
hist(
  z$distribution,
  breaks = 30,
  probability = TRUE,
  main = "Distribution of Sampling Distribution of 
  the Estimated Winning Probability (R Bernoulli Simulation)",
  xlab = "Probability"
)

# inverse function
y = change_door_odd()

hist(
  y$distribution,
  breaks = 30,
  probability = TRUE,
  main = "Distribution of Sampling Distribution of 
  the Estimated Winning Probability(Inverse Function)",
  xlab = "Probability"
)

# control
x = change_door_odd_with_control()

hist(
  x$distribution,
  probability = TRUE,
  breaks = 30,
  main = "Distribution of Sampling Distribution of 
  the Estimated Winning Probability(Inverse Function with 
  Control Variance)",
  xlab = "Probability"
)

# 畫出三種方法的 density curve來比較

plot(
  density(z$distribution),
  lwd = 2,
  col = "red",
  main = "Comparison of three Probability Distributions",
  xlab = "Probability",
  ylim = c(0, 120)
)

lines(
  density(y$distribution),
  lwd = 2,
  lty = 2,
  col = "blue"
)

lines(
  density(x$distribution),
  lwd = 2,
  lty = 3,
  col = "darkgreen"
)

legend(
  "topright",
  legend = c(
    "rbinom",
    "inverse function",
    "control variance"
  ),
  col = c("red", "blue", "darkgreen"),
  lwd = 2,
  lty = c(1,2,3)
)
















