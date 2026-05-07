###以下模擬一開始選到跑車的機率###

#使用inverse function生成Bernoulli(p = 1/3)的隨機變數

n = 10000 # 變數的數量
p = 1/3 # 三門問題中，一開始選到跑車的機率是1/3
u = runif(n)
x = as.integer(u > 1-p)


#透過R內建的生成Bernoulli隨機變數的function進行驗證

x_ver = rbinom(n, size = 1, prob = p)
mean(x_ver)
var(x_ver)

result_1 = c(mean(x),var(x)) # inverse function產生的結果
result_2 = c(mean(x_ver),var(x_ver)) # 內建函數的結果

# 驗證
result = rbind(
  inverse_method = result_1,
  "R裡面內建的函數"  = result_2
)

colnames(result) = c("mean", "var")
result

###以下為結果###
# > result set.seed(1)
#                   mean    var
# inverse_method  0.3367 0.2233554
# R裡面內建的函數 0.3338 0.2223998

###寫出根據inverse function產生一開始選到跑車的function###

first_car = function(n = 10000){
  p = 1/3 # 三門問題中，一開始選到跑車的機率是1/3
  u = runif(n)
  x = as.integer(u > 1-p)
  return(x)
}

# 驗證first_car

mean(x)
var(x)

###嘗試控制var###

#使用inverse function生成Bernoulli(p = 1/3)的隨機變數

first_car_control_var = function(n = 10000){
  p = 1/3 # 三門問題中，一開始選到跑車的機率是1/3
  u = runif(n/2)
  u_2 = 1-u # 使用chapter6 27頁的方法
  U = c(u,u_2)
  x = as.integer(U > 1-p)
  return(x)
}
y = first_car_control_var()

# 驗證
mean(y)
var(y)

# 若是用R內建的柏努力產生隨機變數，則該sample_mean的分布為何
R_defuilt_distribution = function(n = 10000){
  y = c()
  for (i in 1:10000){
    x_ver = rbinom(n, size = 1, prob = p)
    y[i] = mean(x_ver)
  }
  result = c(mean(y),var(y))
  return(result)
}

R_defuilt = R_defuilt_distribution()


# 若是使用inverse function產生隨機變數，則該sample_mean的分布為何
Inverse_distribution = function(n = 10000){
  y = c()
  for (i in 1:10000){
    x = first_car(n)
    y[i] = mean(x)
  }
  result= c(mean(y),var(y))
  return(result)
}

Inverse = Inverse_distribution()

# 若是使用inverse function且控制var後產生隨機變數，則該sample_mean的分布為何
control_distribution = function(n = 10000){
  y = c()
  for (i in 1:10000){
    x = first_car_control_var(n = 10000)
    y[i] = mean(x)
  }
  result = c(mean(y),var(y))
  return(result)
}

control = control_distribution()

Compare_sample_mean_distribution = rbind(
  
  "R內建的函數" = R_defuilt,
  "Inverse function" = Inverse,
  "Inverse function with controling var" = control
  
)

colnames(Compare_sample_mean_distribution) = c("Mean","Var")

Compare_sample_mean_distribution














