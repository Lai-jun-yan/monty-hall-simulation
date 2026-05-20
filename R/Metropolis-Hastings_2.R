first_car_control_var = function(n = 1000){
  p = 1/3 # 三門問題中，一開始選到跑車的機率是1/3
  u = runif(n/2)
  u_2 = 1-u # 使用chapter6 27頁的方法
  U = c(u,u_2)
  x = as.integer(U > 1-p)
  return(x)
}


# 用前面定義的函數模擬真實換門之後的勝率

x_con = first_car_control_var(n = 1000)
  
y_con = change_unknown_control(x_con)
  

# 定義posterior
log_posterior = function(theta, y, a = 1, b = 1){
  
  if(theta <= 0 || theta >= 1){
    return(-Inf)
  }
  
  k = sum(y)
  n = length(y)
  
  log_likelihood =
    k * log(theta) +
    (n-k) * log(1-theta)
  
  log_prior =
    (a-1) * log(theta) +
    (b-1) * log(1-theta)
  
  return(log_likelihood + log_prior)
}


# MH sampler
mh_sampler = function(y, a = 1, b = 1, n_iter = 10000, sigma = 0.05){
  
  samples = numeric(n_iter)
  
  theta_current = (2/3) # 設定與一般人想像的一樣的機率，換門與不換門的機率是一樣
  
  for(i in 1:n_iter){
    
    # proposal
    theta_prop = theta_current + rnorm(1,0,sigma)# 移動的步伐
    
    # reject outside [0,1]
    if(theta_prop <= 0 || theta_prop >= 1){
      
      samples[i] = theta_current
      next
    }
    
    # acceptance ratio
    
    log_r =
      log_posterior(theta_prop, y, a, b) -
      log_posterior(theta_current, y, a, b)
    
    alpha = min(1, exp(log_r)) # 新的theta如果比較好，就直接接受新的theta;
    # 若比較差，則根據一定比例去決定是否移動。
    
    if(runif(1) < alpha){
      theta_current = theta_prop
    }
    
    samples[i] = theta_current
  }
  
  return(samples)
}

## 畫圖
samples = mh_sampler(y_con, a = 10, b = 10, sigma = 0.05)# sigma可以調整，最好是0.05
# a、b皆可以改

# 震盪圖
plot(samples,
     type="l",
     col="blue",
     lwd=1,
     xlab="Iteration",
     ylab=expression(theta),
     main="MH Trace Plot")

abline(h=1/3,
       col="red",
       lwd=2)

# 分布圖

hist(samples,
     breaks = 50,
     probability = TRUE)

abline(v = 1/3,col="red",lwd=2)










