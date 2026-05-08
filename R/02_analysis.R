source("01_create_first_car_function.R")

###參賽者已經選完了第一次的門，接下來要選擇第二次的門
#先創立換門的函數

change_door = function(x){
  
  if(x == 0){
    y = 1 # 因為主持人知道哪裡有羊
  }else if(x == 1){
    y = 0
  }
  
  return(y)
  
}

# 使用R內建的函數，產生一開始選擇之後，換門的勝率

y_ver = c()

x_ver = rbinom(n = 1000,size = 1,prob = 1/3)

for (i in c(1:1000)){
  
  y_ver[i] = change_door(x_ver[i])
  
}

mean(y_ver)
var(y_ver)

#看一下換門之後的勝率
y = c()

x = first_car(n = 1000)

for (i in c(1:1000)){
  
  y[i] = change_door(x[i])
  
}

mean(y)
var(y)

###若用control之後產生隨機變數，再換門的勝率###
y_con = c()

x_con = first_car_control_var(n = 1000)

for (i in 1:1000){
  
  y_con[i] = change_door(x_con[i])
  
}

mean(y_con)
var(y_con)

###sample mean distribution###
# 先控制var
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
    
    result = c(mean(z),var(z))
    
  }
 


x = change_door_odd_with_control()
x


# 不控制var
change_door_odd = function(){
  z = c()
  for (j in 1:1000){
    
    y = c()
    
    x = first_car(n=1000)
    
    for (i in 1:1000){
      
      y[i] = change_door(x[i])
      
    }
    
    z[j] = mean(y)
    
  }
  
  result = c(mean(z),var(z))
  
}

y = change_door_odd()


# R 內建

change_door_odd_R = function(){
  z = c()
  for (j in 1:1000){
    y_ver = c()
    
    x_ver = rbinom(n = 1000,size = 1,prob = 1/3)
    
    for (i in c(1:1000)){
      
      y_ver[i] = change_door(x_ver[i])
      
    }
    z[j] = mean(y_ver)
  }
  
  result = c(mean(z),var(z))
  
}

z = change_door_odd_R()



Compare_sample_odd_mean_distribution = rbind(
  "控制" = x,
  "不控制" = y,
  "R內建" = z
)

colnames(Compare_sample_odd_mean_distribution) = c("Mean","Var")

Compare_sample_odd_mean_distribution



