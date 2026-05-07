source("01_create_first_car_function.R")

set.seed(1)

###參賽者已經選完了第一次的門，接下來要選擇第二次的門
#先創立換門的函數

x = first_car(n = 1)

change_door = function(x){
  
  if(x == 0){
    y = 1 # 因為主持人知道哪裡有羊
  }else if(x == 1){
    y = 0
  }
  
  return(y)
  
}

#看一下換門之後的勝率
y = c()

for (i in c(1:10000)){
  x = first_car(n = 1)
  
  y[i] = change_door(x)
  
}

mean(y)
var(y)

###若用control之後產生隨機變數###
y_con = matrix(0,nrow = 5000,ncol = 2)

for (i in 1:5000){
  x_con = first_car_control_var(n=2)
  x = c()
  x[1] = change_door(x_con[1])
  x[2] = change_door(x_con[2])
  for (j in 1:2){
      y_con[i,j] = x[j] 
  }
}

y = c(y_con[,1],y_con[,2])

mean(y)
var(y)

###sample mean distribution###
# 先控制var
change_door_odd_with_control = function(){
  z = c()
  for (k in 1:1000){
    
    y_con = matrix(0,nrow = 500,ncol = 2)
    
    for (i in 1:500){
      
      x_con = first_car_control_var(n=2)
      x = c()
      x[1] = change_door(x_con[1])
      x[2] = change_door(x_con[2])
      for (j in 1:2){
        y_con[i,j] = x[j] 
      }
    }
    
    y = c(y_con[,1],y_con[,2])
    z[k] = mean(y)
    
  }
  result = c(mean(z),var(z))
}

x = change_door_odd_with_control()
x

# 不控制var
change_door_odd = function(){
  z = c()
  for (j in 1:1000){
    x = first_car(n=1000)
    y = c()
    for (i in 1:1000){
      y[i] = change_door(x[i])
    }
    z[j] = mean(y)
  }
  result = c(mean(z),var(z))
}

y = change_door_odd()

Compare_sample_odd_mean_distribution = rbind(
  "控制" = x,
  "不控制" = y
)

Compare_sample_odd_mean_distribution
