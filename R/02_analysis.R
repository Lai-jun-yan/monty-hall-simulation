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
###主持人不知道門後面有甚麼###
















