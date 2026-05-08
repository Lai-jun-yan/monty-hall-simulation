source("01_create_first_car_function.R")
source("02_analysis.R")

###若改變遊戲規則，換門之後的勝率會是多少###

#若是主持人也不知道哪扇門後面是跑車，而是在參賽者第一次選擇完之後，
##隨機刪除其中一個門，然後再問是否換門

adjust_close_change_door = function(x){
  
  if (x==0){
    
    y = rbinom(n = 1, size = 1, prob = 1/2) # 這邊之後也可以用inverse function替換
    
  }else{
    
    y = 0
    
  }
  
  return(y)
  
}

# 確認換門之後的勝率
y = c()

x = rbinom(n = 10000, size = 1, prob = 1/3) # 這邊之後也可以用inverse function替換

for (i in 1:10000){
  y[i] = adjust_close_change_door(x[i])
}

mean(y)
var(y)
#要想一下為甚麼在這個條件下，換門贏的勝率與一開始選對且不換門的機率等價

## 在主持人不知到門後有甚麼，且第二次選擇中，也沒有開到跑車的門，這時候問參賽者換
## 門之後的勝率為何

x = rbinom(n = 1, size = 1, prob = 1/3)

adjust_open_change_door = function(x){
  if (x == 0){
    
    y_1 = rbinom(n = 1, size = 1, prob = 1/2)
    
    if (y_1 == 0){
      y = "主持人開到了跑車"
    }else{
      y = 1
    }
    
  }else{
    
    y = 0
    
  }
  
  return(y)
  
}

# 確認換門之後的勝率

y_open = c()

x_open = rbinom(n = 10000, size = 1, prob = 1/3) # 這邊之後也可以用inverse function替換

for (i in 1:10000){
  y_open[i] = adjust_open_change_door(x_open[i])
}

y_open = as.numeric(y_open[y_open != "主持人開到了跑車"])

mean(y_open)
var(y_open)







