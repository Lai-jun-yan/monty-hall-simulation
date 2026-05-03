monty_hall <- function(switch = TRUE) {
  doors <- 1:3
  prize <- sample(doors, 1)
  choice <- sample(doors, 1)
  
  remaining <- doors[doors != choice & doors != prize]
  opened <- sample(remaining, 1)
  
  if (switch) {
    choice <- doors[doors != choice & doors != opened]
  }
  
  return(choice == prize)
}