#day 8:
library(tidyverse)
setwd("C:/Users/15517/Downloads")
Day8 <- tibble(readLines("2020_d8.txt")) %>% rename(combined = `readLines("2020_d8.txt")`) %>% separate(col = combined, into=c("instructions", "operation"), sep = " ") %>% mutate(operation = as.double(operation))


keep <- 1 # initialize row position along "instructions" 
value <- 0 #initialize global cumulative value 
while(n_distinct(keep) == length(keep)){
  
  if(Day8$instructions[tail(keep,1)] == "acc"){
    value <- value + Day8$operation[tail(keep,1)]
    keep <- append(keep, (tail(keep,1) + 1))        #go down 1 after increasing our global value 
  }
  else if(Day8$instructions[tail(keep,1)] == "jmp"){
    keep <- append(keep, tail(keep,1) + Day8$operation[tail(keep,1)]) # jump based on operation value 
  }
  else if(Day8$instructions[tail(keep,1)] == "nop"){
    keep <- append(keep, (tail(keep,1) + 1)) #go down 1, do nothing else
    
  }
}
value # answer ------1200

#part2: 
#setTimeLimit(1) ---- if a certain change doesn't finish within x seconds determine it to not be the answer 

changer <- function(change){
Day8$instructions[change] <- ifelse(Day8$instructions[change] == "jmp", "nop", "jmp")
  keep <- 1
  value <- 0 
  i <- 0 
  while(i <= 2000){
  if(Day8$instructions[tail(keep,1)] == "acc"){
    value <- value + Day8$operation[tail(keep,1)]
    keep <- append(keep, (tail(keep,1) + 1))        #go down 1 after increasing our global value 
    i <- i + 1 
  }
  else if(Day8$instructions[tail(keep,1)] == "jmp"){
    keep <- append(keep, tail(keep,1) + Day8$operation[tail(keep,1)])
    i <- i + 1 
  }
  else if(Day8$instructions[tail(keep,1)] == "nop"){
    keep <- append(keep, (tail(keep,1) + 1)) #go down 1, do nothing else
    i <- i + 1 
  }
    
  }
  ifelse((sum(tail(keep,10))*0.1 == tail(keep,1)),1,0)
  } 

  


test <- which(Day8$instructions %in% c("jmp", "nop"))
map_dbl(head(test,100), ~changer(.)) #328 this index can't execute function, means that it reaches the end and tries to jump to 644, out of bounds 

Day8$instructions[328] <- "nop" 
keep <- 1
value <- 0 
while(i <= 1000){
  
  if(Day8$instructions[tail(keep,1)] == "acc"){
    value <- value + Day8$operation[tail(keep,1)]
    keep <- append(keep, (tail(keep,1) + 1)) 
    i <- i + 1 #
  }
  else if(Day8$instructions[tail(keep,1)] == "jmp"){
    keep <- append(keep, tail(keep,1) + Day8$operation[tail(keep,1)]) # jump based on operation value 
    i <- i + 1
  }
  else if(Day8$instructions[tail(keep,1)] == "nop"){
    keep <- append(keep, (tail(keep,1) + 1)) #go down 1, do nothing else
    i <- i + 1
    
  }
}
value #answer ------------------ 1023




