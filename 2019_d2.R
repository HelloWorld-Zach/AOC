library(tidyverse)
setwd("C:/Users/15517/Downloads")
Day2 <- as.double(str_split(readLines("2019_d2.txt"), ",")[[1]])
Day2[2:3] <- c(12,2)
           

for(i in seq( from = 1,  to = length(Day2), by = 4 )){             #we need to add one to all positions because R has index of 1 whereas this assumes index of zero, languages like python have that as default 
  if(Day2[i] == 1){
    a <- Day2[i + 1] + 1 
    b <- Day2[i + 2] + 1
    c <- Day2[i + 3] + 1
    Day2[c] <- Day2[a] + Day2[b]
  }
    else if(Day2[i] == 2){
      a <- Day2[i + 1] + 1
      b <- Day2[i + 2] + 1
      c <- Day2[i + 3] + 1
      Day2[c] <- prod(Day2[a], Day2[b])
    }
   else if(Day2[i] == 99){
        break 
      } 
  else {
    next
  }
}
Day2[1] #answer ---------------------> 4138658



#PART 2: (What pair of inputs (between 0 and 99 inclusive) produces output : 19690720 ?)


Pairs <- function(x,y){
  Day2[2:3] <- c(x,y)
  for(i in seq( from = 1,  to = length(Day2), by = 4 )){              
    if(Day2[i] == 1){
      a <- Day2[i + 1] + 1 
      b <- Day2[i + 2] + 1
      c <- Day2[i + 3] + 1
      Day2[c] <- Day2[a] + Day2[b]
    }
    else if(Day2[i] == 2){
      a <- Day2[i + 1] + 1
      b <- Day2[i + 2] + 1
      c <- Day2[i + 3] + 1
      Day2[c] <- prod(Day2[a], Day2[b])
    }
    else if(Day2[i] == 99){
      break 
    } 
    else {
      next
    }
  }
  Day2[1]
}
M <- array(rep(0, 100^2), dim=c(100,100))
for(i in 0:99)
  for(j in 0:99)
M[i,j] <- Pairs(i,j)

which(M==19690720, arr.ind=T) # (72,64)

100*72+64 #ANSWER -----------------------------------------> 7264
