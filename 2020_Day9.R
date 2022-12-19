#day 9:
library(tidyverse)
setwd("C:/Users/15517/Downloads")

d9 <- as.numeric(readLines("slider.txt")) #1000 in length 


m <- rep(1,25)
for(i in 26:1000){
  m[i] <- helper(d9[(i-25):(i-1)], d9[i])
  if(m[i] == 0 ){
    break
  }
  
}

d9[length(m)] #answer  - position 660 and 1492208709

helper <- function(x,curr){
  g <- c()
  for(i in 1:length(x))
    for(j in 1:length(x))
      if(x[i]+x[j] == curr && i != j)
        g <- append(g,1)
  return(ifelse(sum(g) >= 1, 1,0))
}


#part 2:

part2 <- function(x){
  m <- rep(1,(x-1))
  for(i in x:660){
    m[i] <- sum(d9[(i-x+1):i])
  }
  return(max(d9[(which(m==1492208709)-x+1):(which(m==1492208709))])+min(d9[(which(m==1492208709)-x+1):(which(m==1492208709))]))
  if(any(m == 1492208709)){
    return(1)
  } else {
    return(0)
  }
  
}
which(map_dbl(1:660, ~part2(.))==1) # 1 and 17, 1 seems trivial so examine 17 and verify this by adding in return(m) in "part2" function 
part2(17) #answer ---- 238243506