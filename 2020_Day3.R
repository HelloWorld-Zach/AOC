#day 3:
library(tidyverse)
forest <- tibble(readLines("forest.txt"))%>% rename(trees = `readLines("forest.txt")`)
forest$trees <- str_dup(forest$trees,times=35)

g <- c()
for(i in 1:nrow(forest)){
  g <- append(g, str_split(forest$trees[i], "")[[1]])
}

trees <- matrix(g, ncol=1085, byrow=T)
curr <- c(1,1) #start at top left corner 
# traversal 
g <- 0
while(curr[1] <= nrow(trees) & curr[2] <= ncol(trees)){
  
  if(trees[curr[1],curr[2]] == "#"){
    g <- g + 1 
  } else {
    g <- g 
  }
  curr[1] <- curr[1] + 1
  curr[2] <- curr[2] + 3 
  
  
} 

g #answer 198 


#part 2: product of multiple slopes 

forest <- tibble(readLines("forest.txt"))%>% rename(trees = `readLines("forest.txt")`)
forest$trees <- str_dup(forest$trees,times=100) #change times parameter to account for increased slopes - larger than it needs to be 
g <- c()
for(i in 1:nrow(forest)){
  g <- append(g, str_split(forest$trees[i], "")[[1]])
}
trees <- matrix(g, ncol=3100, byrow=T)

parttwo <- function(down, right){
  curr <- c(1,1)
  g <- 0
  while(curr[1] <= nrow(trees) & curr[2] <= ncol(trees)){
    
    if(trees[curr[1],curr[2]] == "#"){
      g <- g + 1 
    } else {
      g <- g 
    }
    curr[1] <- curr[1] + down
    curr[2] <- curr[2] + right
    
    
  } 
  
  return(g) 
  
  
}

down <- c(1,1,1,1,2)
right <- c(1,3,5,7,1)
prod(map2_dbl(down,right, ~parttwo(.x,.y))) #answer ---   5140884672