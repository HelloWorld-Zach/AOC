setwd("C:/Users/15517/Downloads")
library(tidyverse)
input <- readLines("lights.txt")
input <- str_remove_all(input, "through")
instructions <-  map_chr(seq_along(input), ~str_flatten(str_extract_all(input, "[:alpha:]")[[.]]))
x1 <- map_dbl(seq_along(input), ~as.double(str_extract_all(input, "\\d+")[[.]][1])) + 1
y1 <- map_dbl(seq_along(input), ~as.double(str_extract_all(input, "\\d+")[[.]][2])) + 1
x2 <- map_dbl(seq_along(input), ~as.double(str_extract_all(input, "\\d+")[[.]][3])) + 1
y2 <- map_dbl(seq_along(input), ~as.double(str_extract_all(input, "\\d+")[[.]][4])) + 1
lights <- array(data=rep(0, 1000^2), dim=c(1000,1000))
for(i in seq_along(input)){
  
  if(instructions[i] == "toggle"){
    lights[(x1[i]:x2[i]), (y1[i]:y2[i])] <- -lights[(x1[i]:x2[i]), (y1[i]:y2[i])] + 1
  }
  else if(instructions[i] == "turnoff"){
    lights[(x1[i]:x2[i]), (y1[i]:y2[i])] <- 0
  }
  else if(instructions[i] == "turnon"){
    lights[(x1[i]:x2[i]), (y1[i]:y2[i])] <- 1
  }
  
  
}
length(which(lights==1)) #ANSWER ---------------------------------> 543,903

#PART 2:

lightsp2 <- array(data=rep(0, 1000^2), dim=c(1000,1000))

for(i in seq_along(input)){
  
  if(instructions[i] == "toggle"){
    lightsp2[(x1[i]:x2[i]), (y1[i]:y2[i])] <- lightsp2[(x1[i]:x2[i]), (y1[i]:y2[i])] + 2
  }
  else if(instructions[i] == "turnoff"){
    lightsp2[(x1[i]:x2[i]), (y1[i]:y2[i])] <- apply(lightsp2[(x1[i]:x2[i]), (y1[i]:y2[i])], c(1,2), function(x) max(0, x-1))
  }
  else if(instructions[i] == "turnon"){
    lightsp2[(x1[i]:x2[i]), (y1[i]:y2[i])] <- lightsp2[(x1[i]:x2[i]), (y1[i]:y2[i])] + 1
  }
  
  
}

sum(lightsp2) #ANSWER ---------------------------------> 
