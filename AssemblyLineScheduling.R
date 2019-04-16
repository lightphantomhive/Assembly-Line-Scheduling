#Assembly Line Scheduling using dynamic programming

findOptimalPath <- function(num, line1, line2, line1To2, line2To1){
  
  #Path Data structures Initialization
  path1 = matrix(rep(0, 2*(num-1)), nrow = 2)
  path2 = matrix(rep(0, 2*(num-1)), nrow = 2)
  rownames(path1) <- c("AssemblyLine", "Station")
  rownames(path2) <- c("AssemblyLine", "Station")
  path1[1,1] = 1
  path2[1,1] = 2
  path1[2, ] = c(1:(num-1))
  path2[2, ] = c(1:(num-1))
  flag1 = 1
  flag2 = 2
  
  DynamicDS = matrix(rep(1001, 2*(num+1)), nrow = 2)
  
  DynamicDS[1][1] = line1[1]
  DynamicDS[2][1] = line2[1]
  
  for(i in 2:(num-1)){
    
    DynamicDS[1,i] = min(DynamicDS[1,i-1] + line1[i], DynamicDS[1,i-1] + line1To2[i-1])
    DynamicDS[2,i] = min(DynamicDS[2,i-1] + line2[i], DynamicDS[2,i-1] + line2To1[i-1])
    
    #Store the paths based on minimum time consumption
    if(DynamicDS[1,i-1] + line1[i] < DynamicDS[1,i-1] + line1To2[i-1]){
      
      path1[1,i] = flag1
    }
    else{
      
      if(flag1 == 1) flag1 = 2 else flag1 = 1
      path1[1,i] = flag1
    }
    if(DynamicDS[2,i-1] + line2[i] < DynamicDS[2,i-1] + line2To1[i-1]){
      
      path2[1,i] = flag2
    }
    else{
      
      if(flag2 == 1) flag2 = 2 else flag2 = 1
      path1[1,i] = flag2
    }
  }
  
  if(flag1 == 1) exit1 = line1[num] else exit1 = line2[num]
  if(flag1 == 1) exit2 = line1[num] else exit2 = line2[num]
  DynamicDS[1,num] = DynamicDS[1,num-1] + exit1
  DynamicDS[2,num] = DynamicDS[2,num-1] + exit2
  
  #print(DynamicDS)
  
  if(DynamicDS[1,num] < DynamicDS[2,num]){
    
    print(path1)
  }
  else{
    
    print(path2)
  }
  print(min(DynamicDS[1,num], DynamicDS[2,num]))
  
}

dataSet <- function(num){
  
  
  range <- seq(1:1000)
  line1 <- sample(range, num + 1, replace = TRUE)
  line2 <- sample(range, num + 1, replace = TRUE)
  print("Assembly Line 1", quote = FALSE)
  print(line1)
  print("Assembly Line 2", quote = FALSE)
  print(line2)
  line1To2 <- sample(range, num - 1, replace = TRUE)
  line2To1 <- sample(range, num - 1, replace = TRUE)
  
  findOptimalPath(num+1, line1, line2, line1To2, line2To1)
}

num = readline(prompt = "Enter the number of stations in one assembly line : ")
dataSet(as.integer(num))
