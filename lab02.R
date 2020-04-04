name = 'Muhamamd Noufal'
liuid = 'muhno651'
#Task 01
sheldon_game = function(player1,player2)
{
  alloptions = c("rock","lizard","spock","scissors","paper")
  if(!(player1 %in% alloptions && player2 %in% alloptions))
    stop('Error')
  
  index1 = which(alloptions %in% player1)
  index2 = which(alloptions %in% player2)
  temp  = (index1 + c (1 ,3)) %% 5 
  temp[temp  == 0] = 5
  if( length(which(temp %in% index2)) > 0) {
    return ("Player 1 wins!")
  }
  else if(index1 == index2){
    
    return("Draw!")
  }
  else {
    return ("Player 2 wins!")
  }
  
  #Simple if condition
  # if (player1 == 'rock' && (player2 == 'scissors' || player2 == 'lizard'))
  # {return("Player 1 wins!")}
  # else if (player1 == 'paper' && (player2 =='rock' || player2 == 'spock'))
  # {return("Player 1 wins!")}
  # else if (player1 == 'scissors' && (player2 == 'paper' || player2 == 'lizard'))
  # { return("Player 1 wins!")}
  # else if (player1 == 'lizard' && (player2 == 'paper' || player2 == 'spock'))
  # { return("Player 1 wins!")}
  # else if (player1 == 'spock' && (player2 == 'rock' || player2 == 'scissors'))
  # {return("Player 1 wins!")}
  # else if (player1 == player2)
  # {   return("Draw!")}
  # else
  # {return("Player 2 wins!")}
  
}

#Task 04
find_cumsum  = function(x,find_sum)
{
  i = 1
  sum = 0
  while(i <= length(x))
  {
    sum = sum +x[i]
    i = i+1
    if(sum > find_sum)
     break
  }
  return(sum)
}
moment = function(i)
{
  defined <- ls()
  passed <- names(as.list(match.call())[-1])
  #any(!defined %in% passed)
  if (missing(i)) {
    stop("args missing")
  }
  if(class(i) == class('1'))
  {
    stop('wrong args')
  }
  function(x) {
    try({
      if(class(x) != class(''))
      {
        vector_mean = mean(x)
        sum = sum((x-vector_mean)^i)
        central_moment  = (1/length(x))*sum
        return(central_moment)
      }
    })
    stop("Error")
  }
}#Task 09
cov = function(X)
{
  try({
    if(length(X) >3)
      {
      return(unlist(lapply(X,function(i){sd(i)/mean(i)}),use.names = TRUE))
      }
    })
  stop("Error")
  
}    

#Task 3
for_mult_table = function(from,to)
{
  if((class(from) == class('c') || class(to) == class('c')) || from <= 0 ||  to < from) stop('Wrong Arguments')
  mat = c()
  v_index=1
  for(i in c(from:to))
  {
    tempVector = c(from:to)
    for(j in tempVector)
    {
      mat[v_index] = j*i
      v_index = v_index+1
    }
    #mat = c(mat,tempVector*i)
  }
  result_mat = matrix(mat,nrow = (to-from+1), ncol = (to-from+1))
  colnames(result_mat) = c(from:to)
  rownames(result_mat) = c(from:to)
  return (result_mat)
}
#Task 07

repeat_my_moving_median = function(x,n,...)
{
  try(if(class(x) == 'character' || class(n) == 'character') stop('Wrong Arguments') )
  resultVector = c(1)
  #for(i in c(1:(length(x)-n)))
  i=1
  repeat
  {
    nextindex = i+n;
    resultVector[i] = median(x[i:nextindex],...)
    i = i+1
    if(i > (length(x)-n))
      break
  }
  return (resultVector)
}


#Task 02
my_moving_median = function(x,n,...)
{
  if(class(x) == 'character' || class(n) == 'character') stop('Wrong Arguments') 
  resultVector = c(1)
  for(i in c(1:(length(x)-n)))
  {
    nextindex = i+n;
    resultVector[i] = median(x[i:nextindex],...)
  }
  return (resultVector)
}
#Task 08
in_environment = function(env)
{
  return(ls(env))
}
#Task 06
repeat_find_cumsum  = function(x,find_sum)
{
  i = 1
  sum = 0
  repeat
  {
   
    sum = sum +x[i]
    i = i+1
    if(sum > find_sum || i > length(x))
      break
    
  }
  return(sum)
}
#Task 05
while_mult_table = function(from,to)
{
  try(if((class(from) == 'character' || class(to) == 'character') || from <= 0 ||  to < from) stop('Wrong Arguments'))
  mat = c()
  i = 1
  v_index = 1
  original_vector = c(from:to)
  while(i <= length(original_vector))
  {
    
    tempVector = c(from:to)
    j = 1
    while(j<= length(tempVector))
    {
      mat[v_index] = tempVector[j]*original_vector[i]
      v_index = v_index+1
      j=j+1
    }
    i=i+1
  }
  result_mat = matrix(mat,nrow = (to-from+1), ncol = (to-from+1))
  colnames(result_mat) = c(from:to)
  rownames(result_mat) = c(from:to)
  return (result_mat)
}

