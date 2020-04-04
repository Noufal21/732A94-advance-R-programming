library(parallel)
#' @title brute_force_knapsack
#' @description brute_force_knapsack function to solve the knapsack problem
#' @name brute_force_knapsack
#' @import utils
#' @param x , the dataset
#' @param W , the capacity of the knapsack
#' @param parallel, make comination using parallel or not
#' return list , returns a list that contain the maximum value and the list of elements chosen
#' @export
brute_force_knapsack = function(x, W, parallel = FALSE)
{
  if(class(x) == class('s'))
    stop('Error')
  length = nrow(x)
  value = 0
  elements = c()
  if(W <=0)
    stop("ERROR")
  if(parallel == TRUE)
  {
    numCores = parallel::detectCores()
    cl <- parallel::makeCluster(numCores/2)
    
    # getCombinations = function(i,var2) {
    #   combine=combn(x[[var2]], i)
    # }
    combine_w_list = parallel::parLapply(cl, 1:length, function(i,var2){combine=combn(x[[var2]], i)},var2 = 'w')
    combine_v_list = parallel::parLapply(cl, 1:length, function(i,var2){combine=combn(x[[var2]], i)},var2 = 'v')
    
    parallel::stopCluster(cl)
  }
  for (i in 1:length)
  {
    if(parallel == TRUE)
    {
      combine_w = as.data.frame(combine_w_list[[i]])
      combine_v = as.data.frame(combine_v_list[[i]])
    }
    else
    {
      combine_w = as.data.frame(combn(x[['w']], i))
      combine_v = as.data.frame(combn(x[['v']], i))
    }
    sumw = colSums(combine_w)
    sumv = colSums(combine_v)
    components =  which(sumw <=W)
    if(length(components) != 0){
      
      value_temp_max  =  max(sumv[components])
      if (value_temp_max > value) {
        value = value_temp_max
        index =  which(sumv == value)
        weight = combine_w[,index]
        elements = match(weight, x$w)
      }
    }
  }
  knapsack = list("value" = round(value), "elements" = elements)
  return(knapsack)
}





#' @title knapsack_dynamic
#' @description knapsack_dynamic function to solve the knapsack problem
#' @name knapsack_dynamic
#' @param x , the dataset
#' @param W , the capacity of the knapsack
#' return list , returns a list that contain the maximum value and the list of elements chosen
#' @export
knapsack_dynamic  = function(x,W)
{
  if(W <=0)
    stop("ERROR")
  if(class(x) == class('s'))
    stop('Error')
  knapsack = list()
  n = nrow(x)
  K = matrix(0L,nrow = (n+1),ncol = (W+1))
  
  Keep = matrix(0L,nrow = (n+1),ncol = (W+1))
  val = x[['v']] 
  wt = x[['w']]
  elements = c()
  for (i  in c(2:(n+1))) {
    for (w in c(2:(W+1))) {
      if (wt[i-1] <= (w-1)) 
      {
        
        K[i,w] = max(val[i-1] + K[i-1,(w-wt[i-1])+1],  K[i-1,w])
        if(val[i-1] + K[i-1,(w-wt[i-1])+1] > K[i-1,w])
          Keep[i,w] = 1
        else
          Keep[i,w] = 0
      }
      else
      {
        K[i,w] = K[i-1,w]
        Keep[i,w] = 0
      }
    }
  }
  index = 1;
  rowCount = n+1
  weight = W+1
  while (rowCount > 1) {
    if(Keep[rowCount,weight] == 1)
    {
      elements[index] = rowCount-1
      index = index + 1
      weight = weight - wt[rowCount-1];
    }
    rowCount = rowCount -1
  }
  knapsack$value = round(K[(n+1),(W+1)])
  knapsack$elements = elements
  return(knapsack)
}

#' @title greedy_knapsack
#' @description greedy_knapsack function to solve the knapsack problem
#' @name greedy_knapsack
#' @param x , the dataset
#' @param W , the capacity of the knapsack
#' return list , returns a list that contain the maximum value and the list of elements chosen
#' @export
greedy_knapsack = function(x,W)
{
  if(W <=0)
    stop("ERROR")
  if(class(x) == class('s'))
    stop('Error')
  x$ratio = x$v/x$w
  sorted = x[order(x$ratio,decreasing = TRUE),]
  max_value = 0
  weight = 0 
  fractions = c(0)*nrow(x)
  for (i in c(1:nrow(x)))
  {
    if (sorted$w[i] + weight <= W){
      fractions[i] = which(x[['ratio']] == sorted$ratio[i])
      max_value = max_value + sorted$v[i]
      weight = weight + sorted$w[i]
      #W =W - wt[i]
    }
    else{
      # remain = W - weight; 
      # max_value =max_value +  sorted$v[i] * ( remain / sorted$w[i]); 
      # fractions[i] = which(x[['w']] == sorted$w[i])
      break
    }
  }
  knapsack = list("value" = round(max_value), "elements" = fractions)
  return (knapsack)
}


