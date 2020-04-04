# Problem 1
# part a
# 1)properly give meaningfull name to each variables.
# 2)choose any codding style and stick with it in your whole project. Forexample camelCase
# 3) Only put comments where you want to explain the tricky part of your code. It helps you to remember the code 
# and easy for other developer to work on it.
# 4) give function meaningfull name forexample action_type i.e create_fridge(), add_items

#part b
# 1) Name 
# 2) Description
# 3) Args
# 4) Return
# 5) example 
# 6) Usage



#Problem 3

#part a
cal_factorial = function(number)
{
  if(!is.numeric(number))
    stop("Error: Input Should be number")
  if(number < 0)
    stop("Error: argument should be positive integer")
  if(number == 0)
    return(1)
  fact = 1
  for(i in c(number:1))
  {
    fact = fact * i
  }
  return(fact)
}
# Part b
# the complexcity of this code is O(n)

#Part c
cal_Stirling_fact = function (number)
{  
  if(!is.numeric(number))
  stop("Error: Input Should be number")
  if(number < 0)
    stop("Error: argument should be positive integer")
  if(number == 0)
    return(1)
  return (sqrt(2*pi*number)*(number/exp(1))^number)
}
cal_Stirling_rightLimt = function (number)
{
  if(!is.numeric(number))
    stop("Error: Input Should be number")
  if(number < 0)
    stop("Error: argument should be positive integer")
  if(number == 0)
    return(1)
  return (exp(1)*sqrt(number)*(number/exp(1))^number)
}
library(testthat)
expect_true(cal_factorial(10) > cal_Stirling_fact(10) && cal_factorial(10) < cal_Stirling_rightLimt(10))




# Problem 02

modern_fridge = setRefClass("modern_fridge",
                            fields = list(
                              number_users = "numeric",
                              user_info = "list",
                              food_items = "list"
                            ))

modern_fridge$methods(
  create_fridge= function(number_users = 2)
  {
    .self$food_items = list()
    .self$user_info = list()
    if(!is.numeric(number_users))
      stop("Error: number of users should be numeric")
    if(!(number_users > 0))
      stop("Error: number of users should be greater than 0")
    
    .self$number_users = number_users
    
    
    .self$build_diet_list()
    
    for(user in c(1:number_users))
    {
      new_user = list(list(user_id=user, food_list= .self$food_items )) 
      .self$user_info = append(.self$user_info,new_user)
    }
  },
  build_diet_list = function()
  {
    for(item in c(1:20))
    {
      new_fooditem = list(list(food_id=item,permissible_number = round(runif(1,5,15)), conflict_ids= round(runif(2,1,20))))
      .self$food_items = append(.self$food_items , new_fooditem)  
    }
  },
  update_food_list = function (user_id,food_id,update_max=0,update_conflicting_food=c())
  {
    if(user_id > .self$number_users)
      stop("Error: this user is not present")
    if(food_id > 20)
      stop("Error: food id is not present")
    if(update_max < 0 )
      stop("Error: maximum daily amount should be greater than 1")
    if(!is.numeric(update_conflicting_food))
      stop("Error: it should be ids")
    if(update_max > 0)
    {
      .self$user_info[[user_id]]$food_list[[food_id]]$permissible_number = update_max
    }
    if(length(update_conflicting_food) > 0)
    {
      current_conflict_Ids =  .self$user_info[[user_id]]$food_list[[food_id]]$conflict_ids
      removing_ids = abs(update_conflicting_food[update_conflicting_food < 0])
      removinf_ids_index = which(current_conflict_Ids %in% removing_ids)
      new_conflicts = update_conflicting_food[update_conflicting_food > 0]
      if(length(removinf_ids_index) == 0)
        .self$user_info[[user_id]]$food_list[[food_id]]$conflict_ids = c(current_conflict_Ids, new_conflicts)
      else
        
        .self$user_info[[user_id]]$food_list[[food_id]]$conflict_ids = c(current_conflict_Ids[-removinf_ids_index], new_conflicts)
    }
  },
  request_food = function(user_id,food_ids)
  {
    if(user_id > .self$number_users)
      stop("Error: this user is not present")
    if(food_id > 20)
      stop("Error: food id is not present")
    if(!is.numeric(food_ids))
      stop("Error: it should be ids")
    for(id in food_ids)
    {
      food_id = .self$user_info[[user_id]]$food_list[[id]]
      current_conflict_Ids =  food_id$conflict_ids
      check_conflict = which(current_conflict_Ids %in% food_ids)
      if(length(check_conflict) > 0)
        stop("Error: conflict in food id. Request is denied")
      
    }
    error = 0
    for(id in food_ids)
    {
      food_id = .self$user_info[[user_id]]$food_list[[id]]
      if(food_id$permissible_number ==0)
        error = error +1 
    }
    if(error > 0)
    {
      stop("Error: Already consume the maximum limit. Request is denied")
    }
    else
    {
      for(id in food_ids)
      {
        food_id = .self$user_info[[user_id]]$food_list[[id]]
        .self$user_info[[user_id]]$food_list[[id]]$permissible_number = food_id$permissible_number - 1
      }
    }
   
  },
  print = function()
  {
    for(user in c(1:.self$number_users))
    {
      current_user = .self$user_info[[user]]
      cat("User Id ")
      cat(current_user$user_id)
      cat("\n")
      cat("*********************************")
      cat("\n")
      for(food in c(1: length(current_user$food_list)))
      {
        currrent_food_item = current_user$food_list[[food]]
        print.data.frame(as.data.frame.integer(currrent_food_item))
        # cat("Food Id : ")
        # print.default(currrent_food_item$food_id)
        # cat("Premissible Number : ")
        # print.default(currrent_food_item$permissible_number)
        # cat("conflict ids : ")
        # print.default(currrent_food_item$conflict_ids)
         cat("\n")
         cat("*********************************")
         cat("\n")
      }
      cat("\n")
      cat("-----------------------------------")
      cat("\n")
    }
  }
  
)

# Crearte fridge
a =modern_fridge$new()
a$create_fridge(1)
# print
a$print()


# update_food_list function
food_id<-1
user_id<-1
update_max<-5
update_conflicting_food<-c(-2)
a$user_info[[user_id]]$food_list[[food_id]]
a$update_food_list(user_id,food_id,update_max,update_conflicting_food)
as.data.frame.integer(a$user_info[[user_id]]$food_list[[1]])

# request food 
user_id<-1
vfood_ids<-c(1,10)
a$request_food(user_id,vfood_ids)


