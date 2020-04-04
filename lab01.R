name = 'Muhamamd Noufal'
liuid = 'muhno651'
#Task 1.2.2
calculate_elements <- function(A)
{
  num_elements <- nrow(A)*ncol(A)     # cell count row*col
  #elements <- length(mat)            # directly calculate it
  return (num_elements)
}

#Task 1.4.2
sort_head <- function (df,var.name,n)
{
    #- for decreasing 
  return(head(df[order(-df[[var.name]]),],n))
}

#Task 1.2.1
my_magic_matrix <-function()
{
  magic_matrix = matrix(c(4, 3, 8, 9, 5, 1, 2, 7, 6),nrow=3,ncol=3)
  return(magic_matrix)
}

#Task 1.2.4
add_elements_to_matrix <- function(A, x,i,j)
{
  A[i,j] = A[i,j]+x
  return(A)
}

#Task 1.2.3
row_to_zero <- function(A,i)
{
  A[i,]=0
  return(A)
}

#Task 1.4.1
my_data.frame <- function()
{
  id = c(1:3)
  name = c('John','Lisa','Azra')
  income =  c(7.30,0.00,15.21)
  rich =  c(FALSE,FALSE,TRUE)
  frame = data.frame(id,name,income,rich)
  return (frame)
}

#Task 1.3.2
change_info <-function(x,text)
{
  x$info = text
  return(x)
}

#Task 1.4.3
add_median_variable <- function(df,j)
{
  df$compared_to_median='Smaller'
  med = median(df[[j]])
  index = df[[j]] > med     #index vector having value greater than median
  df[['compared_to_median']][index] = "Greater"
  index = df[[j]] == med    #index vector having value equal than median
  df[['compared_to_median']][index] = "Median"
  return(df)
}

#Task 1.3.4
sum_numeric_parts <- function(x)
{
  
  #Use Loops for this 
  
  #total_sum = 0;
  #for(item in x)
  #{
  #  if(typeof(item) != typeof(""))
  #  {
  #    total_sum = total_sum +sum(item)
  #  }
  #}
  
  #User lapply
  l = lapply(x, function(item){ if(typeof(item) != typeof("")){ sum(as.numeric(item)) }})
  v = unlist(l,use.names = FALSE)
  total_sum = sum(v,na.rm = TRUE)
  return(total_sum)
}

#Task 1.4.4
analyze_columns <-  function (df,j)
{
  result = list()
  for(i in j)
  {
    #converting df column to vector
    avector <- df[[i]]
    #calculating mean median and S.D
    mean = mean(avector)
    median = median(avector)
    sd = sd(avector)
    #Create the matrix
    mat =c(mean,median,sd)
    #Giving the name to the columns 
    names(mat) = c('mean', 'median', 'sd')
    #Appending the matrix into the list
    result = c(result,list(mat))
  }
  #giving name to all the element of the list
  names(result) = colnames(df[j])
  #calculating the correlation between the given columns
  result$correlation_matrix = cor(df[j])
  return (result)
}

#Task 1.1.4
approx_e <- function(N)
{
  vec = 0:N
  approx_value <- sum(1/factorial(vec))
  return (approx_value)
}

#Task 1.1.1
my_num_vector <- function()
{
  list_data = c(log10(11),cos(pi/5),exp(pi/3),(1173 %% 7)/19);
   return (list_data)
}

#Task 1.1.2
filter_my_vector <- function(x, leq)
{
  x[x>=leq] = NA;x
  return (x)
}

#Task 1.1.3
dot_prod <-function(a,b)
{
  mult_vector = a*b
  dot_product = sum(mult_vector)
  return (dot_product)
} 

#Task 1.3.1
my_magic_list <- function()
{
  list = list(info = "my own list",my_num_vector(),my_magic_matrix())
  return(list)
}

#Task 1.3.3
add_note <-function(x,note)
{
  x$note = note
  return(x)
}
