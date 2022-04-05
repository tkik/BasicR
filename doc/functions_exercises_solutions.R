## -----------------------------------------------------------------------------
my_first_function <- function(num1, num2){
  if (num2==0){
    stop("Not working")
}
  res <- num1/num2
  return(res)
}


## -----------------------------------------------------------------------------
esperente <- function(c){
  gsub("[aoiu]", "e", c)
}


## -----------------------------------------------------------------------------
esperente <- function(c){
  if (!is.character(c)){
    message("Not a character")
  }
  if (length(c)!=1){
    message("Too many elements!")
  }
  if (!grepl("[aoiu]", c)){
    message("The result will be the same!")
  }
  
  gsub("[aoiu]", "e", c)
}


## -----------------------------------------------------------------------------
variance <- function(num){
  
  sum((num-mean(num))^2)/(length(num)-1)
  
}


## -----------------------------------------------------------------------------
variance <- function(num){
    if (!is.numeric(num))
    stop("The input can only be numeric.")
  if (length(num)<2){
    message("Calculating the variance only makes sense with more than two numbers!")
  }
  sum((num-mean(num))^2)/(length(num)-1)
  
}


## -----------------------------------------------------------------------------
variance <- function(num){
    if (!is.numeric(num))
      num <- as.numeric(num)
    warning("The input can only be numeric, tried to coerce as one.")
  if (length(num)<2){
    message("Calculating the variance only makes sense with more than two numbers!")
  }
  sum((num-mean(num))^2)/(length(num)-1)
  
}


