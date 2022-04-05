## -----------------------------------------------------------------------------
f <- function(){
  cat("Hello world! \n")
}
f()

## -----------------------------------------------------------------------------
f <- function(num1, num2){
  res <- num1+num2
  res
}
f(1,2)

## -----------------------------------------------------------------------------
f <- function(num1, num2){
  res <- num1+num2
  return(res)
  cat("Hello world! \n")
}
f(1,2)

## -----------------------------------------------------------------------------

f <- function(num1, num2){
  print(num1)
  print(num2)
}
f(1,2)
f(num2=2, num1=1)
f(2, num1=1)
try(f(2))

## -----------------------------------------------------------------------------

f <- function(num1, num2=3){
  print(num1)
  #browser()
  print(num2)
}
f(1,2)
f(num2=2, num1=1)
f(2)


## -----------------------------------------------------------------------------
g <- function(x) { 
         x*y
}
try(g(2))
y <- 10
g(2)

#but!
x <- 1:10
try(mean())

g <- function(x) { 
  ab <- 12
         x*y
}
try(print(ab))


## -----------------------------------------------------------------------------

ls(environment(g))

#ls(environment(mean))

search()

## ---- echo = FALSE, out.width = "70%", eval=TRUE------------------------------
knitr::include_graphics("https://github.com/allisonhorst/stats-illustrations/blob/master/other-stats-artwork/debugging.jpg?raw=true", dpi = 100)

