## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
# if (<condition>){
# what to do if true
#} else if (<other condition>){
# what to do if true
#} else {
# what to do if none of them are true
#}
#

## -----------------------------------------------------------------------------
# for (i in 1:10) {
# print(i)
# }



## -----------------------------------------------------------------------------

# count <- 0
# while(count < 10) {
#         print(count)
#         count <- count + 1
# }

##DO NOT RUN##

# count <- 0
# while(count < 10) {
#         print(count)
#         count <- count - 1
# }


## -----------------------------------------------------------------------------

#count <- 1
# repeat {
#         count <- count + 1
# if (count >10)
#break
# }


## -----------------------------------------------------------------------------

# for (i in 1:10) {
# if (i==5)
# next
# print(i)
# }


## -----------------------------------------------------------------------------

 for (i in 1:10) {
   a <- i*2
  #browser()
   print(i)
 }


## -----------------------------------------------------------------------------
dt <- data.frame(mes1=c(2,3,4), mes2=c(3,2,1), mes3=c(2,4,6))
rowmean <- apply(dt, 1, mean)
rowmean
colmean <- apply(dt, 2, mean)
colmean

## -----------------------------------------------------------------------------
lt <- list(c(1,2,4), c(4,52,5,3,2), c(2,3,5,2,2))
lt2 <- lapply(lt, mean)

for (i in 1:length(lt)){
  print(length(lt[[i]]))
}

## -----------------------------------------------------------------------------
data(iris)
tapply(iris$Sepal.Width, iris$Species, median)

## -----------------------------------------------------------------------------

i39 <- sapply(3:9, seq) # list of vectors
sapply(i39, fivenum)
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))


