## -----------------------------------------------------------------------------
data("Orange")

Hmisc::describe(Orange)

Orange$cc <- NA
#Orange$tree <- "something else"

for (i in 1:nrow(Orange)){
  print(i)
if (Orange$circumference[i] <= 65.5){
  Orange$cc[i] <- "small"
} else if (Orange$circumference[i]>65.5 & Orange$circumference<=161.5){
  Orange$cc[i] <- "medium"
} else {
  Orange$cc[i] <- "large"
}
}

Orange$cc <- ifelse(Orange$circumference<=65.5, "small", 
                    ifelse(Orange$circumference<=161.5, "medium", "large"))



## -----------------------------------------------------------------------------

Orange[Orange$Tree==1,"age"]

for (i in 1:5){
  cat("Tree: ", i, " Ages: ", paste0(Orange[Orange$Tree==i,"age"], collapse = ", "), "\n")
}

for (i in 1:5){
  cat("Tree: ", i, " was small at age: ", paste0(Orange[Orange$Tree==i & Orange$cc=="small","age"], collapse = ", "), "\n")
}

# 0     1     2     3     4
# 1     0     1     2     3
# 2     1     0     1     2
# 3     2     1     0     1
# 4     3     2     1     0
data("esoph")
for (agegrp in levels(esoph$agegp)){
  print(sum(esoph$ncontrols[esoph$agegp==agegrp]))
}

for (agegrp in levels(esoph$agegp)){
  for (alcgrp in levels(esoph$alcgp)){
   #print(agegrp)
    #print(alcgrp)
  cat("Age group: ", agegrp, ", alcohol consumption group: ", alcgrp, " number of controls: ", sum(esoph$ncontrols[esoph$alcgp==alcgrp & esoph$agegp==agegrp]), "\n")
  }
}
for (agegrp in levels(esoph$agegp)){
  for (alcgrp in levels(esoph$alcgp)){
    if (agegrp=="25-34" & alcgrp=="120+"){
      #next
    } else {
  cat("Age group: ", agegrp, ", alcohol consumption group: ", alcgrp, " number of controls: ", sum(esoph$ncontrols[esoph$alcgp==alcgrp & esoph$agegp==agegrp]), "\n")}
  }
}


## -----------------------------------------------------------------------------
dataset1 <- cbind(observationA = 16:8, observationB = c(20:19, 6:12))

## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
#two ways

multiply <- function(x, number){
  x <- x*number
  x
}
res <- apply(dataset1,2, multiply, number=3)

res <- apply(dataset1,2, function(x) x*3)


## -----------------------------------------------------------------------------
lll <- list(list(1,3,5,6,1), list(5,2,1), list(6,2,5,7,3,1,9))

## -----------------------------------------------------------------------------
lapply(lll, length)

