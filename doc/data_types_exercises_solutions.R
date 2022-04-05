## -----------------------------------------------------------------------------

#1.
a <- c(1,1,3,4,5)
b <- c(2, 2, 5, 4, 1)
#2.
min(a)
min(b)
#3.
min(c(a,b))
min(a,b)
#4.
a+b
sum(a+b)
#5.
sqrt(a+b)
#6. 
sort(a, decreasing = T)
sort(b, decreasing = T)
#7
 which(duplicated(a))
a[a==a[duplicated(a)]]
#8
a <- sample(1:100, 100, replace = T)
sum(a==3)




## -----------------------------------------------------------------------------
data("iris")
plength <- iris[,"Petal.Length"]
plength <- iris[,3]
iris$Petal.Length

new_df <- iris[,c("Sepal.Width", "Sepal.Length", "Species")]
new_df <- iris[,c(2,1,5)]
max(iris[iris$Species=="setosa", "Petal.Width"])
iris[2, "Sepal.Width"]
sum(iris[iris$Species=="setosa", "Petal.Width"]==0.2)

## -----------------------------------------------------------------------------
dataset <- data.frame(Patient.ID=c("normal_01", "normal_02", "normal_03", "tumor_01", "tumor_02", "tumor_02"), 
                      Sentrix.position=c("A01B01", "A01B02", "A016A01", "B02A02", "C01D02", "C02C01"), Treatment=c("Treated", "Treated", "Not treated", "Treated", "Treated", "Not treated"), value=c(3.25, 3.67, 4.26, 6.24, 5.78, 7.32), row.names = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6"))

## -----------------------------------------------------------------------------
#Examples:
grep("normal", dataset$Patient.ID)
grep("norm", dataset$Patient.ID)
grep("nom", dataset$Patient.ID)
grepl("normal", dataset$Patient.ID)
grepl("[[:alpha:]]", dataset$Patient.ID)
grepl("[[:alpha:]]{5}", dataset$Patient.ID)
grepl("[[:alpha:]]{6}", dataset$Patient.ID)
grepl("[[:alpha:]]_[[:digit:]]", dataset$Patient.ID)
grepl("[[:alpha:]]{6}_[[:digit:]]{2}", dataset$Patient.ID)
regexec("[[:alpha:]]_[[:digit:]]", dataset$Patient.ID)
gregexpr("[[:alpha:]]_[[:digit:]]", dataset$Patient.ID)
gsub("_", ".", dataset$Patient.ID)
gsub(".", "_", dataset$Patient.ID)
gsub("\\.", "_", dataset$Patient.ID)
gsub(".", "_", dataset$Patient.ID, fixed = T)
gsub("([[:alpha:]]{5,6})_([[:digit:]]{2})", "\\2", dataset$Patient.ID)
gsub("([[:alpha:]]{5,6})_([[:digit:]]{2})", "\\1", dataset$Patient.ID)
gsub("([A-Za-z]{5,6})_([[:digit:]]{2})", "\\1", dataset$Patient.ID)
dataset$Sample_type <- gsub("([A-Za-z]{5,6})_([[:digit:]]{2})", "\\1", dataset$Patient.ID)

rownames(dataset) <- gsub("Sample", "Sample_", rownames(dataset))




## -----------------------------------------------------------------------------
dataset$Sample_type <- ifelse(grepl("normal", dataset$Patient.ID), "normal", "tumor")
dataset$Sample_type <- factor(dataset$Sample_type, levels = c("normal", "tumor"))
dataset$Sample_type <- as.factor(dataset$Sample_type)
dataset$Sample_type <- factor(dataset$Sample_type, levels=c("normal", "tumor", "unknown"))
dataset$Sample_type <- factor(dataset$Sample_type, levels=c("tumor", "normal", "unknown"))

## -----------------------------------------------------------------------------
my_list <- list("a", c(1,2), TRUE, factor(c("apple", "oranges")), list(1,2))

my_list <- list(vec=c(1,2), ll=list(1,2,3), mat=matrix(1:9, nrow=3), num=3)
my_list[[2]] <- NULL

my_list[[4]] <- data.frame(one=c(1,2), two=c("one", "two"), stringsAsFactors = F)

my_list[[length(my_list)+1]] <- "empty"


