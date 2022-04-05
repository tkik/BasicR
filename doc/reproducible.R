## ---- echo = FALSE, out.width = "70%"-----------------------------------------
knitr::include_graphics("https://git-scm.com/book/en/v2/images/lifecycle.png", dpi = 100)

## -----------------------------------------------------------------------------
plot(cars)

## ----echo=FALSE---------------------------------------------------------------
plot(cars)

## ----eval=FALSE---------------------------------------------------------------
#  plot(cars)

## ---- results='asis'----------------------------------------------------------
library(dplyr)
data("mtcars")
knitr::kable(mtcars, format = "html")
print(xtable::xtable(mtcars), type = 'html')
 mtcars %>%
 	DT::datatable(filter = 'top', 
          options = list(scrollX = TRUE, keys = TRUE, pageLength = 5),  
          rownames = FALSE)

