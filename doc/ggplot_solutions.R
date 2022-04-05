## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggplot2)
library(plotly)
data(iris)

g <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species))+
  geom_point()+
  geom_smooth(method="lm")

ggplotly(g)


ggplot(iris, aes(y=Sepal.Length, x=Species, fill=Species))+
  geom_boxplot()+
  scale_fill_manual(values = c(alpha("mediumseagreen", alpha = 0.4), "mediumorchid4", "firebrick1"))+
  geom_dotplot(binaxis = "y", fill="black", binwidth = 0.05)+
  labs(y="Sepal length")+
  coord_flip()+
  labs(x="Species")+
theme_bw()


