library(lattice)
library(faraway)
library(tibble)
library(stats)
library(tidyverse)
library(ggplot2)
data("sat")
df<-sat[complete.cases(sat),]

df1 <- df1[,-3]
df1 <- df1[order(-df1[,3]),]

colorIndices = (df1$total < median(df1$total)) + 1
satColors = c("#482677FF","#29AF7FFF")[colorIndices]

parallelplot(df1[,1:3], horizontal.axis=FALSE, col=satColors,lwd=1.5,
             main="Average state SAT: best scores")

