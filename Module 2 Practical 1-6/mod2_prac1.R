library(psych)

df1 <- read.csv("C:/Users/mvluc/OneDrive/Desktop/ANSHRAH/DATA ANALYSIS WITH SASS SPSS R/datasets/MLR2.csv")
summary(df1$PerFemEmploy)
psych::describe(df1$FertilityRate)
