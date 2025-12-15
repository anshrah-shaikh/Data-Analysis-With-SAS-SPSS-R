library(dplyr)

df2 <- read.csv("C:/Users/mvluc/OneDrive/Desktop/ANSHRAH/DATA ANALYSIS WITH SASS SPSS R/datasets/sales_data.csv")
table(df2$Region)
df2 %>% count(Product_Category)
