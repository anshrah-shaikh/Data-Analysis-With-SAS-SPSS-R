


df6 <- read.csv("C:/Users/mvluc/OneDrive/Desktop/ANSHRAH/DATA ANALYSIS WITH SASS SPSS R/datasets/SalesForCourse_quizz_table.csv")
t.test(df6$Unit.Cost, df6$Unit.Price, paired = TRUE)
