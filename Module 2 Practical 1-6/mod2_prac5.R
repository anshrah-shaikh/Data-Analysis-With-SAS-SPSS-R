

df5 <- read.csv("C:/Users/mvluc/OneDrive/Desktop/ANSHRAH/DATA ANALYSIS WITH SASS SPSS R/datasets/student_spending.csv")

df5_sub <- df5 %>% filter(gender %in% c("Male", "Female"))
t.test(monthly_income ~ gender, data = df5_sub)
