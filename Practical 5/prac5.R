library(dplyr)
library(readr)

my_data <- read_csv("C:/Users/mvluc/OneDrive/Desktop/ANSHRAH/DATA ANALYSIS WITH SASS SPSS R/datasets/student_spending.csv")
my_data <- my_data[, -1]
head(my_data)

students_sorted_income <- my_data |>
  arrange(monthly_income)
head(students_sorted_income, 5)

students_sorted_food_desc <- my_data |>
  arrange(desc(food))

head(students_sorted_food_desc, 5)
students_multi_sort <- my_data |>
  arrange(year_in_school, desc(monthly_income))

head(students_multi_sort, 10)
high_tech_by_tuition <- my_data |>
  filter(technology > 150) |>
  arrange(tuition)

cat("Top 5 high-tech spenders with lowest tuition:\n")
print(high_tech_by_tuition |>
        select(technology, tuition, monthly_income) |>
        head(5))
