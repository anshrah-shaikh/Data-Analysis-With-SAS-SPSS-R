install.packages("dplyr")

library(dplyr)
library(readr) 

my_data <- read_csv("C:/Users/mvluc/OneDrive/Desktop/ANSHRAH/DATA ANALYSIS WITH SASS SPSS R/datasets/student_spending.csv")
my_data <- my_data[ , -1]
head(my_data)

high_income <- subset(my_data, monthly_income > 1000)
cat("Number of high-income students:", nrow(high_income), "\n")
summary(high_income$monthly_income)

aid_food_subset <- subset(my_data, financial_aid > 500 & food > 300)
cat("Students with high aid AND high food expenses:", nrow(aid_food_subset), "\n")
head(aid_food_subset)

senior_or_entertainment <- subset(my_data, year_in_school == "Senior" | entertainment > 200)
cat("Number of Seniors OR high-entertainment spenders:", nrow(senior_or_entertainment), "\n")
head(senior_or_entertainment)

low_tuition <- my_data |>
  filter(tuition < 4000)
cat("Students with tuition < 4000:", nrow(low_tuition), "\n")
summary(low_tuition$tuition)

tech_nb <- my_data |>
  filter(gender == "Non-binary", technology > 100)
cat("Non-binary students with high tech spending:", nrow(tech_nb), "\n")
head(tech_nb)

card_or_cash <- my_data |>
  filter(preferred_payment_method %in% c("Credit/Debit Card", "Cash"))
cat("Students using Card or Cash:", nrow(card_or_cash), "\n")
table(card_or_cash$preferred_payment_method)
