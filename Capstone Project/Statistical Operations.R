# ANSHRAH SHAIKH
# SYCS
# Stats operations performed on dataset:
# Data Analysis with SAS SPSS R/Capstone submission/Paranormal Beliefs, Cognitive Style, and Information Processing (Responses).csv

# ===============================
# LOAD LIBRARIES
# ===============================
library(dplyr)
library(psych)

# ===============================
# LOAD DATA
# ===============================
data <- read.csv("C:/Users/ANSI/Desktop/practice/SYCS/sem 4/Data Analysis with SAS SPSS R/Capstone submission/Paranormal Beliefs, Cognitive Style, and Information Processing (Responses).csv")

# ===============================
# CLEAN TEXT SPACES
# ===============================
data$I.believe.that.some.people.can.predict.the.future.using.supernatural.abilities. <- trimws(
  data$I.believe.that.some.people.can.predict.the.future.using.supernatural.abilities.
)

# ===============================
# CONVERT LIKERT RESPONSE TO NUMERIC
# ===============================
data$FuturePrediction <- dplyr::recode(
  data$I.believe.that.some.people.can.predict.the.future.using.supernatural.abilities.,
  "Strongly Disagree" = 1,
  "Disagree" = 2,
  "Neutral" = 3,
  "Agree" = 4,
  "Strongly Agree" = 5,
  .default = NA_real_
)

# ===============================
# REMOVING DUPLICATE COLUMN S
# ===============================
data <- data %>%
  select(-Some.individuals.possess.psychic.powers....2)

# ===============================
# CREATE COMPOSITE BELIEF SCORE
# ===============================
data$BeliefScore <- rowMeans(data[,c(
  "FuturePrediction",
  "Ghosts.or.spirits.of.the.dead.can.influence.the.living.",
  "Astrology.can.provide.accurate.insights.into.a.person.s.personality.or.future.",
  "Certain.objects.or.rituals.can.bring.good.or.bad.luck.",
  "Some.individuals.possess.psychic.powers."
)], na.rm = TRUE)

# =====================================================
# 1 DESCRIPTIVE STATISTICS
# =====================================================

summary(data$BeliefScore)

psych::describe(data$BeliefScore)

# =====================================================
# 2 FREQUENCY TABLES
# =====================================================

table(data$Gender)

table(data$Select.your.Age.Group)

table(data$Do.you.personally.believe.in.paranormal.phenomena.)

table(data$Have.you.ever.experienced.something.you.believe.was.paranormal.)

# =====================================================
# 3 CROSS TABULATIONS
# =====================================================

table(
  data$Gender,
  data$Do.you.personally.believe.in.paranormal.phenomena.
)

table(
  data$Select.your.Age.Group,
  data$Do.you.personally.believe.in.paranormal.phenomena.
)

# =====================================================
# 4 ONE SAMPLE T TEST
# Test if belief score differs from neutral (3)
# =====================================================

t.test(data$BeliefScore, mu = 3)

# =====================================================
# 5 INDEPENDENT TWO SAMPLE T TEST
# Compare belief scores by gender
# =====================================================

t.test(BeliefScore ~ Gender, data = data)

# =====================================================
# 6 ONE WAY ANOVA
# Compare belief across age groups
# =====================================================

anova_age <- aov(
  BeliefScore ~ Select.your.Age.Group,
  data = data
)

summary(anova_age)

# =====================================================
# 7 TWO WAY ANOVA
# Gender + Age effect on belief
# =====================================================

anova_two <- aov(
  BeliefScore ~ Gender * Select.your.Age.Group,
  data = data
)

summary(anova_two)

# =====================================================
# 8 CHI SQUARE TEST
# Relationship between gender and paranormal belief
# =====================================================

chisq.test(
  table(
    data$Gender,
    data$Do.you.personally.believe.in.paranormal.phenomena.
  )
)

# =====================================================
# 9 LINEAR REGRESSION
# Predict overall belief from psychic prediction belief
# =====================================================

model_linear <- lm(
  BeliefScore ~ FuturePrediction,
  data = data
)

summary(model_linear)

# =====================================================
# 10 LOGISTIC REGRESSION
# Predict paranormal belief (Yes/No) from belief score
# =====================================================

# Convert belief variable to binary
data$BeliefBinary <- ifelse(
  data$Do.you.personally.believe.in.paranormal.phenomena. == "Yes",
  1,
  0
)

model_logistic <- glm(
  BeliefBinary ~ BeliefScore,
  data = data,
  family = binomial
)

summary(model_logistic)