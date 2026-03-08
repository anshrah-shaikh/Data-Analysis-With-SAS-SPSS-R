# ANSHRAH SHAIKH
# SYCS
# Plots on dataset:
# Data Analysis with SAS SPSS R/Capstone submission/Paranormal Beliefs, Cognitive Style, and Information Processing (Responses).csv


# ===============================
# LOAD LIBRARIES
# ===============================
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(corrplot)

# ===============================
# LOAD DATA
# ===============================
data <- read.csv("C:/Users/ANSI/Desktop/practice/SYCS/sem 4/Data Analysis with SAS SPSS R/Capstone submission/Paranormal Beliefs, Cognitive Style, and Information Processing (Responses).csv")

# ===============================
# CLEAMN TEXT SPACES
# ===============================
data$I.believe.that.some.people.can.predict.the.future.using.supernatural.abilities. <- trimws(
  data$I.believe.that.some.people.can.predict.the.future.using.supernatural.abilities.
)

data$I.usually.rely.on.intuition.rather.than.detailed.analysis.when.making.decisions. <- trimws(
  data$I.usually.rely.on.intuition.rather.than.detailed.analysis.when.making.decisions.
)

# ===============================
# CONVERT LIKERT TEXT TO NUMERIC
# ===============================
data$FuturePrediction <- recode(
  data$I.believe.that.some.people.can.predict.the.future.using.supernatural.abilities.,
  "Strongly Disagree" = 1,
  "Disagree" = 2,
  "Neutral" = 3,
  "Agree" = 4,
  "Strongly Agree" = 5,
  .default = NA_real_
)

# Conversion o intuition question to numeric
data$IntuitionScore <- recode(
  data$I.usually.rely.on.intuition.rather.than.detailed.analysis.when.making.decisions.,
  "Strongly Disagree" = 1,
  "Disagree" = 2,
  "Neutral" = 3,
  "Agree" = 4,
  "Strongly Agree" = 5,
  .default = NA_real_
)

table(data$IntuitionScore)

# ===============================
# REMOVAK OF DUPLICATE COLUMN
# ===============================
data <- data %>% 
  select(-Some.individuals.possess.psychic.powers....2)

# ===============================
# CREATE BELIEF SCORE
# ===============================
data$BeliefScore <- rowMeans(data[,c(
  "FuturePrediction",
  "Ghosts.or.spirits.of.the.dead.can.influence.the.living.",
  "Astrology.can.provide.accurate.insights.into.a.person.s.personality.or.future.",
  "Certain.objects.or.rituals.can.bring.good.or.bad.luck.",
  "Some.individuals.possess.psychic.powers."
)], na.rm = TRUE)

# ===============================
# SECTION 1: DEMOGRAPHICS
# ===============================

ggplot(data, aes(x = Select.your.Age.Group)) +
  geom_bar(fill="steelblue") +
  theme_minimal() +
  labs(title="Age Group Distribution",
       x="Age Group",
       y="Count")

ggplot(data, aes(x = Gender)) +
  geom_bar(fill="purple") +
  theme_minimal() +
  labs(title="Gender Distribution",
       x="Gender",
       y="Count")

# ===============================
# SECTION 2: BELIEF DISTRIBUTION
# ===============================

ggplot(data, aes(x = Do.you.personally.believe.in.paranormal.phenomena.)) +
  geom_bar(fill="darkgreen") +
  theme_minimal() +
  labs(title="Belief in Paranormal Phenomena",
       x="Belief",
       y="Count")

ggplot(data, aes(x = Have.you.ever.experienced.something.you.believe.was.paranormal.)) +
  geom_bar(fill="orange") +
  theme_minimal() +
  labs(title="Paranormal Experience Distribution",
       x="Experience",
       y="Count")

ggplot(data, aes(
  x = Do.you.personally.believe.in.paranormal.phenomena.,
  fill = Have.you.ever.experienced.something.you.believe.was.paranormal.
)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  labs(title="Experience vs Belief",
       x="Belief",
       y="Count",
       fill="Experience")

# ===============================
# SECTION 3: INFLUENCING FACTORS
# ===============================

ggplot(data, aes(x = What.do.you.think.most.influences.your.belief.in.the.paranormal.)) +
  geom_bar(fill="darkred") +
  theme_minimal() +
  coord_flip() +
  labs(title="Factors Influencing Paranormal Beliefs",
       x="Influence",
       y="Count")

ggplot(data, aes(x = If.yes.or.unsure..whose.experience.was.it...)) +
  geom_bar(fill="brown") +
  theme_minimal() +
  coord_flip() +
  labs(title="Source of Paranormal Experience",
       x="Source",
       y="Count")

# ===============================
# SECTION 4: COGNITIVE RELATIONSHIPS
# ===============================

ggplot(data, aes(y = BeliefScore)) +
  geom_boxplot(fill="skyblue") +
  theme_minimal() +
  labs(title="Composite Paranormal Belief Score",
       y="Belief Score")

# ===============================
# SACTTER PLOTS 
# ===============================

# Future Prediction Belief vs Overall Belief
ggplot(data, aes(x = FuturePrediction, y = BeliefScore)) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.6, color="blue") +
  geom_smooth(method="lm", color="red") +
  theme_minimal() +
  labs(
    title="Belief in Future Prediction vs Overall Paranormal Belief",
    x="Belief that People Can Predict the Future",
    y="Overall Paranormal Belief Score"
  )


# Paranormal Belief vs Age Group
data$AgeNumeric <- as.numeric(factor(data$Select.your.Age.Group))

ggplot(data, aes(x = AgeNumeric, y = BeliefScore)) +
  geom_jitter(width = 0.2, alpha = 0.6, color="darkgreen") +
  geom_smooth(method="lm", color="red") +
  theme_minimal() +
  labs(
    title="Age vs Paranormal Belief",
    x="Age Group",
    y="Belief Score"
  )

# Ghost Belief vs Astrology Belief
ggplot(data, aes(
  x = Ghosts.or.spirits.of.the.dead.can.influence.the.living.,
  y = Astrology.can.provide.accurate.insights.into.a.person.s.personality.or.future.
)) +
  geom_jitter(width=0.2, height=0.2, alpha=0.6, color="purple") +
  theme_minimal() +
  labs(
    title="Ghost Belief vs Astrology Belief",
    x="Belief in Ghost Influence",
    y="Belief in Astrology"
  )


# ===============================
# CORRELATION HEATMAP
# ===============================

numeric_data <- data %>%
  select(
    FuturePrediction,
    Ghosts.or.spirits.of.the.dead.can.influence.the.living.,
    Astrology.can.provide.accurate.insights.into.a.person.s.personality.or.future.,
    Certain.objects.or.rituals.can.bring.good.or.bad.luck.,
    Some.individuals.possess.psychic.powers.,
    IntuitionScore
  )

colnames(numeric_data) <- c(
  "Future",
  "Ghosts",
  "Astrology",
  "Luck",
  "Psychic",
  "Intuition"
)

cor_matrix <- cor(numeric_data, use="pairwise.complete.obs")

corrplot(
  cor_matrix,
  method="color",
  type="upper",
  tl.cex=1,
  tl.srt=45,
  addCoef.col="black"
)