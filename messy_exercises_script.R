
install.packages("readr")
install.packages("writexl")
library(readxl)
library(readexcel)
library(readr)
df <- read_csv("C:/Users/username/OneDrive/R code/hypoxia.csv")

View(df)

# histograms
hist(df$Age) # don't use this
hist(df$BMI)
hist(df$Sleeptime)
hist(df$AHI)
# An AHI of 5-14 is mild; 15-29 is moderate and 30 or more events per hour characterizes severe sleep apnea.
# All normal????
# 1 = (AHI < 5); 2 = (5 ≤ AHI < 15);
# 3 = (15 ≤ AHI < 30); 4 = (AHI ≥ 30)

myfunction <- function() {
  df <- df |> mutate(Sex = if_else(Female == 1, "Female", "Male"), .after = Female) |> mutate(AHI = factor(AHI))
  return(df)
}
df <- myfunction()

library(ggplot2)
ggplot(df) +
  geom_histogram(aes(Age))
ggplot(df) +
  geom_histogram(aes(AHI))
ggplot(df) +
  geom_bar(aes(AHI))

library(ggcorrplot)
# doesn't work error 
# ggcorrplot::ggcorrplot(cor(df))

df |> 
  select(where(is.numeric)) |> 
  cor(use = "complete.obs") |> 
  ggcorrplot()

# save as plot - PNG format
# width 5 inch, height = 5 inch for paper

ggplot(df) +
  geom_bar(aes(AHI)) +
  facet_wrap(~Sex)

ggplot(df) +
  geom_bar(aes(AHI)) +
  facet_wrap(Race~Sex) 

ggplot(df) +
  geom_bar(aes(AHI)) +
  facet_grid(Race~Sex) 

ggplot(df) +
  geom_bar(aes(AHI, fill = Diabetes)) # no colours

df <- df |> 
  mutate(Diabetesfct = factor(Diabetes))
ggplot(df) + geom_bar(aes(AHI, fill=Diabetesfct), position ="fill") + scale_fill_brewer(palette ="Dark2")+ theme_minimal() # no colours

library(dplyr)

mean(df$Age) #50.65667
sd(df$Age)
mean(df$BMI) #46.74875
table(df$AHI) #?missing
table(df$`Duration of Surg`)
mean(df$`Duration of Surg`) #4.314947
# error - don't know why

#tests
chisq.test(table(df$AHI, df$Sex))
chisq.test(table(df$AHI, df$Race))
chisq.test(table(df$AHI, df$Smoking))
chisq.test(table(df$AHI, df$Diabetes))

BMI1 <- df |> filter(BMI <= median(BMI))
BMI2 <- df |> filter(BMI > median(BMI))
t.test(BMI1$Sleeptime, BMI2$Sleeptime, var.equal = T)
t.test(BMI1$Sleeptime, BMI2$Sleeptime, var.equal = T)$p.value

# models

df <- df |> 
  mutate(severeAHI = if_else(AHI ==4, 1, 0))

mod1 <- glm(severeAHI~., data = df, family = "binomial")
summary(mod1)

mod_data <- df |> select(-c(Female, AHI))
mod1 <- glm(severe_AHI~., data = mod_data, family = "binomial")
summary(mod1)

mod2 <- glm(severe_AHI~Age+Sex+BMI, data=mod_data, family = "binomial")
summary(mod2)

mod3 <- glm(severe_AHI~Age+Sex, data=mod_data, family= "binomial")
summary(mod3)

mod4 <- glm(severe_AHI~Age+BMI, data=mod_data, family ="binomial")
summary(mod4)

mod5 <- glm(severe_AHI~Sex+BMI, data=mod_data, family ="binomial")
summary(mod5)

mod6 <- glm(severe_AHI~Age, data=mod_data, family ="binomial")
summary(mod6)

mod7 <- glm(severe_AHI~Sex, data=mod_data, family ="binomial")
summary(mod7)

mod8 <- glm(severe_AHI~BMI, data=mod_data, family ="binomial")
summary(mod8)

c(mod2$aic, mod3$aic, mod4$aic, mod5$aic, mod6$aic, mod7$aic, mod8$aic)
# mod2 lowest 

# results
library(ggforestplot)
library(forestmodel)
forest_model(mod2)
ggsave("forestplot.png")
# saved as forestplot.png
