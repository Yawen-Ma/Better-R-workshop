View(df)

myfunction <- function() {
  df <- df |> mutate(Sex = if_else(Female == 1, "Female", "Male"), .after = Female) |> mutate(AHI = factor(AHI))
  return(df)
}
df <- myfunction()

df <- df |> 
  mutate(Diabetesfct = factor(Diabetes))
ggplot(df) +   geom_bar(aes(AHI, fill=Diabetesfct),position ="fill")+scale_fill_brewer(palette ="Dark2")+ theme_minimal() # no colours

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