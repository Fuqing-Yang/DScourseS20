library(stargazer)
library(tidyverse)
library(sampleSelection)


df <- read.csv("https://raw.githubusercontent.com/tyleransom/DScourseS20/master/Structural/wages12.csv")
college <- as.factor(df$college)
married <- as.factor(df$married)
union <- as.factor(df$union)
stargazer(df)


reg1 <- lm(logwage ~ hgc + exper + I(exper^2) + college + union, data=df, na.action = na.omit)
print(summary(reg1))               


wagesmean <- df
wagesmean$logwage[is.na(wagesmean$logwage)] <- mean(wagesmean$logwage, na.rm = TRUE)
reg2 <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = wagesmean)

df <- df%>% mutate(
  valid = if_else(
    is.na(logwage)==TRUE, 0, 1
  )
)

df$logwage[is.na(df$logwage)] <- 0


reg3 <- selection(selection = valid ~ hgc + union + college + exper + married + kids,
          outcome = logwage ~ hgc + union + college + exper + I(exper^2),
          data = df, method="2step")
stargazer(reg1, reg2, reg3, title = "Results")

#8 Probit model
estim <- glm(union ~ hgc + college + exper + married + kids, 
             family = binomial(link='probit'), data = df)

print(summary(estim))
union <- predict(estim, newdata = df, type = "response")
print(summary(df$union))

#9
estim$coefficients["married"] <- 0
estim$coefficients["kids"] <- 0

df$predcounterfactual <- predict(estim, newdata = df, type = "response")
summary(df$predcounterfactual)




