library(mice)
library(stargazer)
library(tidyverse)

df <-  read.csv("https://raw.githubusercontent.com/tyleransom/DScourseS20/master/ModelingOptimization/wages.csv")
stargazer(df)

df1 <- df %>% drop_na(hgc, tenure)
df2 <- df1 %>% drop_na(logwage)

est <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=df2)
stargazer(est)

df1.mean <- df1
df1.mean$logwage[which(is.na(df1.mean$logwage))] <- mean(df1.mean$logwage, na.rm = TRUE)
est.mean <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=df1.mean)
stargazer(est.mean)

stargazer(est,est.mean)


df.imp = mice(df1, seed = 12345)
summary(df.imp)
fit = with(df.imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
