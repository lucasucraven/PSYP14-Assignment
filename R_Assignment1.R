#ASSIGNMENT 1

library(psych)
library(car)
library(lmtest)
library(sandwich)
library(dplyr)
library(tidyverse)
library(lm.beta)

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

dataRAW = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")


summary(dataRAW)
describe(dataRAW)

#Check data for error

dataRAW %>% 
  filter(pain>10) 
dataRAW %>% 
  filter(STAI_trait<20)

## Subject 88 has a pain score of 55, which is above the maximum score of the scale
## Subject 34 has a STAI_trait score of 4,2, which is below the minimum score of the scale

## So subject 88 and 34 are excluded from the data
data <- dataRAW[-c(88, 34), ]  

data %>% 
  filter(pain>10) 
data %>% 
  filter(STAI_trait<20)

data$sex <- as.factor(data$sex)
class(data$sex)
str(data$sex)
levels(data$sex)
str(data)
summary(data)
## age and sex as predictors of pain (model 1)
## age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures (model 2).

model1 <- lm(pain ~ age + sex, data = data)
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data)

summary(model2)$adj.r.squared
summary(model1)$adj.r.squared	

AIC(model1)
AIC(model2)

anova(model1, model2)
# Big difference in how much variance is explained
# statisical signifiance that these two model are different

## CHECK ASSUMPTIONS FOR BOTH MODELS

#Identiying outliers
model1 %>%
  plot(which = 5)
model1 %>%
  plot(which = 4)

model2 %>%
  plot(which = 5)
model2 %>%
  plot(which = 4)
# 47, 74, 86 high residual error and high leverage cases

data %>% 
  slice(c(46,73,85))

## Normality
#QQPlot
model1 %>%
  plot(which = 2)

model2 %>%
  plot(which = 2)
## 104, 85, 74

##Histogram
residuals_model1 = enframe(residuals(model1))
residuals_model1 %>%
  ggplot() + aes(x = value) + geom_histogram()

residuals_model2 = enframe(residuals(model2))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model1))
describe(residuals(model2))
## skew= -0.15, kurtosis= -0.03
## Since the identified outliers are not extreme cases and normality is assumed, I will stick will model2

##Linearity
model1 %>%
  residualPlots()
model2 %>%
  residualPlots()
## non-significant test - > assuming linearity 

##Homoscdasticity
model1 %>% 
  plot(which=3)

model2 %>% 
  plot(which=3)

model1 %>% 
  ncvTest()

model2 %>% 
  ncvTest()

# P= 0.87
model1 %>% 
  bptest()

model2 %>% 
  bptest()
# P= 0.87
## non-significant test -> assuming homoscedasticity

##No Multicollinearity
model1 %>% 
  vif()

model2 %>% 
  vif()
# cortisol_serum and cortisol_saliva vif scores are above 3 assuming data multicollinearity

data %>%
  select(cortisol_serum,cortisol_saliva,STAI_trait,pain_cat,mindfulness) %>%
  pairs.panels(col = "red", lm = T)

#cortisol measures are 0.85 correlated, because of this we cannot trust the coefficients or t-tests
# cortisol_saliva is excluded
model2nosaliva <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data)

## Normality
#QQPlot
model2nosaliva %>%
  plot(which = 2)
## 104, 85, 74

##Histogram
residuals_model2 = enframe(residuals(model2nosaliva))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2nosaliva))
## skew= -0.18, kurtosis= 0.02
## Since the identified outliers are not extreme cases and normality is assumed, I will stick will model2

##Linearity
model2nosaliva %>%
  residualPlots()
## non-significant test - > assuming linearity 

##Homoscedasticity
model2nosaliva %>% 
  plot(which=3)

model2nosaliva %>% 
  ncvTest()
# P= 0.80

model2nosaliva %>% 
  bptest()
# P= 0.89
## non-significant test -> assuming homoscedasticity

##No Multicollinearity
model2nosaliva %>% 
  vif()

#Model Comparison
summary(model1)
summary(model2nosaliva)
confint(model1)
confint(model2nosaliva)
AIC(model1)
AIC(model2nosaliva)

str(data)

anova(model1,model2nosaliva)
coef_table(model1)
coef_table(model2nosaliva)

#ASSIGNMENT 2
#check data - Fine
model3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data)
coef(model3)
#Identiying outliers
model3 %>%
  plot(which = 5)
model3 %>%
  plot(which = 4)
# 46, 84, 85 high residual error and high leverage cases

data %>% 
  slice(c(46,84,85))

## Normality
#QQPlot
model3 %>%
  plot(which = 2)
## 104, 85, 86

##Histogram
residuals_model3 = enframe(residuals(model3))
residuals_model3 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model3))
## skew= -0.17, kurtosis= -0.08
## Since the identified outliers are not extreme cases and normality is assumed, I will stick will model2

##Linearity
model3 %>%
  residualPlots()
## non-significant test - > assuming linearity 

##Homoscdasticity
model3 %>% 
  plot(which=3)

model3 %>% 
  ncvTest()
# P= 0.87

model3 %>% 
  bptest()
# P= 0.87
## non-significant test -> assuming homoscedasticity

##No Multicollinearity
model3 %>% 
  vif()


backward.model = step(model3, direction = "backward")


summary(model3)
summary(backward.model)
summary(model2nosaliva)
confint(backward.model)
confint(model2nosaliva)
AIC(backward.model)
AIC(model2nosaliva)
AIC(model3)
coef_table(backward.model)
#ANOVA BECAUSE NESTED MODELS
anova(model2nosaliva,backward.model)

dataRAW2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")

summary(dataRAW2)
describe(dataRAW2)

dataRAW2$sex <- as.factor(dataRAW2$sex)
class(dataRAW2$sex)
str(data)

# calculate predicted values
pred_tbm <- predict(model2nosaliva, dataRAW2)
pred_bwm <- predict(backward.model, dataRAW2)
# calculate the sum of squared residuals
RSS_tbm <- sum((dataRAW2[, "pain"] - pred_tbm)^2)

RSS_bwm <- sum((dataRAW2[, "pain"] - pred_bwm)^2)

RSS_tbm
RSS_bwm

# This test reveals that the backward regression model has more error than the theory based model.

#Assignment 3

library(lme4)
library(lattice)
library(lmerTest)
library(tidyverse)
library(r2glmm)
library(psych)
library(car)
library(MuMIn)
library(ggplot2)
library(sjstats)

stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	
error_plotter <- function(mod, col = "black", x_var = NULL) {
  mod_vars = as.character(mod$call[2])
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern = "~",
                                              mod_vars)) - 2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern = "~", mod_vars)) +
               2, nchar(mod_vars))
  data$pred = predict(mod)
  if (x == "1" & is.null(x_var)) {
    x = "response_ID"
    data$response_ID = 1:nrow(data)
  } else if (x == "1") {
    x = x_var
  }
  plot(data[, y] ~ data[, x], ylab = y, xlab = x)
  abline(mod)
  for (i in 1:nrow(data)) {
    clip(min(data[, x]), max(data[, x]), min(data[i, c(y,
                                                       "pred")]), max(data[i, c(y, "pred")]))
    abline(v = data[i, x], lty = 2, col = col)
  }
}

datafile3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
str(datafile3)
summary(datafile3)

#sex and hospital as factor variable
datafile3 = datafile3 %>%
  mutate(hospital = factor(hospital), sex = factor(sex))

#hospital as ordered factor
datafile3$hospital <- factor(datafile3$"hospital", levels = c("hospital_1",
                                           "hospital_2", 
                                           "hospital_3", 
                                           "hospital_4",
                                           "hospital_5",
                                           "hospital_6",
                                           "hospital_7",
                                           "hospital_8",
                                           "hospital_9",
                                           "hospital_10"), ordered = TRUE)
levels(datafile3$hospital)

# converting women to female
datafile3[25,"sex"] <- "female"
# dropping all factor levels not in use (women)
datafile3 <- droplevels(datafile3)


#Create model

mod_rnd_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = datafile3)

datafile3 = datafile3 %>%
  mutate(resid = residuals(mod_rnd_int))
summary(mod_rnd_int)
confint(mod_rnd_int)
std.coef(mod_rnd_int, partial.sd = FALSE)

#calculate RSS
sum(residuals(mod_rnd_int)^2) #224.3138
#calculate marginal R^2
r2beta(mod_rnd_int, method = "nsj", data = datafile3)
#calculate conditional R^2
r.squaredGLMM(mod_rnd_int)

datafile4 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")

str(datafile4)
summary(datafile4)
describe(datafile4)

datafile4 = datafile4 %>%
  mutate(hospital = factor(hospital), sex = factor(sex))

#hospital as ordered factor
datafile4$hospital <- factor(datafile4$"hospital", levels = c("hospital_11",
                                          "hospital_12", 
                                          "hospital_13", 
                                          "hospital_14",
                                          "hospital_15",
                                          "hospital_16",
                                          "hospital_17",
                                          "hospital_18",
                                          "hospital_19",
                                          "hospital_20"), ordered = TRUE)
class(datafile4$hospital)
levels(datafile4$hospital)


pred_mod_rnd <- predict(mod_rnd_int, newdata = datafile4, allow.new.levels = TRUE)

pred_mod_mean<- mean(pred_mod_rnd)

TSS_mod_rnd <- sum((datafile4[, "pain"] - pred_mod_mean)^2)

RSS_mod_rnd <- sum((datafile4[, "pain"] - pred_mod_rnd)^2)
RSS_mod_rnd
r2_mod_rnd <- 1-(RSS_mod_rnd/TSS_mod_rnd)
r2_mod_rnd
#r^2 = 0.38

#calculate marginal R^2
r2beta(mod_rnd_int, method = "nsj", data = datafile3)
#calculate conditional R^2
r.squaredGLMM(mod_rnd_int)

mo_inf_pred = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = datafile3)
# warning - boundary (singular) fit: see ?isSingular

datafile3 = datafile3 %>% 
  mutate(pred_int = predict(mo_inf_pred), pred_slope = predict(mo_inf_pred))

datafile3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + 
  geom_line(color = "red",aes(y = pred_slope, x = cortisol_serum)) + 
  facet_wrap(~hospital, ncol = 2)

#the regression line is similar and flat in most hospitals - no effect
#in individual hospitals

mod_mean <- lm(pain ~ 1, data = datafile3)
error_plotter(mod_mean, col = "red", x_var = "cortisol_serum")

TSS_inf_pred <- sum((datafile3$pain - predict(mod_mean))^2)

RSS_inf_pred <- sum(residuals(mo_inf_pred)^2)

r2_inf_pred <- 1-(RSS_inf_pred/TSS_inf_pred)

mean(datafile3$pain)
mean(data$pain)