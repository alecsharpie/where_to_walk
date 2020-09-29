# load packages
library(tidyverse)
library(lubridate)
library(magrittr)
library(data.table)
library(broom)

# modelling packages
library(modelr)
library(MASS)
library(MXM)


# read in pedestrian count data
ped_count <- read.csv("data/Pedestrian_Counting_System_2009_to_Present_counts_per_hour.csv", 
                      header = TRUE, 
                      stringsAsFactors = FALSE)


#preocess data, setting columns to correct type then taking a sample of the data
melb_data <- ped_count %>%
  mutate(Date_Time = mdy_hms(Date_Time)) %>%
  mutate(date = date(Date_Time)) %>%
  mutate(Time = as.character(Time)) %>%
  mutate(Sensor_ID = as.character(Sensor_ID)) %>%
  drop_na() %>%
  sample_frac(0.01) #%>%
# select(date, Day, Time, Sensor_ID, Hourly_Counts)


#explore relationships between variables

# creating working object
ped_explore <- melb_data

mean(ped_explore$Hourly_Counts)

var(ped_explore$Hourly_Counts)

# create a dataset collapsed to by day
ped_explore_day <- ped_explore %>%
  group_by(date, Day, sun_j, rain_mm, temp_c)%>%
  summarise(daily_max = max(Hourly_Counts),
            daily_avg = median(Hourly_Counts))


##### Explore Models for one sensor only (melbourne central) ####

#preocess data, setting columns to correct type then taking a sample of the data
mc_ped <- ped_count %>%
  mutate(Date_Time = mdy_hms(Date_Time)) %>%
  mutate(date = date(Date_Time)) %>%
  mutate(Time = as.character(Time)) %>%
  mutate(Sensor_ID = as.character(Sensor_ID)) %>%
  drop_na() %>%
  filter(Sensor_Name == "Melbourne Central")

# create a working object
mc_ped_explore <- mc_ped

# order Day of the week factor
mc_ped_explore$Day <- factor(mc_ped$Day,
                             levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                             ordered = TRUE)

# order Day of the week factor
mc_ped_explore$Time <- factor(mc_ped$Time,
                              levels = as.character(c(0:23)),
                              ordered = TRUE)

# Day of the week
ggplot()+
  geom_boxplot(data = mc_ped_explore, aes(x = Day, y = Hourly_Counts))

# Time of day
ggplot()+
  geom_boxplot(data = mc_ped_explore, aes(x = Time, y = Hourly_Counts), outlier.shape = NA)

# explore hourly counts
par(mfrow = c(1, 1))
hist(mc_ped_explore$Hourly_Counts, breaks = 1000)


mean(mc_ped_explore$Hourly_Counts)

var(mc_ped_explore$Hourly_Counts)

# create by day dataset
ped_explore <- ped %>%
  drop_na()

# create a dataset collapsed to by day
ped_explore_day <- ped_explore %>%
  group_by(Sensor_ID, date, Day, sun_j, rain_mm, temp_c)%>%
  summarise(daily_max = max(Hourly_Counts),
            daily_avg = median(Hourly_Counts))


# Millimeters of rain vs counts
ggplot()+
  geom_point(data = ped_explore_day, aes(x = rain_mm, y = daily_max))+
  geom_smooth(data = ped_explore_day, aes(x = rain_mm, y = daily_max))+
  scale_x_continuous(limits = c(0,25))+
  facet_wrap(~ Sensor_ID, ncol = 10)

# Joules of solar energy vs counts
ggplot()+
  geom_point(data = ped_explore_day, aes(x = sun_j, y = daily_max))+
  geom_smooth(data = ped_explore_day, aes(x = sun_j, y = daily_max))+
  facet_wrap(~ Sensor_ID, ncol = 10)

# Temperature vs counts
ggplot()+
  geom_point(data = ped_explore_day, aes(x = temp_c, y = daily_max))+
  geom_smooth(data = ped_explore_day, aes(x = temp_c, y = daily_max))+
  facet_wrap(~ Sensor_ID, ncol = 10)


# source http://www.learnbymarketing.com/tutorials/linear-regression-in-r/

# get data ready for model, remove missing values
mc_ped_model <- mc_ped %>%
  drop_na()

ped_model <- ped %>%
  drop_na()%>%
  sample_frac(0.01)%>%
  filter(Time %in% c("7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))


mean(ped_model$Hourly_Counts)
sd(ped_model$Hourly_Counts)
var(ped_model$Hourly_Counts)
max(ped_model$Hourly_Counts)



# investigate amount of data for each sensor
ped_model %>%
  group_by(Sensor_ID)%>%
  summarise(n = n())%>%
  arrange(n)



hist(ped_model$Hourly_Counts, breaks = 1000)

#https://stats.stackexchange.com/questions/336613/regression-using-circular-variable-hour-from-023-as-predictor/336646#336646
# create time a circular variable


# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
##### follow this tute !!!!!!! ####

#sample rows for a regression plot in R
library(data.table)
mtcars[sample(.N, 6)]

mc_ped_model %>% dplyr::sample_frac(0.1)

model_mc %>% dplyr::sample_frac(0.1)

#create a linear model for melbourne central

model_mc <- lm(Hourly_Counts ~ Day + Time + sun_j + rain_mm + temp_c, data = mc_ped_model)

#summary of model
summary(model_mc)

model_mc2 <- lm(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + poly(temp_c, 2), data = mc_ped_model)

model_mc3 <- lm(Hourly_Counts ~ Day + Time + sun_j + rain_mm + poly(temp_c, 2), data = mc_ped_model)

model_mc4 <- lm(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + temp_c, data = mc_ped_model)


model_mc_pois <- glm(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + poly(temp_c, 2), data = mc_ped_model, family = poisson(link = "log"))

model_mc_qpois <- glm(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + poly(temp_c, 2), data = mc_ped_model, family = quasipoisson(link = "log"))

model_qpois <- glm(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + poly(temp_c, 2) + Sensor_ID, data = ped_model, family = quasipoisson(link = "log"))

model_negbin <-  glm.nb(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + poly(temp_c, 2) + Sensor_ID, data = ped_model)
# 8:18pm => 10:56

model_pois <- glm(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + poly(temp_c, 2), data = ped_model, family = poisson(link = "log"))


#summary of model
summary(model_pois)

par(mfrow = c(2, 2))
plot(model_pois)

par(mfrow = c(1, 1))
hist(residuals(model_pois), breaks = 100)

plot(predict(model_pois), log(ped_model$Hourly_Counts))
abline(0, 1, col = "red")

plot(exp(predict(model_pois)), ped_model$Hourly_Counts)
abline(0, 1, col = "red")



#summary of model
summary(model_mc_pois)

par(mfrow = c(1, 2))
plot(model_mc_pois)

par(mfrow = c(1, 1))
hist(residuals(model_mc_pois), breaks = 100)

plot(log(mc_ped_model$Hourly_Counts), predict(model_mc_pois))
abline(0, 1, col = "red")

plot(mc_ped_model$Hourly_Counts, exp(predict(model_mc_pois)))
abline(0, 1, col = "red")


#summary of model
summary(model_negbin)

mean(residuals(model_negbin))

par(mfrow = c(2, 2))
plot(model_negbin)

par(mfrow = c(1, 1))
hist(residuals(model_negbin), breaks = 1000)

plot(log(ped_model$Hourly_Counts), predict(model_negbin))
abline(0, 1, col = "red")

plot(ped_model$Hourly_Counts, exp(predict(model_negbin)))
abline(0, 1, col = "red")

plot(predict(model_negbin), ped_model$Hourly_Counts)
abline(0, 1, col = "red")


#summary of model quasi
summary(model_mc_qpois)

par(mfrow = c(2, 2))
plot(model_mc_qpois)

par(mfrow = c(1, 1))
hist(residuals(model_mc_qpois), breaks = 100)

plot(mc_ped_model$Hourly_Counts, predict(model_mc_qpois))
abline(0, 1, col = "red")

plot(mc_ped_model$Hourly_Counts, exp(predict(model_mc_qpois)))
abline(0, 1, col = "red")



#summary of model
summary(model_qpois)

mean(residuals(model_qpois))

par(mfrow = c(1, 2))
plot(model_qpois)

par(mfrow = c(1, 1))
hist(residuals(model_qpois), breaks = 100)

plot(log(ped_model$Hourly_Counts), predict(model_qpois))
abline(0, 1, col = "red")

plot(ped_model$Hourly_Counts, exp(predict(model_qpois)))
abline(0, 1, col = "red")



# see model diagnostics
model_mc_diag_qpois <- augment(model_mc_qpois)
head(model_mc_diag_qpois)

# creating columns for sun and temp using 
model_mc_diag_qpois <- model_mc_diag_qpois %>%
  mutate(temp = as.numeric(model_mc_diag_qpois$poly.temp_c..2.[,2])) %>%
  mutate(sun = as.numeric(model_mc_diag_qpois$poly.sun_j..2.[,2]))



#create graphs of the variables and regression lines
ggplot(model_mc_diag_qpois, aes(x = temp, y = Hourly_Counts)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = temp, yend = .fitted), color = "red", size = 0.3)


model_mc_diag_qpois$poly.temp_c..2.

#summary of model
summary(model_mc2)

#plot a summary of the model
par(mfrow = c(2, 2))
plot(model_mc2)

hist(residuals(model_mc2), breaks = 1000)

mean(residuals(model_mc2))

library(olsrr)

ols_plot_resid_hist(model_mc2)

# see model diagnostics
model_mc_diag2 <- augment(model_mc2)
head(model_mc_diag2)

#create graphs of the variables and regression lines
ggplot(model.diag.metrics, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = youtube, yend = .fitted), color = "red", size = 0.3)

# get the confidence intervals of the model
confint(model_mc2, level=0.95)

# plot the residuals against the fitted values to check for underlying pattern
plot(fitted(model_mc2),residuals(model_mc2))

# plot the residuals against rain to check for underlying pattern
plot(mc_ped_model$rain_mm, residuals(model_mc2))

# plot the residuals against sun to check for underlying pattern
plot(mc_ped_model$sun_j, residuals(model_mc2))

# plot the residuals against temp to check for underlying pattern
plot(mc_ped_model$temp_c,residuals(model_mc2))

# plot the residuals against actuals to check for underlying pattern
plot(mc_ped_model$Hourly_Counts, predict(model_mc2))
abline(0, 1, col = "red")

# plot the residuals against actuals to check for underlying pattern
plot(mc_ped_model$Hourly_Counts, predict(model_mc))
abline(0, 1, col = "red")



# source http://www.science.smith.edu/~jcrouser/SDS293/labs/lab12-r.html


#compare the fit of the models 
anova(model_mc, model_mc2)
anova(model_mc2, model_mc3)
anova(model_mc2, model_mc3)
anova(model_mc2, model_mc4)


# The second models provides a significantly better fit
# because of the low p value in the anova

# trying to compute residual variance
(summary(model_mc)$sigma)**2
(summary(model_mc2)$sigma)**2

agelims = Wage %>%
  select(age) %>%
  range

# Generate a sequence of age values spanning the range
age_grid = seq(from = min(agelims), to = max(agelims))

# Predict the value of the generated ages,
# returning the standard error using se = TRUE
preds = predict(fit, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, 
                 "lower" = preds$fit-2*preds$se.fit)
#Finally, we plot the data and add the fit from the degree-4 polynomial.

ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = preds$fit), color = "#0000FF") +
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Degree-4 Polynomial")

hist(model_mc$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(model_mc$resid)
qqline(model_mc$resid)


library(fBasics)

jarqueberaTest(model_mc2$resid)

library(lmtest) #dwtest

dwtest(model_mc)


# The mean of the errors is zero (and the sum of the errors is zero).
# The distribution of the errors are normal.
# All of the errors are independent.
# Variance of errors is constant (Homoscedastic)

# create dummy dataframe to test predicting set variables
for_pred <- tibble(Day = c("Friday", "Saturday", "Thursday"), Time = c("14","23", "8" ), sun_j = c(25, 25, 25), rain_mm = c(0, 0, 0), temp_c = c(17, 17, 17))


# predict from variables
predict(model_mc2, for_pred)

predict(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
        interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"),
        terms = NULL, na.action = na.pass,
        pred.var = res.var/weights, weights = 1)


##### EXPLORE MODELS #####

# test if month makes a big difference to hourly_count
summary(lm(Hourly_Counts ~ Month, data = melb_data))

# test if sensor ID makes a big difference to hourly_count
summary(lm(Hourly_Counts ~ Sensor_ID, data = melb_data))

# USE MANY MODELS TO RUN A MODEL FOR EACH SENSOR



# negative binomial model

nb_model <- glm.nb(Hourly_Counts ~ Day + Time, data = melb_data)

summary(nb_model)

AIC(nb_model)
logLik(nb_model)

par(mfrow = c(2, 2))
plot(nb_model)

par(mfrow = c(1, 1))
hist(residuals(nb_model), breaks = 1000)

plot(predict(nb_model), log(melb_data$Hourly_Counts))
abline(0, 1, col = "red")

plot(exp(predict(nb_model)), melb_data$Hourly_Counts)
abline(0, 1, col = "red")


# negative binomial model with sensors

nb_model_ws <- glm.nb(Hourly_Counts ~ Day + Time + Month + Sensor_ID, data = melb_data)

summary(nb_model_ws)

AIC(nb_model_ws)
logLik(nb_model_ws)

par(mfrow = c(2, 2))
plot(nb_model_ws)

par(mfrow = c(1, 1))
hist(residuals(nb_model_ws), breaks = 100)

plot(predict(pois_model), log(ped_model$Hourly_Counts))
abline(0, 1, col = "red")

plot(exp(predict(pois_model)), ped_model$Hourly_Counts)
abline(0, 1, col = "red")

anova(nb_model, nb_model_ws)


#convert time to numeric
melb_data$Time <- as.numeric(melb_data$Time)

# nb model with circular predictors

nb_model_wcp <- glm.nb(Hourly_Counts ~ Day + cos(pi*Time/12) + sin(pi*Time/12) + Sensor_ID, data = melb_data)

summary(nb_model_wcp)

extract_eq(nb_model_wcp)

AIC(nb_model_wcp)
logLik(nb_model_wcp)

par(mfrow = c(2, 2))
plot(nb_model_wcp)

par(mfrow = c(1, 1))
hist(residuals(nb_model_wcp), breaks = 100)

plot(predict(nb_model_wcp), log(melb_data$Hourly_Counts))
abline(0, 1, col = "red")

plot(exp(predict(nb_model_wcp)), melb_data$Hourly_Counts)
abline(0, 1, col = "red")


# models only containing time

#convert time to numeric
melb_data$Time <- as.numeric(melb_data$Time)

nb_model_ct <- glm.nb(Hourly_Counts ~ cos(pi*Time/12) + sin(pi*Time/12), data = melb_data)

summary(nb_model_ct)

AIC(nb_model_ct)

par(mfrow = c(2, 2))
plot(nb_model_ct)

plot(predict(nb_model_ct), log(melb_data$Hourly_Counts))
abline(0, 1, col = "red")

plot(exp(predict(nb_model_ct)), melb_data$Hourly_Counts)
abline(0, 1, col = "red")


# convert time to character
melb_data$Time <- as.character(melb_data$Time)

nb_model_t <- glm.nb(Hourly_Counts ~ Time, data = melb_data)

summary(nb_model_t)

AIC(nb_model_t)

par(mfrow = c(2, 2))
plot(nb_model_t)

plot(predict(nb_model_t), log(melb_data$Hourly_Counts))
abline(0, 1, col = "red")

plot(exp(predict(nb_model_t)), melb_data$Hourly_Counts)
abline(0, 1, col = "red")





# poisson model

pois_model <- glm(Hourly_Counts ~ Day + Time + Sensor_ID, data = melb_data, family = poisson(link = "log"))

summary(pois_model)

AIC(pois_model)

par(mfrow = c(2, 2))
plot(pois_model)

par(mfrow = c(1, 1))
hist(residuals(pois_model), breaks = 100)

plot(predict(pois_model), log(ped_model$Hourly_Counts))
abline(0, 1, col = "red")

plot(exp(predict(pois_model)), melb_data$Hourly_Counts)
abline(0, 1, col = "red")



model_negbin <-  glm.nb(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + poly(temp_c, 2) + Sensor_ID, data = ped_model)


model_pois <- glm(Hourly_Counts ~ Day + Time + poly(sun_j, 2) + rain_mm + poly(temp_c, 2), data = ped_model, family = poisson(link = "log"))

# create a list of all sensors with circular time variables
# https://chroniclesofcalculation.wordpress.com/tag/trigonometry/


#summary of model
summary(pois_model)

par(mfrow = c(2, 2))
plot(pois_model)

par(mfrow = c(1, 1))
hist(residuals(pois_model), breaks = 100)

plot(predict(pois_model), log(ped_model$Hourly_Counts))
abline(0, 1, col = "red")

plot(exp(predict(pois_model)), ped_model$Hourly_Counts)
abline(0, 1, col = "red")







