## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
library(psych)

# set working directory
setwd("~/Desktop/STA_215")

# Load data 
library(readr)
dataset <- read_csv("raw_data.csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
table(dataset$basketball)
mean(dataset$basketball)
sd(dataset$basketball)

table(dataset$football)
mean(dataset$football)
sd(dataset$football)

table(dataset$pov_over10)
mean(dataset$pov_over10)
sd(dataset$pov_over10)

table(dataset$land_locked)
mean(dataset$land_locked)
sd(dataset$land_locked)

table(dataset$lon_trm)
mean(dataset$lon_trm)
sd(dataset$lon_trm)

table(dataset$pov_avg)
mean(dataset$pov_avg)
sd(dataset$pov_avg)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
anova <- aov(pov_avg ~ pov_over10, data = dataset)
summary(anova)
boxplot(pov_avg ~ pov_over10, data = dataset)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(dataset$nhwhite, dataset$pov_avg)
print(linear_plot)
meany <- mean(dataset$pov_avg)
meanx <- mean(dataset$nhwhite)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(pov_avg ~ nhwhite, data = dataset)
summary(linear_relationship)
abline(linear_relationship, col = "red")



##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$tour_inf, residuals(linear_relationship))
plot(dataset$gdp_per_capital, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$pov_over10,dataset$land_locked)

chisq.test(table(dataset$pov_over10,dataset$land_locked))