#==========================================================================================
# INTRODUCTION TO DATA ANALYSIS AND REGRESSION
#==========================================================================================

#==========================================================================================
# BASICS OF R: LOAD THE NECESSARY PACKAGES

# R works using packages. Thus you need to load the packages you will use using the library
# command. But if the package does not exist in your computer you can install it using the 
# install.packages command. For example:

library(ggplot2) # Loads the ggplot package

# If the package does not exist in your computer, you can install it using the install.packages
# command. But for the purposes of this file I will not activate it

#install.packages("ggplot2") # Remove the number sign at the beginning to activate it. 

# To make things quick I use a function that works on an object that contains the list c(...)
# of packages:

needed.packages <- c("foreign","ggplot2","rstudioapi","tidyverse","dplyr","stargazer",
                     "broom","lfe")  

# This command installs all packages listed in needed.packages and their dependencies.

#install.packages(needed.packages, dependencies=TRUE)  

# This command will tell R we will use all packages listed in needed.packages

sapply(needed.packages, require, character.only = TRUE)

#==========================================================================================
# SET THE WORKING DIRECTORY: THE PLACE IN THE COMPUTER WHERE WE WILL WORK (I.E., THE PATH)

setwd("D:/Documents/Dropbox/NYU/Dissertation - phase/Things on the side/Political Economy Honors (STERN)/Slides/Recitation 4 - Interaction Effects")

# I like using this shortcut which tells R that the folder where my R-file is defines the path:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd() # Check it

#==========================================================================================
# CLEAN THE WORK SPACE AND SET A SEED

rm(list= ls())  # This deletes everything that is the environment.

set.seed(12345) # Setting a seed is important so that you can replicate things if a command
# uses random number generation.

#==========================================================================================
# SIMULATION NO 4: EFFECT OF EDUCATION ON WAGES
#==========================================================================================
# The person interviewed is 18 years of age

#==========================================================================================
# FIRST LET US CREATE THE SIMULATION
n_interviewed = 500 # number of individuals surveyed in our data
n_periods = 10 # number of periods each individual is interviewed

# First we create the basics for the panel structure 

id=c(1:n_interviewed) # Person id
fe=rbinom(n_interviewed,100,0.4)/100 # Fixed effect: Innate skill score
preschool=sample(c(0,1),n_interviewed,replace=TRUE) # Time invariant confounder
education   = rbinom(n_interviewed,12,0.4) # Generating the baseline levels of education at age 20

# Create a treatment condition for the difference in differences estimator
treatment_DID= sample(c(0,1),n_interviewed,replace=TRUE)

# Time-invariant proportion of the data
data<-data.frame(id,fe,education,preschool,treatment_DID)

# Now we add the time dimension to the panel structure
data <- data %>% slice(rep(1:n(), each = n_periods)) # Duplicate observations
data <- data %>% group_by(id) %>% mutate(age = 18+row_number()) # Add period variable

# Adding some exogenous time variation to treatment: this is essential!
data$education <- data$education+rnorm(n_interviewed*n_periods, mean = 3, sd = 0.2)


# Time-variant confounder
data <- data %>% group_by(id) %>% mutate(health = row_number()*rnorm(1, mean = 0, sd = 1))

# Create data generating process for outcome

# Modify the data generating process for the outcome
data$wage <- 100 + 5*data$education+data$health+50*data$preschool+100*data$fe+50*data$age+
  rnorm(n_interviewed*n_periods, mean = 0, sd = 1)

# Endogenize education to create confounding
data$education <- data$education+data$health+data$preschool+10*data$fe+0.1*data$age

#==========================================================================================
# FIXED EFFECTS REGRESSION TABLES

# First we do not control for confounders nor fixed effects
reg_no_1<- felm( wage ~ education,  data=data)

# Second control for individual fixed effects
reg_no_2<- felm( wage ~ education | id ,  data=data)

# Third we control for time/age + individual fixed effects
reg_no_3<- felm( wage ~ education | age + id,  data=data)

# Second we control for time variant confounders (i.e., health)
reg_no_4<- felm( wage ~ education + health | age + id,  data=data)

# Visualizing a beautiful table:

stargazer(reg_no_1,reg_no_2,reg_no_3,reg_no_4,type="text",out="table.html")

#==========================================================================================
# DIFFERENCES IN DIFFERENCES

# Modify the data generating process for the outcome to add the intervention: 2 more years of Ed.
data[which(data$age>4+18 & data$treatment_DID==1),]$wage = 
  data[which(data$age>4+18 & data$treatment_DID==1),]$wage + 
  3*(data[which(data$age>4+18 & data$treatment_DID==1),]$education+1)

# Create the varible that identifies the treatment period
data$treatment_period=(data$age>4+18)

# Second we control for time variant confounders
reg_no_5<- felm( wage ~ education + education:treatment_period | age + id,  data=data)

# Visualizing a beautiful table:
stargazer(reg_no_5,type="text")

#==========================================================================================
# REGRESSION PLOTS
reg_no_6<- felm( wage ~ education + education:factor(age) | age + id,  data=data)
tidy_reg<-tidy(reg_no_6) %>% filter(term!='education')
tidy_reg$period=c(2:10)-5

# Ordering the models (R doesn't understand what's the right order, and assumes alphabetical)
coef_plot<-ggplot(tidy_reg, aes(x=period, y=estimate))+
  geom_point()+
  geom_pointrange(aes(ymin = estimate-1.96*std.error, ymax =  estimate+1.96*std.error))+
  geom_hline(yintercept=0, color = "red") +
  labs(title = "Coefficients")  +
  labs(title="Impact of education on wages", x="Period", y="Coefficient size") +
  theme_classic()  +
  theme(legend.position='top', plot.title = element_text(hjust = 0.5))

#Visualize
coef_plot







