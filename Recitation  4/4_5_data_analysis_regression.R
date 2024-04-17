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

needed.packages <- c("foreign","ggplot2","rstudioapi","tidyverse","dplyr","stargazer")  

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
# SIMULATION NO 2: EFFECT OF UNEMPLOYMENT ON ATTITUDES AGAINST GLOBALIZATION
#==========================================================================================
# We will assume our outcome is preferences against migration!

#==========================================================================================
# FIRST LET US CREATE THE SIMULATION

n_interviewed = 100000 # number of individuals surveyed in our data

republican  = sample(c(0,1),n_interviewed,replace=TRUE) # Create random variable: 1 if individual is republican, 0 if not

education   = rbinom(n_interviewed,16,0.4) # Create random variable with the number of years of education

# Data generating process for lobby: effect of being republican + effect of education + random component

unemployed  = 0.6+0.1*republican - 0.17*education + sample(c(0,1),n_interviewed,replace=TRUE) 
unemployed = round(unemployed)

# Data generating process for tariffs: effect of lobby - effect of exporter + random component

# Index for anti-migration attitudes. Note education as a confounder and republican as a confounder and moderator

anti_migrant = 0.01 + 0.5*unemployed +0.1*unemployed*republican 
  + 0.05*republican - 0.8*education   + rnorm(n_interviewed,mean=0,sd=1)

# In our notation beta_1 is 0.1 and beta_3 is 0.05, these are the true treatment effects

# Finally let us create a data base with this data

DATA <- data.frame(anti_migrant,unemployed,education,republican)

# Let's add some labels to the data

# We will use tidyverse for the sake of making easy/clean code. The tidy verse uses an element
# called pipping or %>% , this symbol passes a command into another command. So for instance if
# if you have a function g() and another one f(), g %>% f is the same as f(g()) .
# Check a video: https://www.youtube.com/watch?v=ui3VZeuN8QY&ab_channel=DataDaft


# And then save it:
write.csv(DATA,"Simulation.csv")

#==========================================================================================
# SECOND LET US ANALYZE OUR DATA

rm(list= ls()) # Clean the workspace

data_frame = read.csv("Simulation.csv") # Load the data

# Let us check at the distribution of tariffs by lobbying status:

p<-ggplot(data_frame, aes(x=anti_migrant, fill=unemployed)) +
  geom_density(size=1, alpha = 0.5) + 
  geom_vline(aes(xintercept=mean(anti_migrant[unemployed == 0])), color="red", linetype="dashed", size=1.5) +
  geom_vline(aes(xintercept=mean(anti_migrant[unemployed == 1])),  color="blue", linetype="dashed", size=1.5) +
  labs(title="Distribution of anti-migration attitudes", x="Index of anti-migration", y="Density") +
  theme_classic() + 
  theme(legend.position='top', plot.title = element_text(hjust = 0.5))

p <- p + theme(axis.text = element_text(size = 15),
               axis.title = element_text(size = 25),
               plot.title = element_text(size=22),
               legend.text=element_text(size=15))

# Visualize
p


#==========================================================================================
# THIRD DO SOME HYPOTHESIS TESTING

# First we do not control for confounders
reg_no_1 = lm(anti_migrant ~ unemployed, data = data_frame)

# Second we do control for confounders
reg_no_2 = lm(anti_migrant ~ unemployed +   republican + education, data = data_frame)

# Next we include the interaction effect but no confounders
reg_no_3 = lm(anti_migrant ~ unemployed +  unemployed:republican + republican + education , data = data_frame)

# Visualizing a beautiful table:

stargazer(reg_no_1,reg_no_2,reg_no_3,type="text")







