#==========================================================================================
# INTRODUCTION TO DATA ANALYSIS AND REGRESSION
#==========================================================================================

#==========================================================================================
# BASICS OF R: LOAD THE NECESSARY PACKAGES

needed.packages <- c("foreign","ggplot2","rstudioapi","tidyverse","dplyr","stargazer",
                     "vtable","broom")  

# This command installs all packages listed in needed.packages and their dependencies.

# install.packages(needed.packages, dependencies=TRUE)  

# This command will tell R we will use all packages listed in needed.packages

sapply(needed.packages, require, character.only = TRUE)

#==========================================================================================
# SET THE WORKING DIRECTORY: THE PLACE IN THE COMPUTER WHERE WE WILL WORK (I.E., THE PATH)

setwd("D:/Documents/Dropbox/NYU/Dissertation - phase/Things on the side/Political Economy Honors (STERN)/Slides/Recitation 5 - Tables and Graphs")

# I like using this shortcut which tells R that the folder where my R-file is defines the path:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd() # Check it
#==========================================================================================
# CLEAN THE WORK SPACE AND SET A SEED

rm(list= ls())  # This deletes everything that is the environment.

set.seed(12345) # Setting a seed is important so that you can replicate things if a command
                # uses random number generation.

#==========================================================================================
# LET US ANALYZE OUR DATA

# The data is a random 10% sub-sample of selected variables from a project that I'm working
# on with IHDS data.

data = read.dta("exercise_class.dta") # Load the data

# Our main outcome is the variable income_salary and we will analyze what is the impact of
# social capital on income (i.e., that is all variables with the prefix membership_). Social 
# capital is often understood as having access to a community that can help you out, provide
# you with opportunities as well as a safety net. 

# The variables with the prefix hh_ are observable characteristics of the household head.
# These characteristics will be our confounders. 

# First lets look for missing data

colSums(is.na(data)) 

# There very few missing observations so we can get rid of them without damaging our sample.

data <- na.omit(data)
# Check again:
colSums(is.na(data))

# If there were too many missing you have to be very careful because they will bias your
# estimates due to selection bias.

#==========================================================================================
# BAR GRAPHS WITH STANDARD ERRORS

# Means by group, in this case we will look at a club (which will transform to  a factor variable) 
data$membership_club =  as.factor(data$membership_club)

# First we create  data frame that has the values of the averages for each group
summary_stats <- data %>% group_by(membership_club) %>% summarise(
  mean = mean(income_salary, na.rm = TRUE), 
  var = var(income_salary, na.rm = TRUE),
  sd = sd(income_salary, na.rm = TRUE),
  p50 =  median(income_salary, na.rm = TRUE),
  count = n()
)

# Next we use ggplot to create an object that will contain our bar graph

bar_graph <- ggplot(summary_stats, aes(x=membership_club, y=mean)) + # Specify what do you want to plot
  geom_bar(position=position_dodge(), fill=c("black","gray"), stat="identity") +
  geom_errorbar(aes(ymin=mean-1.96*sd/sqrt(count), ymax=mean+1.96*sd/sqrt(count)), # Standard errors
                width=.2,                    # width of the error bars
                position=position_dodge(.9)) +
  scale_fill_hue(name="", labels=c("No", "Yes")) +  # Add legends to your bars
  labs(title="Average salary by club membership status", x="Group membership", y="Average salary (Rupees)") +
  theme_classic()  + # Classic theme makes the background white
  theme(legend.position='top', plot.title = element_text(hjust = 0.5)) # Centers the title of the graph
  
# You can keep modifying the graph because we have saved it as an object
bar_graph <- bar_graph + theme(plot.title = element_text(size=22),
               legend.text=element_text(size=15)) 
# Visualize
bar_graph

# Save your graph to use it later!
ggsave(bar_graph, file="means_by_.png", device="png")

# For further information you can check R's cookbook:
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/ 

#==========================================================================================
# KERNEL DENSITY DISTRIBUTIONS

# The distribution of wages tends to be quite skewed so me need to make a logarithmic 
# transformation. Further information here: https://www.youtube.com/watch?v=aaN_atlM1kM

data$log_salary = log(data$income_salary+1)

cdf<-ggplot(data, aes(x=log_salary, linetype=membership_club)) +
  geom_density(size=1, alpha = 0.5) + 
  scale_linetype(name="Member of a club:", labels=c("No", "Yes")) +
  labs(title="Distribution of wages", x="log(wage)", y="Density") +
  theme_classic()  +
  theme(legend.position='top', plot.title = element_text(hjust = 0.5))

# Visualize
cdf

# Save

ggsave(cdf, file="wages_distribution_by_group.png", device="png")

# More information about plotting distributions here: 
# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/

#==========================================================================================
# CORRELATIONS

# Let us construct an index variable to facilitate looking at the correlations
data_2 <- data
groups<-c("membership_religious","membership_mandal","membership_club",
             "membership_trade","membership_selfhelp","membership_savings",
             "membership_agriculture")
data_2[,groups] <- lapply(data[,groups] , function(x) ifelse(x == '(1) Yes', 1, 0))
data_2$index<-rowMeans(data_2[,groups])

correlation<- ggplot(data_2, aes(x = index, y = log_salary)) +
  geom_point(colour = "grey60") +
  stat_smooth(method = lm, level = 0.99, colour = "black") +
  labs(title="Index of group membership vs. salary", x="Index of social capital", y="Salary") +
  theme_classic()  +
  theme(legend.position='top', plot.title = element_text(hjust = 0.5))
  
# Visualize
correlation

# Save

ggsave(p, file="correlation.png", device="png")


#==========================================================================================
# BREAK: Adding names to columns
colnames(data) <- c("Member of religious group","Age of household head","Gender of household head",
                    "Area of residence","Religion of household head","Salary","Poor status",
                    "Member of mandal","Member of club","Member of trade association",
                    "Member of self-help group",
                    "Member of savings association","Member of agricultural association")

#==========================================================================================
# DESCRIPTIVE STATISTICS

# Easiest but more standard:
stargazer(data_2, type = "text", title="Descriptive statistics", digits=1, out="table1.html")

# We can restrict ourselves to one group:
stargazer(data_2[which(data_2$membership_club==0), ], type = "text", title="Descriptive statistics", digits=1, out="table1.html")

# We can even be very specific:
stargazer(data_2[which(data_2$membership_club==0), ], type = "text", title="Descriptive statistics", digits=1, out="table1.html",
           summary.stat = c("n", "p75", "sd"))

# We can use sumtable as well (which is usually the option I like the most):
st(data_2, group = 'membership_club', group.long = TRUE,file='table2.html',
   summ=c('mean(x)',
          'median(x)',
          'min(x)',
          'max(x)'))


# We can build it by hand if we need more complexity:
summary_stats_A <- data_2 %>% group_by(membership_club) %>% summarise(
  mean = mean(income_salary, na.rm = TRUE), 
  var = var(income_salary, na.rm = TRUE),
  sd = sd(income_salary, na.rm = TRUE),
  p50 =  median(income_salary, na.rm = TRUE),
  count = n()
)
summary_stats_B <- data_2 %>% group_by(membership_club) %>% summarise(
  mean = mean(head_age, na.rm = TRUE), 
  var = var(head_age, na.rm = TRUE),
  sd = sd(head_age, na.rm = TRUE),
  p50 =  median(head_age, na.rm = TRUE),
  count = n()
)
# You can possible use R functions to speed this up, but that is outside of the scope of this class.
summary_table<-rbind(data.frame(id = "income", summary_stats_A),
                     data.frame(id = "age", summary_stats_B))

write.csv(summary_stats,"summary_table.csv")


# There are actually many packages and ways to create summary statistics. I have just
# shown you some extremely easy ones. Some potential options are listed below:
# http://www.danieldsjoberg.com/gtsummary/
# https://stats.idre.ucla.edu/r/faq/how-can-i-get-a-table-of-basic-descriptive-statistics-for-my-variables/
# https://www.jakeruss.com/cheatsheets/stargazer/
# https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html

#==========================================================================================
# REGRESSION TABLES

# First we do not control for confounders
reg_no_1 = lm(log_salary ~ index, data = data_2)

# Second we do control for confounders
reg_no_2 = lm(log_salary ~ index + head_age + head_sex + hh_urban + factor(hh_religion), data = data_2)

# Next we include the interaction effect but no confounders
reg_no_3 = lm(log_salary ~ index +  index:head_age + head_age + head_sex + hh_urban +
                factor(hh_religion), data = data_2)

# Visualizing a beautiful table:

stargazer(reg_no_1,reg_no_2,reg_no_3,type="text",out="table3.html")

# Restricting the output and adding labels

stargazer(reg_no_1,reg_no_2,reg_no_3, keep = c("index","index:head_age"),
          title            = "Impact of social capital index on wages",
          covariate.labels = c("Index of soc. capital", "Index of soc. capital x Age of hh."),
          dep.var.labels  = "Dependent variable: log(salary)",
          type="text",out="table4.html",style = "qje")


# There is more information that you can glean here:
# https://www.princeton.edu/~otorres/NiceOutputR.pdf
# https://www.jakeruss.com/cheatsheets/stargazer/
# Also, you can take a look at the help file:
?stargazer

#==========================================================================================
# REGRESSION PLOTS
tidy_reg1<-tidy(reg_no_1) %>% filter(term=='index')
tidy_reg2<-tidy(reg_no_2) %>% filter(term=='index')
coefficients<-rbind(data.frame(model = "No controls", tidy_reg2),
                     data.frame(model = "Controls", tidy_reg1))

# Ordering the models (R doesn't understand what's the right order, and assumes alphabetical)
coefficients$model <- factor(coefficients$model, levels = c("No controls", "Controls"))

coef_plot<-ggplot(coefficients, aes(x=model, y=estimate))+
  geom_point()+
  geom_pointrange(aes(ymin = estimate-1.96*std.error, ymax =  estimate+1.96*std.error))+
  geom_hline(yintercept=0, color = "red") +
  labs(title = "Coefficients of a linear regression model")  +
  labs(title="Impact of social capital index on wages", x="Model", y="Coefficient size") +
  theme_classic()  +
  theme(legend.position='top', plot.title = element_text(hjust = 0.5))

#Visualize
coef_plot

# Saving
ggsave(p, file="correlation.png", device="png")






