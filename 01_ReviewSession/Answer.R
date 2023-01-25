#=====================================================================
# Question 1
# Unzip and read the csv file "food_stamp_2022_2021.csv"
#=====================================================================

df <- read.csv("food_stamp_2022_2021.csv")

#=====================================================================
# Question 2
# Get the summary statistics of Household income, Number of Foodstamps
# and number of foodstamps
#=====================================================================

summary(df$HHINCOME)
summary(df$STAMPNO)
summary(df$STAMPMO)

#=====================================================================
# Question 3
# Get the Histogram of Household income, Number of Foodstamps
# and number of foodstamps
#=====================================================================

hist(df$HHINCOME)
hist(df$STAMPNO)
hist(df$STAMPMO)

#=====================================================================
# Question 4
# Verify typeof YEAR and subset by year
#=====================================================================

typeof(df$YEAR)

df_2021 <- df[df$YEAR == 2021,]
df_2022 <- df[df$YEAR == 2022,]


#=====================================================================
# Question 5
# Plot scatterplots between Household income, Number of Foodstamps
# and number of foodstamps
#=====================================================================

plot(df$HHINCOME, df$STAMPNO)
plot(df$HHINCOME, df$STAMPMO)
plot(df$STAMPNO, df$STAMPMO)

#=====================================================================
# Question 6
# Calculate the 'effective stamp' received which is defined as
# number of months on food stamp multiplied by the number of 
# receipients. Plot the relationship
#=====================================================================

df$STAMPEFF <- df$STAMPMO * df$STAMPNO

plot(df$HHINCOME, df$STAMPEFF)


#=====================================================================
# Question 7
# Write a function to for a two-sampled t-test, assuming that
# there is no difference between the years, returning the p-value
#=====================================================================


t_test <- function(sample_x, sample_y) {
  
  #=====================================================================
  # Question 7(a)
  # Calculate the number of samples
  #=====================================================================
  
  n_x <- length(sample_x) 
  n_y <- length(sample_y)
  
  #=====================================================================
  # Question 7(b)
  # Calculate the mean of samples
  #=====================================================================
  
  mean_x <- mean(sample_x)
  mean_y <- mean(sample_y)
  
  #=====================================================================
  # Question 7(c)
  # Calculate the standard deviation of samples
  #=====================================================================
  
  sd_x <- sd(sample_x)
  sd_y <- sd(sample_y)
  
  #=====================================================================
  # Question 7(d)
  # Calculate the degrees of freedom, standard deviation of the population,
  # the t-statistic and the p-value
  #===================================================================== 
  
  deg_freedom <- n_x + n_y - 2
  
  sd_p <- ((n_x - 1)*sd_x^2 + (n_y - 1)*sd_y^2)/deg_freedom
  
  t_stat <- (mean_x - mean_y)/sqrt(sd_p*((1/n_x + 1/n_y)))
  
  if(t_stat > 0) {
    lower_tail = F
  } else {
    lower_tail = T
  }
  
  p_val <- 2*pt(q=t_stat, df=deg_freedom, lower.tail = lower_tail)

  return(p_val)
}

#=====================================================================
# Question 8
# Calculate the p-value using t_test (your function) and 
# t.test (R's base function) to see if the number of receipients of 
# foodstamps and the duration of receipt are statistically different
#===================================================================== 

t_test(df_2021$STAMPNO, df_2022$STAMPNO)
t.test(df_2021$STAMPNO, df_2022$STAMPNO)

t_test(df_2021$STAMPMO, df_2022$STAMPMO)
t.test(df_2021$STAMPMO, df_2022$STAMPMO)

#=====================================================================
# Question 9
# What can you say about the observation?
#===================================================================== 



