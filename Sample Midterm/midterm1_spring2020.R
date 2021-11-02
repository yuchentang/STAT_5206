# Q1 In R, is the variable defined below, q1, a vector, yes/no?
q1 <- "hello"

#A: YES. 

# Q2 Please write the code that creates 3 vectors:
#   - a vector with the values 1 through 13, where each value is repeated 4 times,
#     assigned to a variable called "cards". The ordering does not matter.
#   - a vector with the values 1 through 6, assigned to a variable called "dice"
#   - a vector called "rolls" that represent 50 random dice rolls
#   - a vector called "draws" that represent 50 cards drawn without replacement, randomly
#   - a vector called "total" where each element is the sum of the
#     corresponding element in "rolls" and "draws"
cards <- rep(1:13, 4)
dice <- 1:6
rolls <- sample(dice, 50, replace = TRUE)
draws <- sample(cards, 50)
total <- rolls + draws

# Q3 What can cause the following error message in R:
#   - "Error: object 'q3' not found"
#     Code is not necessary, a short explanation is sufficient

#A: The variable 'q3' is not defined correctly! 

# Q4 Please give an example code that will contain
#    the following error message:
#    - "non-numeric argument to binary operator"
#    Hint: binary operators are functions with 2 inputs
chr <- "hello"
print(chr + 1)


# Q5 Please write a function named "cln_dat" that
#    - takens in a numeric vector, "x"
#    - if there are more than or exactly 100 data points in x
#      - computes the 2nd and 98th percentile of x (hint: quantile() )
#      - creates a boolean vector named "outlier" that has TRUE for any value
#        in "x" that is less than the 2nd or larger than the 98th percentile
#        (if equal to the percentiles, these values are not considered outliers)
#    - otherwise,
#      - computes the mean of "x"
#      - computes the SD of "x"
#      - creates a boolean vector named "outlier" that has TRUE for any value
#        in "x" that is more than 2.05375 standard deviations away from the mean (in either direction)
#    - prints out a statement that states how many data points were removed, hint: print()
#    - returns all the values in "x" that do NOT correspond to outliers.
cln_dat <- function(x){
  if(length(x) >= 100){
    quan_0.02 <- quantile(x, 0.02)
    quan_0.98 <- quantile(x, 0.98)
    outlier <- x < quan_0.02 | x > quan_0.98
  }
  else{
    mean_x <- mean(x)
    sd_x <- sd(x)
    outlier <- (x > mean_x + 2.05375 * sd_x) | (x < mean_x - 2.05375 * sd_x)
  }
  print(sum(outlier))
  return(x[outlier != TRUE])
}

# Q6 
# - Please create 100 samples drawn from an exponential distribution with rate = 0.05
#   and name the output "q6_data"
# - Please apply the function from Q5 "q6_data" and assign the output to a variable named "q6_cln"
# - Please print out the length of "q6_cln"
q6_data <- rexp(100, rate = 0.05)
q6_cln <- cln_dat(q6_data)
print(length(q6_cln))

# Q7
# It's the flu season! We can think of getting sick as a function of
#   long exposure to the flu virus.
# - Please simulate 365 Bernoulli random variables, 
#   each with p=0.3 of showing 1. Let's name this vector "exposed".
# - Please write the code that loops over "exposed",
#   if you see 5 consecutive 1's, please print out the message "got sick"
#   note: if you have 4 consecutive 1's then a 0, you should restart
#   the count.
#   hint: starting the loop with a variable called "days_exposed"
#   might help.
# - No code, please articulate how you would show through simulations,
#   if we increase p, then the chances of getting sick will increase?
exposes <- rbinom(365, 1, prob = 0.3)
days_exposed <- 0
for (i in exposes){
  days_exposed <- days_exposed + i
  if (days_exposed == 5){
    print("got sick")
    break
  }
  if (i == 0) {
    days_exposed <- 0
  }
}

# Q8 
# Job descriptions often list out qualities and skills that
# employers hope to see in candidates. Students can use this information
# to know if their classes are useful or not. A basic text mining technique
# simply counts the frequency of key words for each job description.
# The "mid1_bow.csv" file has columns that correspond to the frequency
# of certain key words where each row corresponds to a different
# job description.
# Note: An NA value is produced when a job description does not have the key word.
# This data is "real", but non-tech jobs are included.
# Please show CODE for all of the following:
# - What are the key words in "mid1_bow.csv"? They are in the column names.
library(tidyverse)
jobs <- read_csv("mid1_bow.csv")
keywords <- names(jobs)
# - What is the maximum number of times "r" is mentioned in a single job description?
jobs[is.na(jobs)] <- 0
max_r_mentioned <- max(jobs$r)
# - Is "python" or "r" mentioned more frequently across these job descriptions? Please
#   make a decision whether multiple mentions in a single job description should be counted more
#   for a student to understand what to study.

#Ans: Multiple mentions should be counted as once because they just represent requirements for one job!
#What's more, multiple mentions can be because the descriptions are redundant, which is meaningless
#to count more. And the answer is 'NO'. "python" or "r" are not mentioned more frequently. 
jobs_temp <- jobs
jobs_temp[jobs_temp > 1] <- 1
map_dbl(jobs_temp, sum)
# - What percentage of job descriptions mention "statist" (this word comes from unifying
#   words like statistics, statistician, statistical, etc)?
perc_stat <- sum(jobs_temp$statist) / length(jobs_temp$statist)
# - What percentage of job descriptions mention both "r" AND "python"?
perc_r_py <- sum((jobs$r != 0) & (jobs$python != 0)) / nrow(jobs)
# - Given a job mentions "statist", is it more likely to have "python" listed or "r" listed as a skill?

#It is morely to have "python".
jobs_statist <- filter(jobs, statist > 0)
print(sum(jobs_statist$python))
print(sum(jobs_statist$r))
# - Given a job mentions "r", what is the chance a job description also mentions "statist"
jobs_r <- filter(jobs, r > 0)
sum(jobs_r$statist > 0) / nrow(jobs_r)
# - Given a job mentions "python", what is the chance a job description also mentions "statist"
jobs_python <- filter(jobs, python > 0)
sum(jobs_python$statist > 0) / nrow(jobs_python)
# - NO CODE, given the answers above, should a statistics student study "R" or "Python" to maximize
#   their chance of employment?

