###reading data
library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(skimr)
library(ggplot2)
library(moderndive)
library(infer)
library(tidymodels)
umps<- read_csv(".csv")

eval_umps <- umps |>
  select(pitches_called, incorrect_calls, correct_calls, accuracy, consistency, favor_home, total_run_impact)

eval_umps |>
  summarise(mean_pitches_called = mean(pitches_called, na.rm = TRUE), 
            mean_incorrect_calls = mean(incorrect_calls, na.rm = TRUE), 
            mean_correct_calls = mean(correct_calls, na.rm = TRUE), 
            mean_accuracy = mean(accuracy, na.rm = TRUE), 
            mean_consistency = mean(consistency, na.rm = TRUE), 
            mean_favor_home = mean(favor_home, na.rm = TRUE), 
            mean_total_run_impact = mean(total_run_impact, na.rm = TRUE), 
            median_pitches_called = median(pitches_called, na.rm = TRUE), 
            median_incorrect_calls = median(incorrect_calls, na.rm = TRUE), 
            median_correct_calls = median(correct_calls, na.rm = TRUE), 
            median_accuracy = median(accuracy, na.rm = TRUE), 
            median_consistency = median(consistency, na.rm = TRUE), 
            median_favor_home = median(favor_home, na.rm = TRUE), 
            median_total_run_impact = median(total_run_impact, na.rm = TRUE))



###Calculating Summary statistic
eval_umps |>
  select(pitches_called, incorrect_calls, correct_calls, accuracy, consistency, favor_home, total_run_impact) |>
  skim_without_charts()


###Visualizing datalibrary(readr)
ggplot(eval_umps, aes(consistency, total_run_impact)) +
  geom_point() +
  labs(x = "Umpire Consistency", y = "Total Run Impact on Game", title = "Consistency Impact on Game") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(eval_umps, aes(accuracy, total_run_impact)) +
  geom_point() +
  labs(x = "Umpire Accuracy", y = "Total Run Impact on Game", title = "Accuracy Impact on Game") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(eval_umps, aes(consistency, favor_home)) +
  geom_point() +
  labs(x = "Umpire Consistency", y = "Runs Favoring Home Team", title = "Consistency Favoring Home Team") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(eval_umps, aes(accuracy, favor_home)) +
  geom_point() +
  labs(x = "Umpire Accuracy", y = "Runs Favoring Home Team", title = "Accuracy Favoring Home Team") +
  geom_smooth(method = "lm", se = FALSE)

eval_umps |>
  ggplot(aes(x = accuracy, y = total_run_impact)) +
  geom_boxplot() +
  labs(x = "Umpire Accuracy", y = "Total Run Impact on Game", title = "Accuracy Impact on Game")

eval_umps |>
  ggplot(aes(x = consistency, y = total_run_impact)) +
  geom_boxplot() +
  labs(x = "Umpire Consistency", y = "Total Run Impact on Game", title = "Consistency Impact on Game")

eval_umps |>
  ggplot(aes(x = accuracy, y = favor_home)) +
  geom_boxplot() +
  labs(x = "Umpire Accuracy", y = "Favoring Home Team", title = "Accuracy Favoring Home Team")

eval_umps |>
  ggplot(aes(x = consistency, y = favor_home)) +
  geom_boxplot() +
  labs(x = "Umpire Consistency", y = "Favoring Home Team", title = "Consistency Favoring Home Team")


###linear regression model
total_run_model <- lm(total_run_impact ~ accuracy + consistency, data = eval_umps)
get_regression_table(total_run_model)

favor_home_model <- lm(favor_home ~  accuracy + consistency, data = eval_umps)
get_regression_table(favor_home_model)


#Checking for normality
eval_umps |>
  ggplot(aes(x = accuracy)) +
  geom_histogram(binwidth = 0.5) +
  labs(title ="Histogram of Accuracy")

eval_umps |>
  ggplot(aes(x = consistency)) +
  geom_histogram(binwidth = 0.5) +
  labs(title ="Histogram of Consistency")

t_test<- t.test(eval_umps$accuracy)
t_test

t_test2<- t.test(eval_umps$consistency)
t_test2


###-We can say with 95% confidence that consistency will fall between the interval [93.14, 93.2] and that its p-value is less than 0.05 making it significant

###-We can say with 95% confidence that accuracy will fall between the interval [92.38, 92.46] and that its p-value is less than 0.05 making it significant

###Bootstrapping distributions
x_bar<- eval_umps |>
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE), mean_consistency = mean(consistency, na.rm = TRUE))
x_bar

set.seed(1)
ump_sample <- eval_umps |>
  rep_sample_n(size = 50, replace = TRUE, reps = 35)

ump_sample |>
  summarise(resample_mean = mean(accuracy, na.rm = TRUE), resample_mean2 =  mean(consistency, na.rm = TRUE))

ggplot(ump_sample, aes(x = accuracy))+
  geom_histogram(binwidth = 0.5, color = "white", boundary = 91) +
  labs(y= "Total Run Impact", title = "Bootstrap Histogram of Accuracy")
  

ggplot(ump_sample, aes(x = consistency))+
  geom_histogram(binwidth = 0.5, color = "white", boundary = 91) +
  labs(y= "Total Run Impact", title = "Bootstrap Histogram of Consistency")

ump_sample |>
  specify(response = accuracy) |>
  generate(reps = 1000, type = "bootstrap")

bootstrap_dist <- ump_sample |>
  specify(response = accuracy) |>
  generate(reps = 1000) |>
  calculate(stat = "mean")
bootstrap_dist

percentile_ci <- bootstrap_dist |>
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci

visualize(bootstrap_dist) +
  shade_confidence_interval(endpoints = percentile_ci) +
  labs(x = "Umpire Rating", title = "Bootstrap Distribution of Accuracy")

ump_sample |>
  specify(response = consistency) |>
  generate(reps = 1000, type = "bootstrap")

bootstrap_dist <- ump_sample |>
  specify(response = consistency) |>
  generate(reps = 1000) |>
  calculate(stat = "mean")
bootstrap_dist

percentile_ci <- bootstrap_dist |>
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci

visualize(bootstrap_dist) +
  shade_confidence_interval(endpoints = percentile_ci) +
  labs(x = "Umpire Rating", title = "Bootstrap Distribution of Consistency")
