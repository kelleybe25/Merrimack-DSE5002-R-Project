###title: "DSE5002 R Project"
###author: "Ben Kelley"
###date: "Nov 26, 2023"
###output: pdf_document


library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
######################import and cleaning#######################
################################################################

## read in csv file, remove non-full-time entries to make data relevant
r_project_data <- read.csv("r_project_data.csv")

full_time_data1 <- r_project_data %>%
  select(work_year, experience_level, employment_type, job_title, 
         salary_in_usd, employee_residence, remote_ratio, 
         company_location,  company_size) %>%
  filter(employment_type == "FT") 


## further filter to remove L company size and EN-MI experience level
full_time_data2 <- r_project_data %>%
  select(work_year, experience_level, employment_type, job_title, 
         salary_in_usd, employee_residence, remote_ratio, 
         company_location,  company_size) %>%
  filter(employment_type == "FT", experience_level != "EN", 
         experience_level != "MI", company_size != "L") 

#######################analysis#################################
################################################################

## calculating IQR for all FT salaries by company size and 
## experience level
salary_summary1 <- full_time_data1 %>%
  group_by(experience_level, company_size) %>% 
  summarize(med = median(salary_in_usd),
            Q1 = quantile(salary_in_usd, 0.25),
            Q3 = quantile(salary_in_usd, 0.75))


## calculating IQR for FT salaries by S-M company size and 
## SE-EX experience level
salary_summary2 <- full_time_data2 %>%
  group_by(experience_level, company_size) %>% 
  summarize(med = median(salary_in_usd),
            Q1 = quantile(salary_in_usd, 0.25),
            Q3 = quantile(salary_in_usd, 0.75))

## using group_by and pivot_longer to separate US workers 
## from other countries
location_summary1 <- full_time_data1 %>%
  group_by(employment_type) %>%
  summarize(domestic = mean(salary_in_usd[employee_residence == "US"]),
            offshore = mean(salary_in_usd[employee_residence != "US"]))

location_summary2 <- location_summary1 %>%
  pivot_longer(cols = c("domestic", "offshore"),
               names_to = "location",
               values_to = "salary_in_usd")

###########################visuals##############################
################################################################

## create plot of all salary for visual overview
ggplot(salary_summary1, aes(x = company_size, 
                            color = experience_level)) +
  geom_pointrange(aes(y = med,
                      ymin = Q1,
                      ymax = Q3),
                  show.legend = TRUE) +
  labs(x = "Company Size",y = "Salary in USD",
       title = "Salary Range by Company Size and Experience") +
  theme_light()

## create plot from salary_summary for most relevant data visual
ggplot(salary_summary2, aes(x = company_size, 
                            color = experience_level)) +
  geom_pointrange(aes(y = med,
                      ymin = Q1,
                      ymax = Q3),
                  show.legend = TRUE) +
  labs(x = "Company Size", y = "Salary in USD",
       title = "Salary Range for S-M Company and SE-EX Employee Experience") +
  theme_light()

## create bar plot to compare US positions to those offshore
ggplot(location_summary2) +
  geom_col(aes(x = location, fill = location,
               y= salary_in_usd),
           show.legend = TRUE) +
  labs(x = "Employee Location",
       y = "Average Salary in USD",
       title = "Domestic vs Offshore Salaries")


