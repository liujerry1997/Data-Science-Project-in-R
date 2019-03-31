library(tidyverse)
library(stringr)
library(forcats)

clean <- all_uisc_clean_colName[c(-1, -7, -9, -15, -17, -18)]

clean %>% filter(!is.na(ugrad_gpa) & ugrad_gpa < 4.0) %>% 
  ggplot(aes(ugrad_gpa)) + geom_histogram(bins = 40) + labs(titles = "GPA Distribution")

# comment
The gpas for applicants is similar to a normal distribution skewed to the left. That implies that applicants with a higher gpa are more likely to pursue further education.   



clean %>% filter(!is.na(status)) %>% 
  mutate(count = n(), prop = count / sum(count)) %>% 
  ggplot(aes(x = status)) + geom_bar() + 
  labs(titles = "Immigration Status") 


clean %>% filter(!is.na(ugrad_gpa|gre_verbal|gre_quant)& ugrad_gpa < 4 & is_new_gre == TRUE) %>%
  mutate(GRE_Total = gre_verbal + gre_quant) %>%
  group_by(uni_name) %>% mutate(mean_gpa = mean(ugrad_gpa), mean_GRE = mean(GRE_Total)) %>%
  ungroup() %>%
  ggplot(aes(x = mean_GRE, y = mean_gpa)) + geom_point(aes(color = "RED", alpha = 0.001)) +
  labs(titles = "Relationship between GPA and GRE Score", y = "GPA", x = "GRE Score")
