library(faraway)
library(tibble)
library(stats)
library(tidyverse)
library(ggplot2)

data("sat")
df<-sat[complete.cases(sat),]

## Question 1
ggplot(df, aes(x = salary, y = total)) +
  geom_point() +
  labs(x = "Salary ($1,000)", y = "Average SAT score",
       title ="Relationship between salary and total SAT score",
       subtitle = "No other variable consider in this scatterplot",
       caption = "source: faraway package\nauthor: thinkingondata.com") +
  theme_minimal()


ggplot(df, aes(x = salary, y = total, color = df$takers)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Salary ($1,000)", y = "Average SAT score",
       title ="Relationship between salary and total SAT score",
       subtitle = "Less students taking SAT, higher scores for those whom taking the test.",
       caption = "source: faraway package\nauthor: thinkingondata.com") + 
  scale_colour_viridis_c()

ggplot(df, aes(x = salary, y = total, color = df$expend)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Salary ($1,000)", y = "Average SAT score",
       title ="Scatterplot between salary and total SAT score",
       subtitle = "No relation between more expend and better scores.",
       caption = "source: faraway package\nauthor: thinkingondata.com") + 
  scale_colour_viridis_c()

## Question 2: Organize the data in clusters
clusters <-   kmeans(df %>% select(salary, total, takers), centers = 3)
SAT  <-  df %>%
  mutate(cluster = factor(clusters$cluster))

SAT <-  SAT %>%
  mutate(frac_cat = cut(takers, breaks = c(0, 22, 49, 81),
                        labels = c("low", "medium", "high")))
ggplot(SAT, aes(x = salary, y = total, color = frac_cat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Salary ($1,000)", y = "Average SAT score",
       title ="Groups per salary and total SAT",
       subtitle = "Percentage of eligible students by colors",  
       caption = "source: faraway package\nauthor: thinkingondata.com") +
  theme_minimal() +
  scale_color_viridis_d()

## Question 3: Correlation 

round(cor(df),2)
# expend ratio salary takers verbal  math total
# expend   1.00 -0.37   0.87   0.59  -0.41 -0.35 -0.38
# ratio   -0.37  1.00   0.00  -0.21   0.06  0.10  0.08
# salary   0.87  0.00   1.00   0.62  -0.48 -0.40 -0.44
# takers   0.59 -0.21   0.62   1.00  -0.89 -0.87 -0.89
# verbal  -0.41  0.06  -0.48  -0.89   1.00  0.97  0.99
# math    -0.35  0.10  -0.40  -0.87   0.97  1.00  0.99
# total   -0.38  0.08  -0.44  -0.89   0.99  0.99  1.00





