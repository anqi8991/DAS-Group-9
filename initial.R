# Load required packages
library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(broom)
#read data
films <- read.csv("dataset9.csv", header = TRUE)
# View data structures and variable types
str(films)
# View variable distribution
summary(films)
# Correlation matrix
cor(films[,c("year", "length", "budget", "votes", "rating")])
# Check for missing values
sum(is.na(films))
# Check which column has missing values
colSums(is.na(films))
# ########Fill missing values (any other way?)
mean_length <- mean(films$length, na.rm = TRUE)
films$length[is.na(films$length)] <- mean_length
# Check if missing values were filled successfully
sum(is.na(films))
# Draw a histogram (show the number of movies with different ratings, and the color distinguishes the fuzzy interval with a score of about 7)
ggplot(films, aes(x = rating, fill = factor(rating > 7))) +
  geom_histogram(alpha = 0.5, binwidth = 0.5) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), 
                    name = "Rating > 7", 
                    labels = c("No", "Yes")) +
  labs(title = "Distribution of IMDB ratings", x = "IMDB rating", y = "Frequency") +
  theme_classic()
# Draw a scatter plot (show the relationship between budget, ratingg and genre)
ggplot(films, aes(x = budget, y = rating)) +
  geom_point(aes(size = votes, color = genre)) +
  scale_color_brewer(palette = "Set2", name = "Genre") +
  labs(title = "Relationship between budget, rating and genre", x = "Budget", y = "IMDB rating") +
  theme_classic()
# Draw a line chart by year (the mean and median of each year are drawn in the red and blue lines)
ggplot(films, aes(x = year, y = rating, group = 1)) +
  stat_summary(fun = mean, geom = "line", color = "red", size = 1) +
  stat_summary(fun = median, geom = "line", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(1930, 2020, by = 10), 
                     labels = seq(1930, 2020, by = 10)) +
  labs(title = "Trend of IMDB rating", x = "Year", y = "IMDB rating") +
  theme_classic()
# Convert year and length to continuous variables(Because time and length can be regarded as unlimited values)
films$year <- as.numeric(as.character(films$year))
films$length <- as.numeric(as.character(films$length))
# one-hot encoding
films <- cbind(films, model.matrix(~genre-1, data=films))
# Binarize the IMDB score
films$rating2 <- ifelse(films$rating > 7, 1, 0)
# Fitting a GLM model
fit <- glm(rating2 ~ year + length + budget + votes + genreAction + genreAnimation+genreComedy + genreDocumentary + genreDrama + genreRomance, data=films, family=binomial)
summary(fit)
# Extract Coefficients and Standard Errors
coef_df <- tidy(fit, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate_if(is.numeric, list(~ round(., 2))) %>%
  mutate(lower = estimate - 1.96 * std.error,
         upper = estimate + 1.96 * std.error)

#According to the results, the following conclusions can be drawn:
#Year (year) has a coefficient of 1, indicating that there is no significant difference in the impact of the year on the score.
#The coefficient of film length (length) is 0.94, and the confidence interval does not include 1, indicating that the film length has a negative impact on the score, that is, the longer the film length, the lower the score.
#The coefficient of budget (budget) is 1.68, and the confidence interval does not include 1, indicating that the budget has a positive impact on the score, that is, the higher the budget, the higher the score.
#The coefficient of the number of votes (votes) is 1, indicating that the impact of the number of votes on the score is not significantly different.
#The coefficients of different types of movies are quite different, and the coefficients of action movies (genreAction) and cartoons (genreAnimation) are relatively small, indicating that these two types of movies have little impact on ratings; while the coefficients of documentaries (genreDocumentary) are compared Large, indicating that documentaries have a greater impact on ratings.

# plot the coefficients
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(title = "Effect of predictors on movie rating",
       x = "", y = "Odds Ratio") +
  theme_minimal()
