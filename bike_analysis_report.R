#############################
# London bike demand
# 
# Date: 17 April 2020
# Author: Tom Dorrington Ward
#############################


# Import required libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")



#########################
# Import required dataset
#########################

FILENAME <- "./london_bikes/london_merged.csv"

# Import `fname` dataset and create basic features.
import_dataset <- function(filename) {
  
  df <- read_csv(filename)

  # Create labels for `season` and `weather_code` levels.
  SEASON <- c("Spring", "Summer", "Autumn", "Winter")  
  WEATHER_CODE <- c("Clear", "Few Clouds", "Broken Clouds", "Cloudy", "Rain", "Thunder", "Snowfall")

  # Convert `season` and `weather_code` to factors; extract time / date features from `timestamp`.
  df <- df %>% mutate(
    season = factor(season, labels = SEASON),
    weather_code = factor(weather_code, labels = WEATHER_CODE),
    hour = factor(hour(timestamp)),
    year = factor(year(timestamp)),
    month = factor(month(timestamp)),
    day = day(timestamp),
    wday = factor(weekdays(timestamp)),
  )
  
  df

}

fullset <- import_dataset(FILENAME)



###############################################################################
# Split journeys dataset into a training dataset and a final validation dataset
###############################################################################

# `validation` set will be final 10% of `fullset` data
VALIDATION_PROPORTION <- 0.1

# `validation` set will be final p percent of `fullset`
createTimePartition <- function(y, p) round(length(y) * (1 - p), 0):length(y)

create_train <- function(dataset, test_proportion) {
  
  # Create indices for `validation` set observations from `fullset`
  test_index <- createTimePartition(y = dataset$cnt, p = test_proportion)
  
  # Create `train` set
  dataset <- arrange(dataset, timestamp)
  train <- dataset[-test_index, ]
  temp <- dataset[test_index, ]
  
  # Make sure `hour` and `wday` in `test` set are also in `train` set
  test <- temp %>% 
    semi_join(train, by = "hour") %>%
    semi_join(train, by = "wday")
  
  # Add rows removed from validation set back into `training` set
  removed <- anti_join(temp, test)
  train <- rbind(train, removed)
  
  train
}

training <- create_train(fullset, VALIDATION_PROPORTION)
validation <- anti_join(fullset, training)



###########################
# Exploratory data analysis
###########################

# Function to calculate summary statistics
summary_stats <- function(dataset) {
  list(
    start_date = date(min(dataset$timestamp)),
    last_date = date(max(dataset$timestamp)),
    total_obs = dataset %>% count() %>% pull(),
    total_trips = dataset$cnt %>% sum(),
    total_days = dataset %>% mutate(date = date(timestamp)) %>%
      group_by(date) %>% count() %>% ungroup() %>% count() %>% pull(),
    zero_cnt = dataset %>% filter(cnt == 0) %>% count %>% pull(n)
  )
}

# Create standard summary statistics of `fullset` and `training` sets
summary_stats_full <- summary_stats(fullset)
summary_stats_train <- summary_stats(training)

# Function to create table of summary statistics
summary_table <- function(dataset) {
  dat <- dataset %>% select(cnt, t1, t2, hum, wind_speed, is_holiday, is_weekend)
  tab <- data.frame(
    Statistics = c("Minimum", "Median", "Mean", "Maximum"))
  tab %>% cbind(map_df(dat, function(v) {
    c(min(v), median(v), mean(v), max(v))
  })) %>% as_tibble()
}

# Create table of summary statistics for `training` set
summary_table_train <- summary_table(training)

# Create theme for charts
theme_parts <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_line(size = .1, color = "darkgrey"),
    panel.grid.major.y = element_line(size = .1, color = "darkgrey"),
    axis.ticks.x = element_line(color = "darkgrey"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    aspect.ratio = 1
  )

# Plot distribution of `cnt` variable
p1 <- training %>% ggplot(aes(cnt)) +
  geom_histogram(fill = rep(c("tomato1", "tomato3"), 30), bins = 60) +
  scale_x_continuous(labels = scales::label_comma(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, 2600), expand = expand_scale(add = c(0, 0.03))) +
  ylab("Number of observations, n") +
  xlab("Number of new journeys, cnt") +
  theme_parts

# Plot distribution of `cnt` variable, with log-transformation
p2 <- training %>%
  ggplot(aes(cnt)) +
  geom_histogram(fill = rep(c("tomato1", "tomato3"), 30), bins = 60) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, 2600), expand = expand_scale(add = c(0, 0.03))) +
  scale_x_log10(labels = scales::label_comma(accuracy = 1)) +
  ylab("Number of observations, n") +
  xlab("Number of new bike journeys, cnt (log10 scale)") +
  theme_parts

# Combine two histograms into one figure
journey_dist <- ggpubr::ggarrange(p1, p2, align = "v") +
  theme(plot.margin = margin(r = 10))
journey_dist <- ggpubr::annotate_figure(journey_dist, bottom = text_grob("Note: To accommodate the log10 scale, one observations with `cnt` of zero has been removed.", size = 10))
rm(p1, p2)

# Create day helper levels
workday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
breakday_levels <- c("Saturday", "Sunday", "Holiday")
facet_levels <- c(workday_levels, breakday_levels)
shortdays <- substr(facet_levels,1,3)

# Plot distribution of `cnt` variable by day of week (log-transformed)
p1 <- training %>%
  mutate(wday_holiday = factor(ifelse(is_holiday == 1 & is_weekend == 0, 8 , wday),
                        labels = c(levels(wday), "Holiday"))) %>%
  mutate(wday_holiday2 = factor(wday_holiday, levels = facet_levels)) %>%
  ggplot(aes(cnt)) +
  geom_histogram(fill = rep(c("seagreen3", "seagreen4"), 240/2)) +
  scale_x_log10(labels = scales::label_comma(accuracy = 1), breaks = c(10, 100, 1000)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, NA), expand = expand_scale(add = c(0, 0.03))) +
  theme_parts +
  theme(plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_line(color = "darkgrey")) +
  facet_wrap(. ~ wday_holiday2, ncol = 4) +
  labs(x = "Number of new bike journeys, cnt (log10 scale)",
       y = "Number of observations, n",
       caption = "Notes:\nIf an observation has an `is_holiday` value of 1, it is included in the 'Holiday' chart and excluded from\nits corresponding weekday chart.\nTo accommodate the log10 scale, one observations with `cnt` of zero has been removed.")

# Plot proportion of journeys by day
p2 <- training %>% 
  mutate(wday_holiday = factor(ifelse(is_holiday == 1 & is_weekend == 0, 8 , wday),
                               labels = c(levels(wday), "Holiday"))) %>%
  mutate(wday_holiday2 = factor(wday_holiday, levels = facet_levels)) %>%
  group_by(wday_holiday2) %>%
  summarise(total = sum(cnt)) %>%
  mutate(prop = total / sum(total)) %>%
  ggplot(aes(wday_holiday2, prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), expand = expand_scale(add = c(0, 0))) +
  scale_x_discrete(labels = shortdays) +
  theme_parts +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_line(color = "darkgrey")) +
  labs(y = "% of journeys")

# Plot average number of journeys per day
p3 <- training %>% 
  mutate(wday_holiday = factor(ifelse(is_holiday == 1 & is_weekend == 0, 8 , wday),
                               labels = c(levels(wday), "Holiday"))) %>%
  mutate(wday_holiday2 = factor(wday_holiday, levels = facet_levels)) %>%
  group_by(wday_holiday2) %>%
  summarise(total = sum(cnt), avg = mean(cnt)) %>%
  mutate(prop = total / sum(total)) %>%
  ggplot(aes(wday_holiday2, avg)) + geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), expand = expand_scale(add = c(0, 0))) +
  scale_x_discrete(labels = shortdays) +
  theme_parts +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_line(color = "darkgrey")) +
  labs(y = "Avg per hr")

# Join three plots above into one figure for report
layout <- "
AAB
AAC
"
dist_byday <- p1 + p2 + p3 + plot_layout(design = layout)
rm(p1, p2, p3, layout)

# Plot distribution of `cnt` by time of day (day or night) and day of week
dist_byday_bytime <- training %>% 
  mutate(wday_holiday = factor(ifelse(is_holiday == 1 & is_weekend == 0, 8 , wday),
                               labels = c(levels(wday), "Holiday"))) %>%
  mutate(wday_holiday2 = factor(wday_holiday, levels = facet_levels)) %>%
  mutate(
    day_split = factor(ifelse(hour %in% 6:23, "Day (6-23)", "Night (0-5)"))) %>%
  ggplot(aes(cnt, y = ..count.., fill = day_split)) + geom_density(alpha = 0.3, color = NA) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  scale_x_log10(labels = scales::label_comma(accuracy = 1), breaks = c(10, 100, 1000)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, NA), expand = expand_scale(add = c(0,0))) +
  ylab("Number of observations, n") +
  xlab("Number of new bike journeys, cnt (log10 scale)") +
  facet_wrap(wday_holiday2 ~ ., nrow = 2) +
  theme_parts +
  labs(fill = "Time of day",
       caption = "Note: To accommodate the log10 scale, one observations with `cnt` of zero has been removed.") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_line(color = "darkgrey")
  )

# Plot mean average `cnt` by hour
avgplot <- training %>%
  mutate(wday_holiday = factor(ifelse(is_holiday == 1 & is_weekend == 0, 8 , wday),
                               labels = c(levels(wday), "Holiday"))) %>%
  mutate(
    week_split = factor(ifelse(wday_holiday %in% c("Saturday", "Sunday", "Holiday"), "Non-Working Day", "Working Day")),
    hour = as.numeric(hour) - 1) %>%
  ggplot(aes(hour, cnt, col = week_split)) +
  geom_jitter(alpha = 0.01, width = 0.25) +
  scale_color_manual(values = c("tomato3", "steelblue4")) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, NA), expand = expand_scale(add = c(0,0))) +
  ylab("Number of new bike journeys, cnt") +
  xlab("Hour") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme_parts +
  labs(caption = "Note: 'Non-Working Day' includes Saturdays, Sundays and Holidays.") +
  theme(legend.title = element_blank(),
        aspect.ratio = 3 / 5,
        axis.ticks.x = element_line(color = "darkgrey"),
        plot.caption = element_text(hjust = 0))

# Plot mean `cnt` by hour for different months
winter_levels <- c("December", "January", "February")
spring_levels <- c("March", "April", "May")
summer_levels <- c("June", "July", "August")
autumn_levels <- c("September", "October", "November")
season_levels_long <- c(winter_levels, spring_levels, summer_levels, autumn_levels)
season_levels_short <- substr(season_levels_long, 1, 3)

winter_palette <- c("slategray2", "slategray3", "slategray4")
summer_palette <- c("chocolate2", "chocolate3", "chocolate4")
season_palette <- c(winter_palette, summer_palette)

monthplot <- training %>%
  mutate(wday_holiday = factor(ifelse(is_holiday == 1 & is_weekend == 0, 8 , wday),
                               labels = c(levels(wday), "Holiday"))) %>%
  mutate(
    week_split = factor(ifelse(wday_holiday %in% c("Saturday", "Sunday", "Holiday"), "Non-Working Days", "Working Days"))) %>%
  mutate(month = factor(month, levels = c(12, 1:11), labels = season_levels_short),
         hour = as.numeric(hour) - 1) %>%
  group_by(month, hour, week_split) %>%
  summarise(cnt = mean(cnt)) %>%
  filter(month %in% c("Dec", "Jan", "Feb", "Jun", "Jul", "Aug")) %>%
  ggplot(aes(hour, cnt, color = month)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, NA), expand = expand_scale(add = c(0, 0))) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(y = "Average number of new bike journeys, mean(cnt)", x = "Hour") +
  theme_parts +
  scale_color_manual(values = season_palette) +
  theme(legend.title = element_blank(),
        axis.ticks.x = element_line(color = "darkgrey")) +
  facet_grid(. ~ week_split)

# Plot `temperature `cnt` against hour for different seasons and temperatures
foo <- training %>% group_by(month, hour) %>%
  summarise(avg_t2 = mean(t2))

temphourplot <- training %>% left_join(foo, by = c("month", "hour")) %>%
  mutate(wday_holiday = factor(ifelse(is_holiday == 1 & is_weekend == 0, 8 , wday),
                               labels = c(levels(wday), "Holiday"))) %>%
  mutate(week_split = factor(ifelse(wday_holiday %in% c("Saturday", "Sunday", "Holiday"), "Non-Working Days", "Working Days"))) %>%
  mutate(t2_category = factor(ifelse(t2 > avg_t2, "High", "Low"))) %>%
  mutate(season = factor(season, labels = c("Spring", "Summer", "Autumn", "Winter"))) %>%
  mutate(hour = as.numeric(hour) - 1) %>%
  ggplot(aes(hour, cnt, color = t2_category)) +
  geom_point(alpha = 0.2) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, NA), expand = expand_scale(add = c(0, 0))) +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  labs(y = "Number of journeys, cnt", x = "Hour") +
  theme_parts +
  theme(axis.ticks.x = element_line(color = "darkgrey"),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  facet_grid(week_split ~ season) +
  scale_color_manual(values = c("#E69F00", "dodgerblue4"), name = "'Feels like' temperature") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(caption = "Note:\n'High' ('Low') is a temperature greater (less) than average for that hour in that calendar month.")
rm(foo)

# Plot comparison of temperature metrics
tempplot <- training %>%
  mutate(month = factor(month, levels = c(12, 1:11), labels = season_levels_short)) %>%
  ggplot(aes(t1, t2, color = month)) + geom_point(alpha = 0.1) + theme_parts +
  theme(panel.grid.minor.x = element_line(size = .1, color = "darkgrey"),
        panel.grid.major.x = element_line(size = .1, color = "darkgrey"),
        legend.title = element_blank()) +
  labs(x = "Real temperature, Celsius", y = "'Feels like' temperature, Celsius") +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

# Plot `cnt` against weather variables

#- Weather codes
p1 <- training %>%
  mutate(weather_code = factor(weather_code, labels = c(1, 2, 3, 4, 7, 10, 26))) %>%
  ggplot(aes(weather_code, cnt)) +
  geom_jitter(alpha = 0.05) +
  scale_y_log10(labels = scales::label_comma(accuracy = 1)) +
  theme_parts +
  labs(x = "Weather code", y = "cnt, log10 scale")

#- Humidity
p2 <- training %>%
  ggplot(aes(hum, cnt)) + 
  geom_point(alpha = 0.03) +
  scale_y_log10(labels = scales::label_comma(accuracy = 1)) +
  theme_parts + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_line(color = "darkgrey")) +
  xlab("Humidity, %")

#- Wind speed
p3 <- training %>%
  ggplot(aes(wind_speed, cnt)) + 
  geom_point(alpha = 0.03) +
  scale_y_log10(labels = scales::label_comma(accuracy = 1)) +
  theme_parts + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_line(color = "darkgrey")) +
  xlab("Wind speed, km/h")


layout <- "
ABC
"
weatherplots <- p1 + p2 + p3 + plot_layout(design = layout)
rm(p1, p2, p3, layout)



###########################################################################
# Preprocess data - change variables into formats required for the analysis
###########################################################################

# Function to create necessary features in `training` and `validation` sets
add_basic_features <- function(dataset) {
  
  data <- dataset

  # Create new `wday_holiday` feature, which distinguishes weekday holidays
  # from weekday working days - including this is based on the results of the EDA.
  data <- data %>% mutate(wday_holiday = factor(ifelse(is_holiday == 1 & is_weekend == 0, 8 , wday),
                                         labels = c(levels(wday), "Holiday")))
  
  # Remove `wday`, `is_weekend` and `is_holiday` because replaced by `wday_holiday`;
  # Remove `timestamp` because not used in analysis (have new time features);
  # Remove `season`.
  data <- data %>% select(-c(is_weekend, is_holiday, timestamp, season, day, wday))
  
  # Create less granular `weather` features.
  CLEAR <- c("Clear")
  CLOUDY <- c("Few Clouds", "Broken Clouds", "Cloudy")
  RAIN <- c("Rain", "Thunder", "Snowfall")
  data <- data %>% mutate(weather = factor(ifelse(weather_code %in% CLEAR, 1, ifelse(weather_code %in% CLOUDY, 2, 3)), labels = c("Clear","Cloud","Rain")))
  
  # Create feature for higher or lower than usual temperature in given hour, by month.
  data <- training %>%
    group_by(month, hour) %>%
    summarise(avg_t2 = mean(t2)) %>%
    right_join(data, by = c("month", "hour")) %>%
    mutate(is_high_temp = factor(t2 > avg_t2, labels = c("High","Low"))) %>%
    select(-avg_t2)
  
  # Return `data` set with new features.
  data
}

# Function to calculate group averages of log1p to reduce the number of features (dimension) needed for modelling.
# Use log1p() because `cnt` contains zero values.
calculate_mean_features <- function(dataset) {
  data <- dataset
  
  list(
    by_hour_wday_holiday = data %>% group_by(hour, wday_holiday) %>%
      summarise(cnt_hour_wkday_holiday = mean(log1p(cnt))),
    
    by_month_wday_holiday = data %>% group_by(month, wday_holiday) %>%
      summarise(cnt_month_wkday_holiday = mean(log1p(cnt))),
    
    by_hour_wday_holiday_month = data %>% group_by(hour, wday_holiday, month) %>%
      summarise(cnt_hour_wkday_holiday_month = mean(log1p(cnt))),
    
    by_hour_wday_holiday_temp =  data %>% group_by(hour, wday_holiday, is_high_temp) %>%
      summarise(cnt_wday_holiday_temp = mean(log1p(cnt))),
    
    by_hour_month_temp =  data %>% group_by(hour, month, is_high_temp) %>%
      summarise(cnt_month_temp = mean(log1p(cnt))),
    
    by_hour_wday_holiday_weather =  data %>% group_by(hour, wday_holiday, weather) %>%
      summarise(cnt_wday_holiday_weather = mean(log1p(cnt)))
  )
  
}

# Add mean features to specified dataset.
add_mean_features <- function(dataset, mean_features) {
  
  data <- with(
    mean_features,
    dataset %>%
      left_join(by_hour_wday_holiday, by = c("hour", "wday_holiday")) %>%
      left_join(by_month_wday_holiday, by = c("month", "wday_holiday")) %>%
      left_join(by_hour_wday_holiday_month, by = c("hour", "wday_holiday", "month")) %>%
      left_join(by_hour_wday_holiday_temp, by = c("hour", "wday_holiday", "is_high_temp")) %>%
      left_join(by_hour_month_temp, by = c("hour", "month", "is_high_temp")) %>%
      left_join(by_hour_wday_holiday_weather, by = c("hour", "wday_holiday", "weather")) %>%
      select(-c(weather))
  )
  data

}

# Calculate mean and standard deviation of each feature in dataset (not `cnt`).
calculate_mean_std <- function(dataset) {
  data <- dataset %>% select(-c(cnt))
  model_matrix <- model.matrix( ~ . - 1, data = data)
  preProcess(model_matrix, method = c("center", "scale"))
}

# Centre and rescale features
scale_features <- function(dataset, mean_std) {
  data <- model.matrix( ~ . - 1, data = dataset)
  data <- predict(mean_std, data)
  data <- as_tibble(data)
  data <- model.matrix( ~ . - 1, data = data)
  data <- as_tibble(data)
  data
}



##################################
# Modelling - Set up all functions
##################################

# Baseline model: OLS regression
calculate_OLS_regression <- function(train, test) {
  # Create Poisson outcome variable; use log1p() to add 1 before taking log()
  # to avoid errors from `cnt` == 0.
  y_train <- log1p(train$cnt)
  y_test <- log1p(test$cnt)
  
  train <- train %>% select(-cnt)
  # Fit model to data.
  fit <- lm(y_train ~ . - month12 - year2017, data = train)
  
  # Calculate in-sample RMSLE:
  y_hat_train <- predict(fit, train)
  y_hat_test  <- predict(fit, test)
  
  # Use rmse() function from ModelMetrics to calculate RMSLE of original `cnt` variable
  # and its prediction (equal to exp(y_hat_lm) - 1).
  rmsle_in  <- rmse(y_train, y_hat_train)
  rmsle_out <- rmse(y_test,  y_hat_test)
  
  # Return list of results:
  list(fit = fit,
       y_hat_train = y_hat_train,
       y_hat_test = y_hat_test,
       rmsle_in = rmsle_in,
       rmsle_out = rmsle_out)
}


# Calculate regularized regression
calculate_reg_regression <- function(train, test, alpha = 1) {

  # Create Poisson outcome variable; use log1p() to add 1 before taking log()
  # to avoid errors from `cnt` == 0.
  y_train <- log1p(train$cnt)
  y_test <- log1p(test$cnt)
  
  X_train <- as.matrix(train %>% select(-c(cnt, year2017)))
  X_test <- as.matrix(test %>% select(-c(cnt, year2017)))
  
  alpha <- alpha # alpha equals 0 for L2 regularization

  lambdas_to_try <- 10^seq(-10, 10, length.out = 100)
  cv <- cv.glmnet(X_train, y_train, alpha = alpha, lambda = lambdas_to_try)
  lambda <- cv$lambda.1se
  fit <- glmnet(X_train, y_train, lambda = lambda, alpha = alpha)

  # Calculate in-sample RMSLE:
  y_hat_train <- predict(fit, X_train)
  y_hat_test  <- predict(fit, X_test)
  
  # Use rmse() function from ModelMetrics to calculate RMSLE of original `cnt` variable
  # and its prediction (equal to exp(y_hat_lm) - 1).
  rmsle_in  <- rmse(y_train, y_hat_train)
  rmsle_out <- rmse(y_test,  y_hat_test)
  
  # Return list of results:
  list(cv = cv,
       fit = fit,
       y_hat_train = y_hat_train,
       y_hat_test = y_hat_test,
       rmsle_in = rmsle_in,
       rmsle_out = rmsle_out)
}



##############
# Train models
##############

# Split `training` into a `internal_train` and `internal_test`, where
# `internal_train` is a subset of `training` for training and
# `internal_test` is a subset for model testing / selection.
# `internal_test` is the final 10% of the `training` dataset.
internal_train <- create_train(training, VALIDATION_PROPORTION)
internal_test  <- anti_join(training, internal_train)

# Add new features to datasets
internal_train <- add_basic_features(internal_train)
internal_test  <- add_basic_features(internal_test)
# Calculate `mean_features` for use with both `internal_train` and `internal_test` sets.
mean_features_internal_train <- calculate_mean_features(internal_train)
# Add new group means features to `internal_train` and `internal_test` datasets.
internal_train <- add_mean_features(internal_train, mean_features_internal_train)
internal_test  <- add_mean_features(internal_test, mean_features_internal_train)
# Calculate mean and standard deviation for `internal_train` set
mean_std_internal_train <- calculate_mean_std(internal_train)
# Scale `internal_train` and `internal_test` sets
internal_train <- scale_features(internal_train, mean_std_internal_train)
internal_test  <- scale_features(internal_test, mean_std_internal_train)

# Run OLS regression model on `internal_train` and test on `internal_test`.
results_lm <- calculate_OLS_regression(internal_train, internal_test)  
rmsle_lm <- results_lm$rmsle_out

# Run L2 Regularized regression model on `internal_train` and test on `internal_test`.
set.seed(1984)
results_L2 <- calculate_reg_regression(internal_train, internal_test, alpha = 0)
rmsle_L2 <- results_L2$rmsle_out

# Run L1 Regularized regression model on `internal_train` and test on `internal_test`.
set.seed(1985)
results_L1 <- calculate_reg_regression(internal_train, internal_test, alpha = 1)
rmsle_L1 <- results_L1$rmsle_out



##########################################
# Fit final model to full training dataset
##########################################

# Add new features to datasets
training    <- add_basic_features(training)
validation  <- add_basic_features(validation)
# Calculate `mean_features` for use with both `training` and `validation` sets.
mean_features_training <- calculate_mean_features(training)
# Add new group means features to `training` and `validation` datasets.
training    <- add_mean_features(training,   mean_features_training)
validation  <- add_mean_features(validation, mean_features_training)
# Calculate mean and standard deviation for `training` set
mean_std_training <- calculate_mean_std(training)
# Scale `training` and `validation` sets
training    <- scale_features(training, mean_std_training)
validation  <- scale_features(validation, mean_std_training)


y_training   <- log1p(training$cnt)
y_validation <- log1p(validation$cnt)

if (which.min(c(rmsle_lm, rmsle_L2, rmsle_L1)) == 1) {
  cat("Fit OLS Linear Model\n")
  fitted_model <- lm(y_train ~ . - month12 - cnt, data = training)
  y_hat_training   <- predict(fitted_model, training)
  y_hat_validation <- predict(fitted_model, training)

} else if (which.min(c(rmsle_lm, rmsle_L2, rmsle_L1)) == 2) {
  cat("Fit L2 Regularized Model")
  alpha <- 0
  best_lambda <- results_L2$cv$lambda.1se
  X_training <- as.matrix(training %>% select(-cnt))
  fitted_model <- glmnet(X_training, y_training, lambda = best_lambda, alpha = alpha)
  y_hat_training   <- predict(fitted_model, X_training)
  X_validation <- as.matrix(validation %>% select(-cnt))
  y_hat_validation <- predict(fitted_model, X_validation)
  
} else if (which.min(c(rmsle_lm, rmsle_L2, rmsle_L1)) == 3){
  cat("Fit L1 Regularized Model")
  alpha <- 1
  best_lambda <- results_L1$cv$lambda.1se
  X_training <- as.matrix(training %>% select(-cnt))
  fitted_model <- glmnet(X_training, y_train, lambda = best_lambda, alpha = alpha)
  y_hat_training   <- predict(fitted_model, X_training)
  X_validation <- as.matrix(validation %>% select(-cnt))
  y_hat_validation <- predict(fitted_model, X_validation)
  
}


###################################
# Calculate accuracy of final model
###################################

rmse_validation <- rmse(y_validation, y_hat_validation)
cat("Final RMSLE result:\n", rmse_validation)


