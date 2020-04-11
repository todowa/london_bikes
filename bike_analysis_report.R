#############################
# London bike demand
# 
# Date: 10 April 2020
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
if(!require(patchwork)) install.packages("scales", repos = "http://cran.us.r-project.org")



#########################
# Import required dataset
#########################

# Import dataset
fullset <- read_csv("./london_bikes/london_merged.csv")



###########################################################################
# Preprocess data - change variables into formats required for the analysis
###########################################################################

fullset <- fullset %>% mutate(month = month(timestamp),
                              year = year(timestamp),
                              day_of_week = weekdays(timestamp),
                              hour = hour(timestamp))



###############################################################################
# Split journeys dataset into a training dataset and a final validation dataset
###############################################################################

# `validation` set will be final 10% of `fullset` data
VALIDATION_PROPORTION <- 0.1

# `validation` set will be final p percent of 
createTimePartition <- function(y, p) {
  round(length(y) * (1 - p), 0):length(y)
}

# Create indices for `validation` set observations from `fullset`
validation_index <- createTimePartition(y = fullset$cnt, p = VALIDATION_PROPORTION)

# Create `training` set
fullset <- fullset %>% arrange(timestamp)
training <- fullset[-validation_index,]
temp <- fullset[validation_index,]

# Make sure `hour` and `day_of_week` in `validation` set are also in `training` set
validation <- temp %>% 
  semi_join(training, by = "hour") %>%
  semi_join(training, by = "day_of_week")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
training <- rbind(training, removed)

rm(validation_index, temp, removed)



###########################
# Exploratory data analysis
###########################

# Check for missing values in `fullset`


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
  mutate(day_of_week2 = as.factor(ifelse(is_holiday == 1, "Holiday", day_of_week))) %>%
  mutate(day_of_week3 = factor(day_of_week2, levels = facet_levels)) %>%
  ggplot(aes(cnt)) +
  geom_histogram(fill = rep(c("seagreen3", "seagreen4"), 240/2)) +
  scale_x_log10(labels = scales::label_comma(accuracy = 1), breaks = c(1, 100, 1000)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, NA), expand = expand_scale(add = c(0, 0.03))) +
  theme_parts +
  theme(plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_line(color = "darkgrey")) +
  facet_wrap(. ~ day_of_week3, ncol = 4) +
  labs(x = "Number of new bike journeys, cnt (log10 scale)",
       y = "Number of observations, n",
       caption = "Notes:\nIf an observation has an `is_holiday` value of 1, it is included in the 'Holiday' chart and excluded from\nits corresponding weekday chart.\nTo accommodate the log10 scale, one observations with `cnt` of zero has been removed.")

# Plot proportion of journeys by day
p2 <- training %>% 
  mutate(day_of_week2 = as.factor(ifelse(is_holiday == 1, "Holiday", day_of_week))) %>%
  mutate(day_of_week3 = factor(day_of_week2, levels = facet_levels)) %>%
  group_by(day_of_week3) %>%
  summarise(total = sum(cnt)) %>%
  mutate(prop = total / sum(total)) %>%
  ggplot(aes(day_of_week3, prop)) +
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
  mutate(day_of_week2 = as.factor(ifelse(is_holiday == 1, "Holiday", day_of_week))) %>%
  mutate(day_of_week3 = factor(day_of_week2, levels = facet_levels)) %>%
  group_by(day_of_week3) %>%
  summarise(total = sum(cnt), avg = mean(cnt)) %>%
  mutate(prop = total / sum(total)) %>%
  ggplot(aes(day_of_week3, avg)) + geom_bar(stat = "identity") +
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
  mutate(day_of_week2 = as.factor(ifelse(is_holiday == 1, "Holiday", day_of_week))) %>%
  mutate(day_of_week3 = factor(day_of_week2, levels = facet_levels)) %>%
  mutate(
    day_split = factor(ifelse(hour %in% 6:23, "Day (6-23)", "Night (0-5)"))) %>%
  ggplot(aes(cnt, y = ..count.., fill = day_split)) + geom_density(alpha = 0.3, color = NA) +
  scale_fill_manual(values = c("#E69F00", "#999999")) +
  scale_x_log10(labels = scales::label_comma(accuracy = 1), breaks = c(1, 100, 1000)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1)) +
  ylab("Number of observations, n") +
  xlab("Number of new bike journeys, cnt (log10 scale)") +
  facet_wrap(day_of_week3 ~ ., nrow = 2) +
  theme_parts +
  labs(fill = "Time of day",
       caption = "Note: To accommodate the log10 scale, one observations with `cnt` of zero has been removed.") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_line(color = "darkgrey")
  )

# Plot mean average `cnt` by hour
avgplot <- training %>%
  mutate(day_of_week2 = as.factor(ifelse(is_holiday == 1, "Holiday", day_of_week))) %>%
  mutate(
    week_split = factor(ifelse(day_of_week2 %in% c("Saturday", "Sunday", "Holiday"), "Non-Working Day", "Working Day"))) %>%
  ggplot(aes(hour, cnt, col = week_split)) +
  geom_jitter(alpha = 0.01, width = 0.25) +
  scale_color_manual(values = c("tomato3", "steelblue4")) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1)) +
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
  mutate(day_of_week2 = as.factor(ifelse(is_holiday == 1, "Holiday", day_of_week))) %>%
  mutate(
    week_split = factor(ifelse(day_of_week2 %in% c("Saturday", "Sunday", "Holiday"), "Non-Working Days", "Working Days"))) %>%
  mutate(month = factor(month, levels = c(12, 1:11), labels = season_levels_short)) %>%
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





scale_color_manual(values = c("tomato3", "steelblue4")) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1)) +
  ylab("Number of new bike journeys, cnt") +
  xlab("Hour") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme_parts +
  labs(caption = "Note: 'Non-Working Day' includes Saturdays, Sundays and Holidays.") +
  theme(legend.title = element_blank(),
        aspect.ratio = 3 / 5,
        axis.ticks.x = element_line(color = "darkgrey"),
        plot.caption = element_text(hjust = 0))


training %>% 





  

filter(day_of_week3 %in% workday_levels) %>%
scale_color_manual(values = c("orange1", "orangered1", "springgreen3", "plum3", "royalblue1")) +
  

  geom_density(aes(hour, y = cnt), stat = "identity")


training %>%
  mutate(log_cnt = log(cnt + 1)) %>%
  mutate(day_of_week2 = as.factor(ifelse(is_holiday == 1, "Holiday", day_of_week))) %>%
  mutate(day_of_week3 = factor(day_of_week2, levels = facet_levels)) %>%
  filter(day_of_week3 == "Thursday") %>%
  ggplot(aes(timestamp, log_cnt, color = hour)) + geom_point()



training %>% mutate(log_cnt = log(cnt)) %>% 
  mutate(day_of_week2 = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%ggplot(aes(log_cnt)) +
  scale_x_continuous(labels = scales::label_comma(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), expand = expand_scale(0, 0.03)) +
  geom_histogram(fill = "#E69F00", color = "black") +
  ylab("Number of observations, n") +
  xlab("Number of new journeys, cnt") +
  theme_parts

# Distribution of trips across hours, by day of week
training %>% mutate(day_of_week = as.factor(day_of_week)) %>%
  group_by(hour, day_of_week) %>%
  summarise(cnt = sum(cnt)) %>%
  ggplot(aes(hour, cnt, color = day_of_week)) + geom_line(stat = "identity")
  facet_grid(day_of_week ~ .)

                    training %>% mutate(day_of_week = as.factor(day_of_week),
                    log_cnt = log(cnt)) %>%
  group_by(hour, day_of_week) %>%
  summarise(cnt = sum(cnt)) %>%
  ggplot(aes(hour)) +
  geom_bar() +
  facet_grid(day_of_week ~ .)

training %>% mutate(log_cnt = log(cnt),
                    is_weekend = as.factor(is_weekend)) %>%
  ggplot(aes(log_cnt, fill = is_weekend)) +
  geom_density_ridges()
  
  scale_x_continuous(labels = scales::label_comma(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), expand = expand_scale(0, 0.03)) +
  geom_histogram(fill = "#E69F00", color = "black") +
  ylab("Number of observations, n") +
  xlab("Number of new journeys, cnt") +
  theme_parts




training %>% filter(is_weekend == 1) %>%
  mutate(log_cnt = log(cnt)) %>%
  ggplot(aes(log_cnt)) +
  scale_x_continuous(labels = scales::label_comma(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), expand = expand_scale(0, 0.03)) +
  geom_histogram(fill = "#E69F00", color = "black") +
  ylab("Number of observations, n") +
  xlab("Number of new journeys, cnt") +
  theme_parts


###########################################################################
# Preprocess data - change variables into formats required for the analysis
###########################################################################

fullset <- fullset %>% mutate(month = month(timestamp),
                              year = year(timestamp),
                              day_of_week = as.factor(weekdays(timestamp)),
                              hour = hour(timestamp),
                              is_holiday = as.factor(is_holiday),
                              is_weekend = as.factor(is_weekend),
                              weather_code = as.factor(weather_code))

fullset %>% head()

# Create `year_season` variable.
# If winter and in last few months of the year, replace `year_season`
# with year of previous calendar year.
SPRING_START_MONTH <- 3
season_dates <- dat %>% group_by(date) %>% summarise(season = first(season)) %>%
  mutate(season_start_year = paste0(ifelse(season == 4 & month(date) < 6,
                                           year(date)-1,
                                           year(date)))) %>%
  mutate(year_season = paste0(season_start_year, "_", season)) %>%
  mutate(season_start_month = season * SPRING_START_MONTH) %>%
  mutate(season_start_date = make_date(season_start_year, season_start_month, 1)) %>%
  mutate(season_day = as.numeric(date) - as.numeric(season_start_date) + 1) %>%
  select(date, season, season_start_year, year_season, season_day)




##################################
# Modelling - Set up all functions
##################################




##############
# Train models
##############



##########################################
# Fit final model to full training dataset
##########################################



###################################
# Calculate accuracy of final model
###################################




# Inspect data set
dat %>% summary() %>% knitr::kable()
dat %>% head()

# Create date and hour of day features
dat <- dat %>% mutate(hour = hour(timestamp),
                      date = date(timestamp))

# Inspect season feature
dat <- dat %>% mutate(season = season + 1)





# Plot count by day of season 
dat %>% group_by(date) %>%
  summarise(count = sum(cnt)) %>%
  left_join(season_dates, by = "date") %>%
  mutate(year_season = as.factor(year_season),
         season = as.factor(season)) %>%
  ggplot(aes(season_day, count, color = season_start_year)) +
  geom_line() +
  facet_grid(season~.)

# Plot count by week of season
dat %>% mutate(week = week(date),
               year = as.factor(year(date))) %>%
  group_by(week, year) %>%
  summarise(count = sum(cnt)) %>%
  filter(year != 2017) %>%
  ggplot(aes(week, count, color = year)) +
  geom_line(size = 1.2)

dat %>% mutate(month = as.integer(month(date)),
               year = as.factor(year(date))) %>%
  group_by(month, year) %>%
  summarise(count = sum(cnt)) %>%
  ungroup() %>%
  ggplot(aes(month, count, color = year)) +
  geom_line(size = 1.2)

dat %>%
  group_by(year_season) %>%
  summarise(season_first = first(date),
            season_last = last(date)) %>%
  arrange(season_first)

dat %>%
  mutate(year = year(date)) %>%
  group_by(is_weekend, year) %>%
  summarise(cnt = sum(cnt)) %>%
  ggplot(aes(cnt, year, color = is_weekend)) +
  geom_bar()

dat
# Daily summary dataset
dat_daily <- dat %>% mutate(date = date(timestamp),
               yearday = yday(date)) %>%
  group_by(date) %>%
  summarise(count = sum(cnt),
            yearday = first(yearday), season = first(season))


start_date <- min(date(dat$timestamp))
end_date <- max(date(dat$timestamp))

date_period <- data.frame(tsday = 1, start = start_date, end = end_date) %>%
  nest(start, end) %>%
  mutate(data = map(data, ~seq(unique(.x$start), unique(.x$end), 1))) %>%
  unnest(data) %>%
  rename(date = data) %>%
  mutate(tsday = row_number())
  


start_date
end_date

data.frame(start_date = max(date(dat$timestamp), end_date =)

dat_daily

dat_daily %>% 
  mutate(year_season = paste0(year(date), "_", season)) %>%
  group_by(year_season) %>%
  summarise(season_first = first(date),
            season_last = last(date))
  
dat %>% arrange(date)  

  summarise(season_first )

  
# Plot journey count
dat %>% mutate(date = date(timestamp)) %>%
  group_by(date) %>%
  summarise(count = sum(cnt)) %>%
  ggplot(aes(date, count)) + geom_line()

# Plot journey count by season
dat %>% mutate(date = date(timestamp),
               season = as.factor(season),
               yearday = yday(date)) %>%
  group_by(date) %>%
  summarise(count = sum(cnt), yearday = first(yearday), season = first(season)) %>%
  ggplot(aes(yearday, count, color = season)) + geom_line()


dat %>% group_by(season) %>% 
  mutate(date = date(timestamp), yearday = yday(date))
  summarise(season_first = first(season), season_last = )
