library(dplyr)
library(ggplot2)

# 1. Load full dataset
air_full <- read.csv("Airline_Delay_Cause.csv")

# Quick check
str(air_full)
colnames(air_full)

# 2. Select & rename variables 
air <- air_full %>%
  select(year,
         month,
         carrier,
         airport      = airport,
         arr_flights  = arr_flights,
         arr_del15    = arr_del15,
         carrier_delay,
         weather_delay,
         nas_delay,
         security_delay,
         late_aircraft_delay) %>%
  rename(
    airport_code       = airport,
    total_flights      = arr_flights,
    delayed_flights    = arr_del15,
    delay_carrier      = carrier_delay,
    delay_weather      = weather_delay,
    delay_nas          = nas_delay,
    delay_security     = security_delay,
    delay_late_aircraft= late_aircraft_delay
  )

# 3. Remove missing values in key columns
air <- na.omit(air)

# 4. Create total delay minutes
air$total_delay_minutes <- air$delay_carrier +
  air$delay_weather +
  air$delay_nas +
  air$delay_security +
  air$delay_late_aircraft

# 5. Create season from month
air$season <- ifelse(air$month %in% c(12, 1, 2), "Winter",
                     ifelse(air$month %in% c(3, 4, 5), "Spring",
                            ifelse(air$month %in% c(6, 7, 8), "Summer", "Fall")))

# 6. Create airport_group (High vs Low traffic)
airport_totals <- air %>%
  group_by(airport_code) %>%
  summarise(total = sum(total_flights), .groups = "drop")

med_total <- median(airport_totals$total)

airport_totals <- airport_totals %>%
  mutate(airport_group = ifelse(total > med_total, "High", "Low"))

air <- air %>%
  left_join(airport_totals[, c("airport_code", "airport_group")],
            by = "airport_code")

# 7. Create delay_flag for logistic regression
air$delay_flag <- ifelse(air$delayed_flights > 0, 1, 0)

# 8. Convert to factors
air <- air %>%
  mutate(
    carrier       = factor(carrier),
    airport_code  = factor(airport_code),
    season        = factor(season, levels = c("Winter","Spring","Summer","Fall")),
    airport_group = factor(airport_group),
    delay_flag    = factor(delay_flag)
  )

# 9. Sample 1000 rows for project requirement
set.seed(123)
air <- dplyr::sample_n(air, 1000)

# Final check
dim(air)
head(air)

# Histogram of Total Delay Minutes
ggplot(air, aes(x = total_delay_minutes)) +
  geom_histogram(bins = 30, color = "black", fill = "lightblue") +
  labs(title = "Histogram of Total Delay Minutes",
       x = "Total delay minutes (per airline-airport-month)",
       y = "Count")
# Average Total Delay Minutes by Airline (Top Carriers)
air_carrier_mean <- air %>%
  group_by(carrier) %>%
  summarise(mean_delay = mean(total_delay_minutes), .groups = "drop") %>%
  arrange(desc(mean_delay)) %>%
  slice(1:10)

ggplot(air_carrier_mean, aes(x = reorder(carrier, mean_delay), y = mean_delay)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Total Delay Minutes by Carrier (Top 10)",
       x = "Carrier",
# Weather Delay Minutes by Season
ggplot(air, aes(x = season, y = delay_weather)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Weather Delay Minutes by Season",
       x = "Season",
       y = "Weather delay minutes")
       y = "Average total delay minutes")



##
## Questions
##

# Question 1: Two-sample t-test (Airlines)
airlineA <- "DL"  
airlineB <- "UA"  

air_q1 <- air %>%
  filter(carrier %in% c(airlineA, airlineB)) %>%
  droplevels()

ggplot(air_q1, aes(x = carrier, y = total_delay_minutes)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Total Delay Minutes by Carrier",
       x = "Carrier",
       y = "Total delay minutes")

t_q1 <- t.test(total_delay_minutes ~ carrier, data = air_q1)
t_q1

# Question 2: One-way ANOVA (Seasons)
# Boxplot
ggplot(air, aes(x = season, y = delay_weather)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Weather Delay Minutes by Season",
       x = "Season",
       y = "Weather delay minutes")

# One-way ANOVA
anova_q2 <- aov(delay_weather ~ season, data = air)
summary(anova_q2)

# Optional: Tukey post-hoc test
TukeyHSD(anova_q2)


# Question 3: ANCOVA
# total_delay_minutes ~ airport_group + total_flights
ggplot(air, aes(x = total_flights, y = total_delay_minutes,
                color = airport_group)) +
  geom_point(alpha = 0.6) +
  labs(title = "Total Delay vs Flights by Airport Group",
       x = "Total flights (per month)",
       y = "Total delay minutes",
       color = "Airport group")

# Fit ANCOVA model
ancova_q3 <- aov(total_delay_minutes ~ airport_group + total_flights, data = air)
summary(ancova_q3)

# Question 4: Logistic Regression (Single Predictor)
logit_q4 <- glm(delay_flag ~ delay_weather,
                data = air,
                family = binomial)
summary(logit_q4)

exp(coef(logit_q4))

# Question 5: Multiple Logistic Regression
logit_q5 <- glm(delay_flag ~ season + airport_group + carrier +
                  delay_weather + delay_late_aircraft +
                  delay_carrier + delay_nas,
                data = air,
                family = binomial)

summary(logit_q5)

or_q5 <- exp(coef(logit_q5))
or_q5

or_df <- data.frame(
  term = names(or_q5),
  OR   = as.numeric(or_q5)
)

or_df <- or_df[or_df$term != "(Intercept)", ] 


logit_q5 <- glm(delay_flag ~ season + airport_group + carrier +
                  delay_weather + delay_late_aircraft +
                  delay_carrier + delay_nas,
                data = air,
                family = binomial)

summary(logit_q5)
exp(coef(logit_q5))