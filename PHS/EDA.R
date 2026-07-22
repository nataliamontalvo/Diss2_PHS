library(ggplot2)
library(dplyr)
library(tidyr)
library(INLA)
library(mgcv)
library(lubridate)

# Load main data sets
month_demo_data <- read.csv("opendata_monthly_ae_demographics_202604.csv")
month_activity_data <- read.csv("monthly_ae_activity_202605.csv")
week_activity_data <- read.csv("weekly_ae_activity_20260628.csv")

# Look at data sets

summarise_columns <- function(data) {
  
  # Blank and : as NA
  is_missing <- function(x) {
    if (is.character(x)) {
      is.na(x) | trimws(x) %in% c("", ":")
    } else {
      is.na(x)
    }
  }
  
  data %>%
    summarise(
      across(everything(),
             list(
               type        = ~ class(.)[1],
               n_rows      = ~ length(.),
               n_missing   = ~ sum(is_missing(.)),
               pct_missing = ~ round(mean(is_missing(.)) * 100, 2),
               n_unique    = ~ n_distinct(.[!is_missing(.)]),
               options     = ~ paste(sort(unique(as.character(.[!is_missing(.)]))),
                                     collapse = ", ")
             ),
             .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(everything(),
                 names_to = c("variable", ".value"),
                 names_sep = "__")
}


# Monthly demographics
summarise_columns(month_demo_data)

month_demo_data <- month_demo_data %>%
  mutate(m_date = ym(as.character(Month)),
         year   = format(m_date, "%Y"),
         m_num  = as.integer(format(m_date, "%m")))

month_trend_data <- month_demo_data %>%
  group_by(year, m_num) %>%
  summarise(total = sum(NumberOfAttendances, na.rm = TRUE), .groups = "drop")

m_demo_trend_plt <- ggplot(month_trend_data, aes(x = m_num, y = total, 
                                                 color = year, group = year)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Total Attendances",
       title = "A&E Attendances by Month, Compared Across Years") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

m_demo_trend_plt

ggsave("figures/m_demo_trend_by_year.pdf", plot = m_demo_trend_plt,
       width = 8, height = 5)






summarise_columns(month_activity_data)
summarise_columns(week_activity_data)

