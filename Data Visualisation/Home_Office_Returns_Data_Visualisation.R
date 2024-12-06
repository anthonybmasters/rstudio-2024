## Packages and Themes
# Install the packages and set the theme
library(tidyverse)
library(pdftools)
library(curl)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)
library(scales)
library(sysfonts)
library(showtext)

font_add_google("Spline Sans")
showtext_auto()

theme_clean <- theme_bw(base_family = "Spline Sans") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 20, face = "italic",
                                     margin = margin(b=12)),
        plot.caption = element_text(size = 20,
                                    vjust = -1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title.position = "plot",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 20, face = "bold"))
theme_set(theme_clean)

## Import the data table from the UK Government website
home_office_returns_url <- "https://assets.publishing.service.gov.uk/media/672dc3ed0207c4664564ce5b/returns-datasets-sep-24.xlsx"
home_office_returns_sheet <- "Data - Ret_D02"
home_office_returns_range <- "A2:G45732"

temp <- tempfile()
temp <- curl::curl_download(
  url = home_office_returns_url, destfile = temp,
  quiet = TRUE, mode = "wb")

home_office_returns_df <- read_excel(temp,
                                      sheet = home_office_returns_sheet,
                                      range = home_office_returns_range) %>%
  janitor::clean_names() %>%
  dplyr::group_by(return_type_group, quarter) %>%
  dplyr::summarise(total_returns = sum(number_of_returns)) %>%
  dplyr::mutate(quarter_date = lubridate::yq(quarter) %m+% months(2))

home_office_r12m_df <- home_office_returns_df %>%
  dplyr::select(quarter_date, return_type_group, total_returns) %>%
  dplyr::group_by(return_type_group) %>%
  dplyr::mutate(
    rolling_12m_total_returns = total_returns +
      dplyr::lag(total_returns, n = 1, default = NA_real_) +
      dplyr::lag(total_returns, n = 2, default = NA_real_) +
      dplyr::lag(total_returns, n = 3, default = NA_real_)
  ) %>%
  dplyr::filter(quarter_date >= as_date("2011-09-01"))

## Graph
# Working out the placement of the axis marks
date_axis_min <- min(home_office_r12m_df$quarter_date, na.rm = TRUE)
date_axis_max <- max(home_office_r12m_df$quarter_date, na.rm = TRUE)
date_axis_labs <- seq(date_axis_min, date_axis_max, by = "3 months") %>%
  dplyr::as_tibble() %>%
  dplyr::rename(minor_breaks = value) %>%
  dplyr::mutate(breaks = case_when(month(minor_breaks) == 9 ~ minor_breaks,
                                   TRUE ~ NA_Date_))

# This tibble sets where the direct labels are
home_office_r12m_labels <- tribble(
  ~return_type_group, ~quarter_date, ~rolling_12m_total_returns,
  "Enforced return", as_date("2011-11-01"), 12000,
  "Voluntary return", as_date("2011-11-01"), 28000,
  "Refused entry at port and subsequently departed", as_date("2011-11-01"), 20000)

# These are the labels
home_office_r12m_title <- "Returns from the United Kingdom are rising after the Covid-19 pandemic."
home_office_r12m_subtitle <- "Rolling 12-month totals of returns from the United Kingdom by type, from the year ending September 2011 to the year ending September 2024. This is a count of returns within the 12-month period, not individuals."
home_office_r12m_caption <- "Source: Home Office Ret_D02 dataset, September 2024."

home_office_r12m_gg <- home_office_r12m_df %>%
  ggplot(aes(x = quarter_date, y = rolling_12m_total_returns,
             group = return_type_group, colour = return_type_group)) +
  geom_line(linewidth = 2) +
  scale_x_date(expand = c(0,0),
               breaks = date_axis_labs$breaks,
               date_labels = "%b\n%Y") +
  scale_y_continuous(limits = c(0, 35000),
                     breaks = seq(0, 35000, 5000),
                     labels = scales::label_comma()) +
  geom_text(data = home_office_r12m_labels,
            aes(x = quarter_date, y = rolling_12m_total_returns,
                label = return_type_group, colour = return_type_group),
            size = 7, fontface = "bold", hjust = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#008080", "#fe8c11", "#0941B3")) +
  annotate(geom = "rect", xmin = date_axis_max %m+% months(-24), xmax = date_axis_max,
           ymin = 0, ymax = 35000, alpha = 0.2, fill = "beige") +
  annotate(geom = "text", x = date_axis_max %m+% months(-23), y = 34000,
           label = "The latest eight\nquarters are\nprovisional",
           fontface = "bold", size = 7, hjust = 0, vjust = 1) +
  labs(title = home_office_r12m_title,
       subtitle = str_wrap(home_office_r12m_subtitle, width = 140),
       caption = home_office_r12m_caption,
       x = "Year ending",
       y = "Rolling 12-month number of returns")

## Saving the outputs
png("R/Analysis_2024/05_Data_Visualisation/home_office_r12m_gg.png",
    width = 2000, height = 1200, unit = "px")
home_office_r12m_gg
dev.off()