## Packages and Themes
# Install the packages and set the theme
library(tidyverse)
library(curl)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(patchwork)
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

## Import the data table from the Office for National Statistics website
ons_url <- "https://www.ons.gov.uk/file?uri=/businessindustryandtrade/retailindustry/datasets/retailsalesindexinternetsales/current/internetreferencetables.xlsx"
ons_sheet <- "Internet"
ons_range <- "A4:D222"

temp <- tempfile()
temp <- curl::curl_download(
  url = ons_url, destfile = temp,
  quiet = TRUE, mode = "wb")

ons_retail_df <- read_excel(temp,
                          sheet = ons_sheet,
                          range = ons_range) %>%
  dplyr::slice(-1) %>%
  janitor::clean_names() %>%
  dplyr::mutate_at(.vars = 2:4,
                   .funs = as.numeric) %>%
  dplyr::rename(
    awv_all_retail_sales_million = 2,
    awv_internet_sales_million = 3,
    internet_sales_proportion = 4) %>%
  dplyr::mutate(awv_non_internet_retail_million =
                  awv_all_retail_sales_million - awv_internet_sales_million,
                internet_sales_pct = internet_sales_proportion/100,
                month_date = lubridate::ym(time_period)) %>%
  dplyr::select(month_date,
                awv_internet_sales_million,
                awv_non_internet_retail_million,
                internet_sales_pct)

# The tidy version is for the stacked bar graph
ons_tidy_df <- ons_retail_df %>%
  dplyr::select(1:3) %>%
  tidyr::pivot_longer(cols = 2:3,
                      names_to = "measure",
                      values_to = "value")

# This point is for the line graph
ons_min_date <- min(ons_retail_df$month_date)
ons_max_date <- max(ons_retail_df$month_date)
ons_date_breaks <- seq(ons_min_date, ons_max_date,
                       by = "2 years") %>%
  dplyr::as_tibble()
ons_maxdate_df <- ons_retail_df %>%
  dplyr::filter(month_date == ons_max_date)

## Graphs
# We set up labels and other aspects of the graphs
colour_palette <- c("#008080", "#fe8c11")
retail_labels <- tribble(
  ~measure, ~label, ~month_date, ~value,
  "awv_internet_sales_million", "Internet retail sales", as_date("2007-01-01"), 10000,
  "awv_non_internet_retail_million", "All other retail sales", as_date("2007-01-01"), 9000)
ons_retail_title <- "In the UK lockdowns, the internet share of retail sales in Great Britain ratcheted up above one in four pounds."
ons_retail_subtitle <- paste0("Average weekly retail sales value (in £ million) in Great Britain by internet and non-internet purchase, and the internet share of total retail sales (rounded to one decimal place). Figures are not seasonally adjusted, ",
                              format(ons_min_date, "%B %Y"), " to ", format(ons_max_date, "%B %Y"), ".")
ons_retail_caption <- paste0("Source: Office for National Statistics: Retail Sales Index internet sales dataset, ",
                             format(ons_max_date, "%B %Y"), ".")

# The lock-down dates are taken from this page:
# https://commonslibrary.parliament.uk/research-briefings/cbp-9068/
uk_lockdown_start_dates <- c(as_date("2020-03-23"), as_date("2020-11-05"), as_date("2021-01-06"))
uk_lockdown_end_dates <- c(as_date("2020-06-23"), as_date("2020-12-02"), as_date("2021-03-08"))

# The first graph shows average weekly retail sales by type
ons_retail_gg1 <- ons_tidy_df %>%
  ggplot(aes(x = month_date, y = value,
             group = measure, fill = measure)) +
  geom_col() +
  scale_colour_manual(guide = "none",
                      aesthetics = c("colour", "fill"),
                      values = colour_palette) +
  scale_x_date(breaks = ons_date_breaks$value,
               date_labels = "%b\n%Y") +
  scale_y_continuous(limits = c(0,12500),
                     expand = c(0,0),
                     labels = scales::label_currency(prefix = "£", suffix = "m")) +
  geom_text(data = retail_labels,
            aes(x = month_date, y = value, colour = measure, label = label),
            size = 9, fontface = "bold", hjust = 0) +
  annotate(geom = "rect", xmin = uk_lockdown_start_dates, xmax = uk_lockdown_end_dates,
           ymin = 0, ymax = 12500, alpha = 0.2, fill = "gold") +
  annotate(geom = "text", x = as_date("2014-01-01"), y = 12400,
           label = "UK national\nlockdowns during\nCovid-19 pandemic",
           fontface = "bold", size = 6, hjust = 0, vjust = 1) +
  theme(plot.title = element_text(margin = margin(0, 0, 30, 0))) +
  labs(title = "Average weekly retail sales value by type",
       x = "Month",
       y = "Average weekly retail sales value")

# The second graph show the internet share of retail sales
ons_retail_gg2 <- ons_retail_df %>%
  dplyr::select(month_date, internet_sales_pct) %>%
  ggplot(aes(x = month_date, y = internet_sales_pct)) +
  geom_line(linewidth = 2, colour = "#008080") +
  geom_point(data = ons_maxdate_df,
             aes(x = month_date, y = internet_sales_pct),
             size = 7, colour = "#008080") +
  geom_text(data = ons_maxdate_df,
            aes(x = month_date, y = internet_sales_pct,
                label = percent(internet_sales_pct)),
                hjust = 0.5, vjust = -1,
                fontface = "bold", size = 7, colour = "#008080") +
  scale_x_date(breaks = ons_date_breaks$value,
               date_labels = "%b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.4),
                     labels = scales::percent_format()) +
  geom_hline(yintercept = 0.25,
             linetype = "dashed",
             linewidth = 1.5, colour = "red") +
  annotate(geom = "text", x = as_date("2006-12-01"), y = 0.265,
           label = "One in four pounds", colour = "red",
           fontface = "bold", size = 7, hjust = 0) +
  annotate(geom = "rect", xmin = uk_lockdown_start_dates, xmax = uk_lockdown_end_dates,
           ymin = 0, ymax = 0.4, alpha = 0.2, fill = "gold") +
  theme(plot.title = element_text(color = "#008080",
                                  margin = margin(0, 0, 30, 0))) +
  labs(title = "Internet share of all retail sales",
       x = "Month",
       y = "Internet share")

# Put together these two graphs as a patchwork
ons_retail_gg <- ons_retail_gg1 + ons_retail_gg2 +
  plot_annotation(
    title = ons_retail_title,
    subtitle = stringr::str_wrap(ons_retail_subtitle, width = 120),
    caption = ons_retail_caption)

## Saving the output
png("R/Analysis_2024/05_Data_Visualisation/ons_retail_gg.png",
    width = 1800, height = 1000, unit = "px")
ons_retail_gg
dev.off()