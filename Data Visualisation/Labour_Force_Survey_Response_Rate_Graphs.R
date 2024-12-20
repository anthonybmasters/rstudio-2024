## Packages and Themes
# Install the packages and set the theme
library(tidyverse)
library(curl)
library(readxl)
library(janitor)
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

## Import the data table from the Office for National Statistics website
ons_url <- "https://www.ons.gov.uk/generator?uri=/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/methodologies/labourforcesurveyperformanceandqualitymonitoringreportjulytoseptember2024/ae5c6206&format=xls"
ons_sheet <- "data"
ons_range <- "A7:G48"

temp <- tempfile()
temp <- curl::curl_download(
  url = ons_url, destfile = temp,
  quiet = TRUE, mode = "wb")

ons_file_df <- read_excel(temp,
                          sheet = ons_sheet,
                          range = ons_range) %>%
  dplyr::rename(quarter_code = 1) %>%
  janitor::clean_names() %>%
  dplyr::select(quarter_code, wave_1, wave_5, total) %>%
  dplyr::mutate_at(.vars = c("wave_1","wave_5", "total"),
                .funs = as.numeric) %>%
  dplyr::mutate(year_no = 2000 + as.numeric(substr(quarter_code, 3, 4)),
                month_no = case_when(
                  substr(quarter_code, 1, 2) == "JM" ~ 3,
                  substr(quarter_code, 1, 2) == "AJ" ~ 6,
                  substr(quarter_code, 1, 2) == "JS" ~ 9,
                  substr(quarter_code, 1, 2) == "OD" ~ 12,
                  TRUE ~ NA_real_),
                quarter_date = make_date(year = year_no, month = month_no, day = 1))

ons_tidy_df <- ons_file_df %>%
  dplyr::select(quarter_date, wave_1, wave_5, total) %>%
  tidyr::pivot_longer(cols = 2:4,
                      names_to = "wave_number",
                      values_to = "rr_pct") %>%
  dplyr::mutate(response_rate = rr_pct/100)

## Graphs
# The aim of this code is to build the visualization by layer
# to show what each bit of the code does
# First, we set up the palette, direct labels and plot labels
colour_palette <- c("black", "#008080", "#fe8c11")
lfs_labels <- tribble(
  ~wave_number, ~label, ~quarter_date, ~response_rate,
  "total", "Total", as_date("2014-12-01"), 0.50,
  "wave_1", "Wave 1", as_date("2014-12-01"), 0.62,
  "wave_5", "Wave 5", as_date("2014-12-01"), 0.38)
lfs_title <- "In July to September 2024, the Labour Force Survey response rate was under one in five."
lfs_subtitle <- "Wave-specific response rates to the Labour Force Survey in Great Britain by quarter, based on all eligible and in-scope households. These figures include partial responses, but exclude imputations, for July-September 2014 to July-September 2024."
lfs_caption <- "Source: Office for National Statistics: Labour Force Survey performance and quality monitoring report, July to September 2024."

# Additionally, we should set up the date axis labels
lfs_breaks <- seq(min(ons_tidy_df$quarter_date), max(ons_tidy_df$quarter_date),
                  by = "1 year") %>%
  as_tibble() %>%
  dplyr::rename(quarter_date = 1) %>%
  dplyr::mutate(quarter_start_date = quarter_date %m+% months(-2),
                qs_character = format(quarter_start_date, "%b"),
                qe_character = format(quarter_date, "%b\n%Y"),
                label = paste0(qs_character, "-", qe_character))

# Basic layout of the plot
lfs_gg1 <- ons_tidy_df %>%
  ggplot(aes(x = quarter_date, y = response_rate,
             group = wave_number, colour = wave_number))

# Adding lines, but they are a bit thin
lfs_gg2a <- lfs_gg1 +
  geom_line()

# Adding thicker lines
lfs_gg2 <- lfs_gg1 +
  geom_line(linewidth = 2)

# Changing colors and removing the legend
lfs_gg3 <- lfs_gg2 +
  scale_colour_manual(guide = "none",
                      values = colour_palette)

# Sort out the axes
lfs_gg4 <- lfs_gg3 +
  scale_x_date(breaks = lfs_breaks$quarter_date,
               labels = lfs_breaks$label) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.8),
                     labels = scales::percent_format())

# Add the direct labels
lfs_gg5 <- lfs_gg4 +
  geom_text(data = lfs_labels,
            aes(x = quarter_date, y = response_rate,
                label = label, colour = wave_number),
            size = 7, fontface = "bold", hjust = 0)

# Add the suspension of face-to-face interviews
lfs_gg6 <- lfs_gg5 +
  annotate(geom = "rect", xmin = as_date("2020-03-23"), xmax = as_date("2023-10-25"),
           ymin = 0, ymax = 0.8, alpha = 0.2, fill = "gold") +
  annotate(geom = "text", x = as_date("2020-05-01"), y = 0.75,
           label = "Face-to-face interviews\nsuspended due to the pandemic",
           fontface = "bold", size = 7, hjust = 0, vjust = 1)

# Finish with the plot labels
lfs_gg7 <- lfs_gg6 +
  labs(title = lfs_title,
       subtitle = str_wrap(lfs_subtitle, width = 140),
       caption = lfs_caption,
       x = "Quarterly report period",
       y = "Response rates")

## Saving the outputs
png("R/Analysis_2024/05_Data_Visualisation/lfs_gg1.png",
    width = 2000, height = 1200, unit = "px")
lfs_gg1
dev.off()

png("R/Analysis_2024/05_Data_Visualisation/lfs_gg2a.png",
    width = 2000, height = 1200, unit = "px")
lfs_gg2a
dev.off()

png("R/Analysis_2024/05_Data_Visualisation/lfs_gg2.png",
    width = 2000, height = 1200, unit = "px")
lfs_gg2
dev.off()

png("R/Analysis_2024/05_Data_Visualisation/lfs_gg3.png",
    width = 2000, height = 1200, unit = "px")
lfs_gg3
dev.off()

png("R/Analysis_2024/05_Data_Visualisation/lfs_gg4.png",
    width = 2000, height = 1200, unit = "px")
lfs_gg4
dev.off()

png("R/Analysis_2024/05_Data_Visualisation/lfs_gg5.png",
    width = 2000, height = 1200, unit = "px")
lfs_gg5
dev.off()

png("R/Analysis_2024/05_Data_Visualisation/lfs_gg6.png",
    width = 2000, height = 1200, unit = "px")
lfs_gg6
dev.off()

png("R/Analysis_2024/05_Data_Visualisation/lfs_gg7.png",
    width = 2000, height = 1200, unit = "px")
lfs_gg7
dev.off()