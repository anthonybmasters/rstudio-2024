## Packages and Themes
# Install the packages and set the theme
library(tidyverse)
library(sysfonts)
library(showtext)
library(scales)

font_add_google("Spline Sans")
showtext_auto()

theme_clean <- theme_bw(base_family = "Spline Sans") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
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

## Create the hypergeometric distribution
# What is the distribution if 95 CON members were in the ballot?
total_members <- 458
con_members <- 95
sample_size <- 20
hyper_dist_df <- bind_cols(
  0:sample_size,
  dhyper(x = 0:sample_size,
         m = con_members,
         n = total_members - con_members,
         k = sample_size)) %>%
  dplyr::rename(
    balls_count = 1,
    balls_prob = 2)

# What is the probability of zero CON members being drawn from 20 balls,
# based on the number of CON members in the urn?
con_range <- 60:120
hyper_values_df <-
  bind_cols(
    con_range,
    dhyper(x = 0,
           m = con_range,
           n = total_members - con_range,
           k = sample_size)) %>%
  dplyr::rename(
    con_members = 1,
    prob_zero_chosen = 2)

## Graphs
# The first graph gives a hypergeometric distribution
hyper_dist_title <- "If there were 95 Conservative MPs in the ballot, the most likely outcome would be drawing four Conservative MPs."
hyper_dist_subtitle <- "Hypergeometric distribution with 20 balls drawn from an urn of 458 balls. It is assumed 95 Conservative MPs entered into the Private Members' Bill ballot, drawn on Thursday 05 September 2024."
hyper_dist_caption <- "Source: Hypergeometric distribution for values between 0 and 20. Total balls are equal to 458, with 95 Blue balls and a sample size of 20."
hyper_dist_gg <- hyper_dist_df %>%
  ggplot(aes(x = balls_count, y = balls_prob)) +
  geom_col(aes(fill = (balls_count == 0))) +
  scale_fill_manual(guide = "none",
                    values = c("#0087dc", "#fe8c11")) +
  geom_text(aes(label = scales::percent(balls_prob, accuracy = 0.1)),
            nudge_y = 0.01,
            fontface = "bold", size = 6) +
  scale_x_continuous(expand = c(0,0),
                     breaks = 0:20) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.25),
                     labels = scales::percent_format()) +
  labs(title = hyper_dist_title,
       subtitle = str_wrap(hyper_dist_subtitle, width = 140),
       x = "Number of Conservative MPs drawn",
       y = "Probability",
       caption = hyper_dist_caption)

# This graph shows the probability of zero CON MPs being chosen, by the number
# of CON MPs in the ballot
hyper_values_title <- "If 93 or more Conservative MPs entered the ballot, the chance of none being drawn from 20 balls is under 1 in 100."
hyper_values_subtitle <- "Probability of a hypergeometric variable being equal to 0, where the sample size is 20, the total number of balls is 458 and the number of Conservative MPs is between 60 and 120. This was the outcome of the Private Members' Bill draw on Thursday 05 September 2024."
hyper_values_caption <- "Source: Probability of a hypergeometric variable equalling 0, The urn contains 458 balls, between 60 and 120 are Blue, with 20 being drawn."
hyper_values_gg <- hyper_values_df %>%
  ggplot(aes(x = con_members, y = prob_zero_chosen)) +
  geom_line(colour = "#0087dc",
            linewidth = 2.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     labels = scales::percent_format()) +
  geom_hline(yintercept = 0.01, linetype = "dashed",
             colour = "#fe8c11", linewidth = 2) +
  annotate("text", x = 61, y = 0.012,
           label = "1 in 100",
           hjust = 0, size = 7,
           fontface = "bold", colour = "#fe8c11") +
  labs(title = hyper_values_title,
       subtitle = str_wrap(hyper_values_subtitle, width = 140),
       x = "Number of Conservative MPs entering the ballot",
       y = "Probability of no Conservative MPs being drawn",
       caption = hyper_values_caption)

## Saving the outputs
# Users need to change the output locations for their own devices
png("R/Analysis_2024/03_Hypergeometric/hyper_dist_gg.png", width = 1800, height = 1000, unit = "px")
hyper_dist_gg
dev.off()

png("R/Analysis_2024/03_Hypergeometric/hyper_values_gg.png", width = 1800, height = 1000, unit = "px")
hyper_values_gg
dev.off()