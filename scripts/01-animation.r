#-------------------------------------------------------------------------------
## Program: baseball_tenure/scripts/01-animation.r
## Date: 2023-08-04
## Created by: Josh Radack
## Description: Creates bar chart static plot and animation
#-------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(gganimate)

# Load Data
tenure_data <- readRDS("data/cleaned/tenure_data.rds")

# Function for selecting the team and starting year
filter_data <- function(ds, team, start_year) {
  ds |>
    filter(TEAM == team, YEAR >= start_year)
}

test_data <- tenure_data |>
  filter_data("SLN", 2000)

# Static plot
ds |>
  mutate(rank = rank(-count)) |>
  ggplot(aes(rank, group = PLAYERID)) +
  geom_tile(aes(y = count/2,
                height = count,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(PLAYERID, " ")), vjust = 0.2, hjust = 1) +
  # geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  # scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  # guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))


    

make_static_plot <- function(ds) {
  ds_rank <- ds |>
    group_by(YEAR) |>
    mutate(
      rank = match(count, sort(unique(count), decreasing = TRUE)) |>
        as.factor()
    )
  
  val_lab_df <- ds_rank |>
    group_by(YEAR, rank) |>
    mutate(
      val_lab = if_else(n() > 5, "5+ Players", paste(paste(NICKNAME, LAST), collapse = "\n"))
    ) |>
    select(YEAR, count, rank, val_lab) |>
    distinct()
  
  stat_plot <- ds_rank |>
    ggplot(aes(rank)) +
    geom_tile(aes(y = count/2, height = count, width = 0.8)) +
    geom_text(data = val_lab_df, aes(y = count, label = val_lab), vjust = 0, nudge_y = 0.2) +
    theme(
      
    )
  
  return(stat_plot)
}


staticplot <- make_static_plot(test_data)

anim <- staticplot + transition_states(YEAR, transition_length = 1, state_length = 4) +
  # view_follow(fixed_x = TRUE)  +
  labs(title = 'Longest Tenured Player : {closest_state}',
       # subtitle  =  "Top 10 Countries",
       # caption  = "GDP in Billions USD | Data Source: World Bank Data"
       )

animate(anim, 200, fps = 20,  width = 1200, height = 1000,
        renderer = gifski_renderer("output/figures/gganim.gif"))
for_mp4 <- animate(anim, 200, fps = 20,  width = 1200, height = 1000,
        renderer = av_renderer())
anim_save("output/figures/animation.mp4", animation = for_mp4)
