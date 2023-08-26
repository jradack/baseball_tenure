#-------------------------------------------------------------------------------
## Program: baseball_tenure/scripts/00-data_clean.r
## Date: 2023-08-04
## Created by: Josh Radack
## Description: Reads in Retrosheet roster data to construct yearly affiliation
##              of players
#-------------------------------------------------------------------------------

library(dplyr)

# Load Retrosheet player bio and team data
bio <- data.table::fread("data/raw/retrosheet/biofile.csv")
teams <- data.table::fread("data/raw/retrosheet/teams.csv")

# Function for reading in roster data
read_ros <- function(fpath, fname) {
  year <- substr(fname, 4, 7)
  team <- substr(fname, 1, 3)
  
  roster <- data.table::fread(
      paste0(fpath, fname),
      header = FALSE
    ) |>
    select(1) |>
    rename(
      PLAYERID = 1
    ) |>
    mutate(
      TEAM = team,
      YEAR = year
    )
  
  return(roster)
}

# Read in roster data
roster_folder_path <-"data/raw/retrosheet/rosters/"
roster_files <- list.files(roster_folder_path)
roster_info_raw <- lapply(roster_files, function(x) read_ros(roster_folder_path, x)) |>
  bind_rows()

# Merge team and player data with roster data and perform cleaning
roster_info <- roster_info_raw |>
  arrange(PLAYERID, YEAR, TEAM) |>
  left_join(
    teams,
    by = "TEAM"
  ) |>
  rename(
    TEAM_NICKNAME = NICKNAME,
    TEAM_FIRST_YEAR = FIRST,
    TEAM_LAST_YEAR = LAST
  ) |> 
  filter(
    !(LEAGUE == "" | is.na(LEAGUE))
  ) |>
  left_join(
    bio |>
      select(PLAYERID, LAST, FIRST, NICKNAME),
    by = "PLAYERID"
  )

# Calculate cumulative tenure with each team
tenure_data <- roster_info |>
  group_by(PLAYERID) |>
  mutate(
    # Tracks whether a player switched teams
    changed_team = TEAM != lag(TEAM, default = first(TEAM)),
    # Creates a new team name to differentiate separte stints with same team
    team_stint = paste(TEAM, cumsum(changed_team) + 1)
  ) |>
  group_by(PLAYERID, team_stint) |>
  mutate(
    count = row_number()
  ) |>
  ungroup()

# Save dataset
saveRDS(tenure_data, "data/cleaned/tenure_data.rds")
