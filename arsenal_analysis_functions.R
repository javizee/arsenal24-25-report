# Arsenal 2024/25 Shot Analysis Functions
# Refactored from the original Quarto analysis into named R functions.
#
# Usage:
# source("arsenal_analysis_functions.R")
# arsenal_24_25 <- load_and_clean_arsenal_data("arsenal_24_25.csv")
# get_general_summary(arsenal_24_25)
# plot_defensive_box_traffic(arsenal_24_25)

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


#' Load and clean Arsenal shot data
#'
#' Reads the Arsenal 2024/25 shot dataset from a CSV file and applies the
#' basic cleaning choices used throughout the report: converting
#' `opponent_formation` to character, converting `time_of_shot_after_entry`
#' to numeric, and removing shots marked as defensive errors.
#'
#' These choices were made so the rest of the analysis compares repeatable
#' attacking patterns rather than unusual mistakes by the opponent. Defensive
#' errors can create high-quality chances that do not reflect Arsenal's normal
#' chance creation, so filtering them out keeps the report focused on Arsenal's
#' attacking structure, shot quality, and creation issues.
#'
#' @param path A string path to the CSV file.
#'
#' @return A cleaned tibble of Arsenal shot data.
load_and_clean_arsenal_data <- function(path = "arsenal_24_25.csv") {
  read_csv(path, show_col_types = FALSE) |>
    mutate(
      opponent_formation = as.character(opponent_formation),
      time_of_shot_after_entry = as.numeric(time_of_shot_after_entry)
    ) |>
    filter(defensive_error == "no")
}


#' Calculate overall shot summary statistics
#'
#' Calculates the total number of shots, number of goals, and average xG.
#'
#' These summary statistics were chosen as the opening analytical layer because
#' they give a simple baseline for Arsenal's attacking output before breaking
#' the data into more specific tactical questions. Total shots shows volume,
#' goals shows actual finishing output, and average xG shows the typical quality
#' of the chances Arsenal created.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A one-row tibble with total shots, goals, and average xG.
get_general_summary <- function(data) {
  data |>
    summarise(
      total_shots = n(),
      goals = sum(result == "goal", na.rm = TRUE),
      avg_xg = round(mean(xg, na.rm = TRUE), 3)
    )
}


#' Count shot results
#'
#' Counts how often each shot result appears, such as goals, saves, misses, or
#' blocks.
#'
#' This approach was chosen because result frequency helps separate shot volume
#' from shot effectiveness. Arsenal may shoot often, but the distribution of
#' outcomes shows whether those shots are regularly testing the goalkeeper,
#' being blocked, or actually becoming goals.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A tibble counting each shot result.
count_shot_results <- function(data) {
  data |>
    count(result) |>
    arrange(desc(n))
}


#' Count the most frequent shooters
#'
#' Counts how many shots each Arsenal player took.
#'
#' This method was chosen to identify which players carried the shooting load.
#' In a tactical report, shot concentration matters because overreliance on a
#' small group of attackers can make a team easier to defend.
#'
#' @param data A cleaned Arsenal shot tibble.
#' @param n_players Number of players to return.
#'
#' @return A tibble of player shot counts.
count_frequent_shooters <- function(data, n_players = 6) {
  data |>
    count(player) |>
    arrange(desc(n)) |>
    head(n_players)
}


#' Count most common assist types
#'
#' Counts how often each assist type led to an Arsenal shot.
#'
#' This approach was chosen because assist type gives context about how chances
#' are being created. It helps show whether Arsenal depend more on crosses,
#' cutbacks, set pieces, passes, or other creation methods.
#'
#' @param data A cleaned Arsenal shot tibble.
#' @param n_types Number of assist types to return.
#'
#' @return A tibble of assist type counts.
count_common_assist_types <- function(data, n_types = 6) {
  data |>
    count(assist_type) |>
    arrange(desc(n)) |>
    head(n_types)
}


#' Count most common shot zones
#'
#' Counts the number of shots taken from each zone.
#'
#' This method was chosen because shot location is one of the clearest ways to
#' judge attacking quality. Central shots and shots inside the box are usually
#' more dangerous, while wide or distant shots may suggest that opponents are
#' forcing Arsenal into lower-quality attempts.
#'
#' @param data A cleaned Arsenal shot tibble.
#' @param n_zones Number of zones to return.
#'
#' @return A tibble of shot zone counts.
count_common_shot_zones <- function(data, n_zones = 6) {
  data |>
    count(zone) |>
    arrange(desc(n)) |>
    head(n_zones)
}


#' Summarize opponent defensive line patterns
#'
#' Counts how often Arsenal faced each defensive line type, excluding set-piece
#' entries.
#'
#' Set pieces are removed because they do not reflect normal open-play defensive
#' shape. This analytical choice keeps the focus on how opponents defended
#' Arsenal during regular attacking possessions, which is central to the report's
#' argument about low blocks and final-third congestion.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A tibble with defensive line counts and percentages.
summarize_defensive_line <- function(data) {
  data |>
    filter(final_third_entry != "set piece") |>
    group_by(defensive_line) |>
    summarise(total = n(), .groups = "drop") |>
    mutate(percentage = total / sum(total) * 100)
}


#' Plot defenders in the box for Arsenal shots
#'
#' Creates a histogram showing the percentage of Arsenal shots by number of
#' defenders in the box, excluding set pieces.
#'
#' A percentage histogram was chosen instead of raw counts because it makes the
#' defensive traffic easier to interpret as a distribution. Excluding set pieces
#' helps isolate open-play attacking difficulty, where Arsenal often had to
#' create against several defenders packed near goal.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A ggplot histogram.
plot_defensive_box_traffic <- function(data) {
  plot_data <- data |>
    filter(type_of_attack != "set piece")

  ggplot(
    data = plot_data,
    aes(
      x = number_in_box,
      y = after_stat(count / sum(count) * 100)
    )
  ) +
    geom_histogram() +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 12, by = 1)) +
    labs(
      title = "Arsenal Shots Face Heavy Defensive Traffic - Most Have 5+ Defenders in the Box",
      x = "Number of Defenders in Box",
      y = "Percentage"
    )
}


#' Test whether pressure changes shot quality
#'
#' Runs a two-sample t-test comparing xG for shots taken under pressure versus
#' shots not taken under pressure.
#'
#' A t-test was chosen because the report is comparing the average xG of two
#' groups. The goal is not just to say that pressured shots look worse, but to
#' check whether the observed difference in mean chance quality is large enough
#' to be unlikely under random variation.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return An htest object from `t.test()`.
test_pressure_xg_difference <- function(data) {
  t.test(xg ~ shot_under_pressure, data = data)
}


#' Summarize final-third entry types
#'
#' Groups shots by final-third entry type after combining free kicks and corners
#' into a single set-piece category, while excluding recoveries.
#'
#' This approach was chosen because the report is interested in Arsenal's main
#' routes of chance creation. Recoveries are removed because they are not planned
#' possession entries, and set-piece events are grouped together because they
#' represent a different tactical phase from open-play wing or halfspace attacks.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A tibble with counts and proportions by entry type.
summarize_attack_entries <- function(data) {
  data |>
    filter(entry_method != "recovery") |>
    mutate(
      entry_type = case_when(
        final_third_entry %in% c(
          "free kick",
          "direct freekick",
          "left corner",
          "right corner"
        ) ~ "set piece",
        TRUE ~ final_third_entry
      )
    ) |>
    select(-final_third_entry) |>
    group_by(entry_type) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(prop = count / sum(count))
}


#' Calculate the percentage of goals from set pieces
#'
#' Finds what percentage of Arsenal goals came from set-piece attacks.
#'
#' This measure was chosen because set-piece dependence is important when judging
#' attacking creativity. A high percentage of goals from set pieces can suggest
#' that open-play creation was less reliable, especially for a team expected to
#' dominate possession.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A numeric percentage.
calculate_set_piece_goal_percentage <- function(data) {
  data |>
    filter(result == "goal") |>
    count(type_of_attack) |>
    mutate(percentage = n / sum(n) * 100) |>
    filter(type_of_attack == "set piece") |>
    pull(percentage)
}


#' Plot touches before shot
#'
#' Creates a percentage histogram of how many touches Arsenal players took before
#' shooting.
#'
#' This approach was chosen to evaluate whether Arsenal players were creating
#' shots for themselves or relying on one-touch finishes and team passing
#' patterns. Touches before shot gives a simple proxy for individual shot
#' creation and dribbling involvement.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A ggplot histogram.
plot_touches_before_shot <- function(data) {
  ggplot(
    data = data,
    aes(
      x = touches_before_shot,
      y = after_stat((count / sum(count)) * 100)
    )
  ) +
    geom_histogram() +
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    labs(
      x = "Touches Before Shot",
      y = "Percentage"
    )
}


#' Summarize shots after beating a defender
#'
#' Calculates the number and proportion of all shots where an Arsenal player beat
#' a defender before the shot.
#'
#' This metric was chosen because beating a defender is a direct sign of
#' individual chance creation. Against compact defenses, dribbling past an
#' opponent can break the defensive shape in a way passing alone may not.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A one-row tibble with total shots, shots after beating a defender,
#'   and the proportion.
summarize_shots_after_beating_defender <- function(data) {
  data |>
    summarise(
      total_shots = n(),
      shots_after_beating_defender = sum(beaten_by != "n/a", na.rm = TRUE),
      proportion = shots_after_beating_defender / total_shots
    )
}


#' Summarize goals after beating a defender
#'
#' Calculates the number and proportion of goals where an Arsenal player beat a
#' defender before the shot.
#'
#' This approach was chosen to connect individual dribbling actions to actual
#' scoring output, not just shot creation. It helps test the report's idea that
#' Arsenal need more players who can create separation and turn attacks into
#' goals.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A one-row tibble with total goals, goals after beating a defender,
#'   and the proportion.
summarize_goals_after_beating_defender <- function(data) {
  data |>
    filter(result == "goal") |>
    summarise(
      total_goals = n(),
      goals_after_beating_defender = sum(beaten_by != "n/a", na.rm = TRUE),
      proportion = goals_after_beating_defender / total_goals
    )
}


#' Rank players by defender-beating actions before shots
#'
#' Counts which Arsenal players most often beat a defender before a shot.
#'
#' This method was chosen to identify who provides individual attacking threat.
#' The report argues that Arsenal may rely too heavily on certain players, so
#' ranking defender-beating actions helps reveal whether that burden is spread
#' across the squad or concentrated on a few attackers.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A tibble ranking players by times they beat a defender before a shot.
rank_defender_beating_players <- function(data) {
  data |>
    mutate(beaten_by = na_if(beaten_by, "n/a")) |>
    filter(!is.na(beaten_by)) |>
    group_by(beaten_by) |>
    summarise(times_beaten_defender = n(), .groups = "drop") |>
    mutate(proportion = times_beaten_defender / sum(times_beaten_defender)) |>
    arrange(desc(times_beaten_defender))
}


#' Prepare left-versus-right wing entry data
#'
#' Combines left and left-halfspace entries into a single left category, and
#' right and right-halfspace entries into a single right category.
#'
#' This recoding was chosen because the report is comparing Arsenal's broader
#' left-side and right-side attacking output. Combining wide and halfspace zones
#' makes the comparison easier and better aligned with the tactical question of
#' whether one side of the pitch is more productive.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A tibble filtered to left and right attacking entries.
prepare_wing_entry_data <- function(data) {
  data |>
    mutate(
      final_third_entry = case_when(
        final_third_entry %in% c("left", "left halfspace") ~ "left",
        final_third_entry %in% c("right", "right halfspace") ~ "right",
        TRUE ~ final_third_entry
      )
    ) |>
    filter(final_third_entry %in% c("left", "right"))
}


#' Plot xG by left and right final-third entry
#'
#' Creates a boxplot comparing xG from left-side and right-side entries.
#'
#' A boxplot was chosen because it shows both the typical xG level and the
#' spread of chance quality, including whether one side produces more high-end
#' chances. This matches the report's question about whether Arsenal's right
#' side creates better opportunities than the left.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A ggplot boxplot.
plot_xg_by_wing_entry <- function(data) {
  wings_data <- prepare_wing_entry_data(data)

  ggplot(wings_data, aes(x = final_third_entry, y = xg)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "xG by Final Third Entry",
      x = "Entry Type",
      y = "xG"
    )
}


#' Test xG differences after beating a defender by zone
#'
#' For each shot zone, compares xG between shots where a defender was beaten and
#' shots where a defender was not beaten using a Wilcoxon test.
#'
#' A Wilcoxon test was chosen instead of a t-test because the analysis is split
#' into smaller zone-level groups, where xG may be skewed and sample sizes may
#' be limited. The zone-by-zone design also controls for location better than
#' one overall comparison, since shot zone strongly affects xG.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A tibble with p-values, group means, and sample sizes by zone.
test_beating_defender_xg_by_zone <- function(data) {
  data |>
    filter(type_of_attack != "set piece") |>
    mutate(
      beaten_by = na_if(beaten_by, "n/a"),
      player_beaten = if_else(!is.na(beaten_by), "yes", "no")
    ) |>
    filter(!is.na(zone), !is.na(xg)) |>
    group_by(zone) |>
    filter(n_distinct(player_beaten) == 2) |>
    summarise(
      p_value = wilcox.test(xg ~ player_beaten)$p.value,
      mean_beaten = mean(xg[player_beaten == "yes"], na.rm = TRUE),
      mean_not_beaten = mean(xg[player_beaten == "no"], na.rm = TRUE),
      n_beaten = sum(player_beaten == "yes"),
      n_not_beaten = sum(player_beaten == "no"),
      .groups = "drop"
    )
}


#' Summarize Arsenal transition shots
#'
#' Calculates shot count, average xG, average time to shoot, number of goals, and
#' goal proportion for transition attacks.
#'
#' This approach was chosen because transition attacks are especially important
#' against teams that sit deep. When compact defenses are hard to break down,
#' quick attacks before defenders recover can be one of the best ways to create
#' higher-quality chances.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A one-row tibble summarizing transition shots.
summarize_transition_shots <- function(data) {
  data |>
    filter(transition == "yes") |>
    summarise(
      total = n(),
      average_xg = mean(xg, na.rm = TRUE),
      average_time_to_shoot = mean(time_of_shot_after_entry, na.rm = TRUE),
      num_goals = sum(result == "goal", na.rm = TRUE),
      goals_prop = num_goals / total
    )
}


#' Summarize a player's shots by zone
#'
#' Counts a selected player's shots by zone and calculates the proportion of that
#' player's shots from each zone.
#'
#' This function was chosen for comparing Arsenal's left-wing options because it
#' shows where each player actually gets shots from. Zone distribution helps
#' connect player role and positioning to the broader tactical question of
#' whether Arsenal need a different left-sided profile.
#'
#' @param data A cleaned Arsenal shot tibble.
#' @param player_name The player's name as it appears in the `player` column.
#'
#' @return A tibble with shot counts and proportions by zone.
summarize_player_shots_by_zone <- function(data, player_name) {
  data |>
    filter(player == player_name) |>
    group_by(zone) |>
    summarise(shots = n(), .groups = "drop") |>
    mutate(proportion = shots / sum(shots)) |>
    arrange(desc(shots))
}


#' Run the main Arsenal report tables
#'
#' Produces a named list of the main summary tables and statistical tests used in
#' the report.
#'
#' This wrapper was chosen to make the Quarto report easier to reproduce. Instead
#' of repeating many separate code chunks, the report can call one function and
#' then print the named outputs where needed.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A named list of summary tables and test results.
run_arsenal_report_summaries <- function(data) {
  list(
    general_summary = get_general_summary(data),
    shot_results = count_shot_results(data),
    frequent_shooters = count_frequent_shooters(data),
    common_assist_types = count_common_assist_types(data),
    common_shot_zones = count_common_shot_zones(data),
    defensive_line = summarize_defensive_line(data),
    pressure_test = test_pressure_xg_difference(data),
    attack_entries = summarize_attack_entries(data),
    set_piece_goal_percentage = calculate_set_piece_goal_percentage(data),
    shots_after_beating_defender = summarize_shots_after_beating_defender(data),
    goals_after_beating_defender = summarize_goals_after_beating_defender(data),
    defender_beating_players = rank_defender_beating_players(data),
    beating_defender_zone_tests = test_beating_defender_xg_by_zone(data),
    transition_summary = summarize_transition_shots(data),
    trossard_shots_by_zone = summarize_player_shots_by_zone(
      data,
      "Leandro Trossard"
    ),
    martinelli_shots_by_zone = summarize_player_shots_by_zone(
      data,
      "Gabriel Martinelli"
    )
  )
}
