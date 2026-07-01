# Arsenal 2024/25 Shot Analysis Functions
# Refactored from the original Quarto analysis into named R functions.
# Uses knitr::kable() for tables to avoid extra package issues with gt.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(scales)


# -------------------------------------------------------------------------
# Styling helpers
# -------------------------------------------------------------------------

#' Apply a clean report theme to Arsenal plots
#'
#' Creates a consistent ggplot theme for the report.
#'
#' This theme was chosen so the visualizations look less like raw notebook
#' output and more like finished report graphics. The goal is to keep the plots
#' readable, simple, and professional without distracting from the analysis.
#'
#' @return A ggplot theme.
theme_arsenal <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, margin = margin(b = 8)),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "gray20"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.caption = element_text(color = "gray40", size = 9),
      plot.margin = margin(10, 12, 10, 12)
    )
}


#' Format a table for the Quarto report
#'
#' Converts a data frame or tibble into a cleaner `knitr::kable()` table.
#'
#' This was chosen instead of raw tibble output because kable tables are easier
#' to read in the rendered HTML report while avoiding extra package dependency
#' problems. It gives the project a cleaner look without requiring `gt`.
#'
#' @param data A data frame or tibble.
#' @param caption Optional table caption.
#' @param digits Number of digits to display for numeric columns.
#'
#' @return A formatted knitr table.
make_table <- function(data, caption = NULL, digits = 3) {
  knitr::kable(
    data,
    caption = caption,
    digits = digits,
    align = "c"
  )
}


# -------------------------------------------------------------------------
# Data loading and summaries
# -------------------------------------------------------------------------

#' Load and clean Arsenal shot data
#'
#' Reads the Arsenal 2024/25 shot dataset from a CSV file and applies the basic
#' cleaning choices used throughout the report: converting `opponent_formation`
#' to character, converting `time_of_shot_after_entry` to numeric, and removing
#' shots marked as defensive errors.
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


#' Create formatted overall summary table
#'
#' Formats the overall shot summary as a readable table.
#'
#' This was chosen so the opening summary looks polished in the HTML report
#' instead of appearing as raw console output.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_general_summary <- function(data) {
  make_table(
    get_general_summary(data),
    caption = "Overall Shot Summary"
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
    count(result, name = "shots") |>
    arrange(desc(shots))
}


#' Create formatted shot results table
#'
#' Formats shot result counts as a readable table.
#'
#' This was chosen to make the shot outcome distribution easier to scan in the
#' final report.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_shot_results <- function(data) {
  make_table(
    count_shot_results(data),
    caption = "Shot Results"
  )
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
    count(player, name = "shots") |>
    arrange(desc(shots)) |>
    slice_head(n = n_players)
}


#' Create formatted frequent shooters table
#'
#' Formats the most frequent shooters as a readable table.
#'
#' This table makes it easier to see which players carried Arsenal's shot volume.
#'
#' @param data A cleaned Arsenal shot tibble.
#' @param n_players Number of players to return.
#'
#' @return A formatted knitr table.
table_frequent_shooters <- function(data, n_players = 6) {
  make_table(
    count_frequent_shooters(data, n_players),
    caption = "Most Frequent Shooters"
  )
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
    count(assist_type, name = "shots") |>
    arrange(desc(shots)) |>
    slice_head(n = n_types)
}


#' Create formatted assist type table
#'
#' Formats the most common assist types as a readable table.
#'
#' This was chosen so creation patterns can be read quickly without raw console
#' formatting.
#'
#' @param data A cleaned Arsenal shot tibble.
#' @param n_types Number of assist types to return.
#'
#' @return A formatted knitr table.
table_common_assist_types <- function(data, n_types = 6) {
  make_table(
    count_common_assist_types(data, n_types),
    caption = "Most Common Assist Types"
  )
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
    count(zone, name = "shots") |>
    arrange(desc(shots)) |>
    slice_head(n = n_zones)
}


#' Create formatted shot zone table
#'
#' Formats the most common shot zones as a readable table.
#'
#' This was chosen because the zone counts support the report's discussion of
#' where Arsenal most often created shots from.
#'
#' @param data A cleaned Arsenal shot tibble.
#' @param n_zones Number of zones to return.
#'
#' @return A formatted knitr table.
table_common_shot_zones <- function(data, n_zones = 6) {
  make_table(
    count_common_shot_zones(data, n_zones),
    caption = "Most Common Shot Zones"
  )
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
    mutate(percentage = round(total / sum(total) * 100, 1)) |>
    arrange(desc(total))
}


#' Create formatted defensive line table
#'
#' Formats defensive line counts and percentages as a readable table.
#'
#' This was chosen to make the low-block pattern easier to read in the final
#' report.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_defensive_line <- function(data) {
  make_table(
    summarize_defensive_line(data),
    caption = "Opponent Defensive Line Types"
  )
}


# -------------------------------------------------------------------------
# Plots and statistical analyses
# -------------------------------------------------------------------------

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
    geom_histogram(binwidth = 1, boundary = 0, color = "white", linewidth = 0.4) +
    scale_x_continuous(breaks = seq(0, 12, by = 1)) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = "Arsenal Shots Faced Heavy Defensive Traffic",
      subtitle = "Open-play shots often came with several defenders already in the box",
      x = "Number of Defenders in Box",
      y = "Percentage of Shots"
    ) +
    theme_arsenal()
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


#' Summarize pressure test results in a table
#'
#' Extracts the most important values from the pressure t-test into a clean
#' table.
#'
#' This was chosen because raw test output is hard for readers to scan. A small
#' table makes the mean xG difference and p-value easier to interpret.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_pressure_xg_difference <- function(data) {
  pressure_test <- test_pressure_xg_difference(data)
  
  pressure_summary <- tibble(
    mean_xg_no_pressure = unname(pressure_test$estimate["mean in group no"]),
    mean_xg_under_pressure = unname(pressure_test$estimate["mean in group yes"]),
    difference = mean_xg_no_pressure - mean_xg_under_pressure,
    p_value = pressure_test$p.value
  )
  
  make_table(
    pressure_summary,
    caption = "Effect of Defensive Pressure on xG"
  )
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
    group_by(entry_type) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(prop = round(count / sum(count), 3)) |>
    arrange(desc(count))
}


#' Create formatted final-third entry table
#'
#' Formats the attack entry summary as a readable table.
#'
#' This was chosen because the entry table is central to the argument about
#' Arsenal's wing and set-piece reliance.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_attack_entries <- function(data) {
  make_table(
    summarize_attack_entries(data),
    caption = "Final-Third Entry Types"
  )
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


#' Create formatted set-piece goal table
#'
#' Formats the set-piece goal percentage as a readable table.
#'
#' This was chosen so the single percentage is clearly labeled in the report.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_set_piece_goal_percentage <- function(data) {
  set_piece_summary <- tibble(
    metric = "Goals From Set Pieces",
    percentage = calculate_set_piece_goal_percentage(data)
  )
  
  make_table(
    set_piece_summary,
    caption = "Goals From Set Pieces"
  )
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
    geom_histogram(binwidth = 1, boundary = 0, color = "white", linewidth = 0.4) +
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = "Touches Before Arsenal Shots",
      subtitle = "Most shots came after zero or one touch",
      x = "Touches Before Shot",
      y = "Percentage of Shots"
    ) +
    theme_arsenal()
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


#' Create formatted dribbling impact summary table
#'
#' Combines shot and goal summaries after beating a defender into one readable
#' table.
#'
#' This was chosen because putting shots and goals together makes the dribbling
#' impact easier to compare.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_defender_beating_summary <- function(data) {
  shots <- summarize_shots_after_beating_defender(data) |>
    transmute(
      metric = "Shots after beating defender",
      total = total_shots,
      count = shots_after_beating_defender,
      proportion = proportion
    )
  
  goals <- summarize_goals_after_beating_defender(data) |>
    transmute(
      metric = "Goals after beating defender",
      total = total_goals,
      count = goals_after_beating_defender,
      proportion = proportion
    )
  
  bind_rows(shots, goals) |>
    make_table(caption = "Shots and Goals After Beating a Defender")
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


#' Create formatted defender-beating player table
#'
#' Formats the defender-beating player rankings as a readable table.
#'
#' This was chosen to make the Saka reliance easier to see in the report.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_defender_beating_players <- function(data) {
  rank_defender_beating_players(data) |>
    make_table(caption = "Defender-Beating Actions Before Shots")
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
#' A boxplot was chosen because it shows both the typical xG level and the spread
#' of chance quality, including whether one side produces more high-end chances.
#' This matches the report's question about whether Arsenal's right side creates
#' better opportunities than the left.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A ggplot boxplot.
plot_xg_by_wing_entry <- function(data) {
  wings_data <- prepare_wing_entry_data(data)
  
  ggplot(wings_data, aes(x = final_third_entry, y = xg)) +
    geom_boxplot(width = 0.55, outlier.alpha = 0.6) +
    labs(
      title = "Expected Goals by Side of Attack",
      subtitle = "Comparing xG from left-side and right-side final-third entries",
      x = "Side of Attack",
      y = "Expected Goals (xG)"
    ) +
    theme_arsenal()
}


#' Test xG differences after beating a defender by zone
#'
#' For each shot zone, compares xG between shots where a defender was beaten and
#' shots where a defender was not beaten using a Wilcoxon test.
#'
#' A Wilcoxon test was chosen instead of a t-test because the analysis is split
#' into smaller zone-level groups, where xG may be skewed and sample sizes may be
#' limited. The zone-by-zone design also controls for location better than one
#' overall comparison, since shot zone strongly affects xG.
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


#' Create formatted zone-level defender-beating test table
#'
#' Formats the Wilcoxon zone-level test output as a readable table.
#'
#' This was chosen because the raw test table contains several technical values,
#' and formatting makes the zone comparison easier to read.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_beating_defender_xg_by_zone <- function(data) {
  test_beating_defender_xg_by_zone(data) |>
    make_table(caption = "xG After Beating a Defender by Zone")
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


#' Create formatted transition summary table
#'
#' Formats the transition attack summary as a readable table.
#'
#' This was chosen to make transition efficiency easier to read in the final
#' report.
#'
#' @param data A cleaned Arsenal shot tibble.
#'
#' @return A formatted knitr table.
table_transition_shots <- function(data) {
  summarize_transition_shots(data) |>
    make_table(caption = "Transition Shot Summary")
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


#' Create formatted player shot zone table
#'
#' Formats a selected player's shot zone distribution as a readable table.
#'
#' This was chosen to make the player comparison cleaner in the transfer
#' recommendation section.
#'
#' @param data A cleaned Arsenal shot tibble.
#' @param player_name The player's name as it appears in the `player` column.
#'
#' @return A formatted knitr table.
table_player_shots_by_zone <- function(data, player_name) {
  summarize_player_shots_by_zone(data, player_name) |>
    make_table(caption = paste(player_name, "Shots by Zone"))
}


#' Run the main Arsenal report summaries
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

