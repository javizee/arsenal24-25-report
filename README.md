# Arsenal 2024/25 Shot Analysis Report
An analysis of Arsenal’s attacking performance in the 2024/25 Premier League season, focusing on shot creation, chance quality, and tactical trends.
Author
Javier Zialcita

# Motivation / Goal
I created this project to put the skills I’ve learned through my classes at Cal Poly towards a topic I’m really passionate about and hope to pursue in the future. I’ve been supporting Arsenal Football Club since I was 7 years old and after a frustrating season, I wanted to test myself to see if I could reach some conclusions using the main programming language I’ve learned through my classes in pursuit of a statistics degree.

Analysts of soccer base their reports off of this. As for the coaching and recruiting staff at Arsenal, I would’ve shown this report to them at the start of the transfer window back in June to guide them towards how they can improve their tactics, or get different players that can achieve the playstyle necessary to improve. However, people that aren’t involved in the sport professionally can still read this to gain an understanding of Arsenal’s strengths and weaknesses.

# Features
Data cleaning and preparation: Processes Arsenal’s 2024/25 shot data (arsenal_24_25.csv), including filtering, recoding variables, and adding derived features like player_beaten and side.

Shot outcome summaries: Breaks down results of shots (goal, saved, blocked, off target) and assist types (cross, through ball, cutback, set piece).

Statistical testing: Uses t-tests, Wilcoxon tests, chi-square tests, and logistic regression to explore how pressure, dribbling, angle, and shot type affect chance quality and scoring.

Visualizations: Produces charts showing shot zones, set piece reliance, and outcome distributions to make findings easy to see.

Narrative explanations: Translates statistical results into plain language so even readers who don’t watch soccer can follow the story.

Future adaptability: Regression model can be applied to future datasets to predict scoring likelihood under different conditions.

I got all the data manually, by going through all 38 Arsenal game highlights on Youtube.

# Data
The dataset arsenal_24_25.csv contains one row per Arsenal shot in the 2024/25 season. All the variables are described below

date → Date of the match when shot was taken
matchday → the round of the league season in which the game was played
opponent → the team that the shot was against
minute→ the minute the shot was taken in the match
time_of_shot_after_entry → how many seconds pass between Arsenal moving the ball into the attacking area near the opponent’s goal and when they shoot
player → player who takes the shot
result → Shot outcome (goal, saved, blocked, off target, deflected off target, own goal)
xg → a number that measures the quality of a shot by estimating how likely it is to become a goal
body_part → body part used to shoot (right, left, head, other
’dominant_foot` → whether the shooter used their dominant foot or not
final_third_entry → area/method in which the ball entered the final third - “left halfspace” → area between the left and middle columns of the field - “right halfspace” → area between the right and middle columns of the field - “right corner” → corner kick taken from right side of the field - “left corner” → corner kick taken from the left side of the field - “direct freekick” → pass from a free kick awarded by the referee after a foul
entry_method → method used to enter the final third
type_of_attack → whether the shot came from transitional play, positional play, or a set piece
pre_assist → which player passed it to the player who passed to the shooter
‘pre_assist_type’ → how the pre assister passed to the assister
assisted_by → player who passed it to the shooter
assist_type → how the assister passed to the shooter
shot_type → type of shot the shooter took - “normal” → a normal driven shot - “header” → a shot taken with the head while the ball is in the air - “finesse” → a curved shot with the inside of the foot - “volley” → a shot taken with the feet while the ball is in the air - “free kick” → a direct shot from a free kick situation outside the box
touches_before_shot → the number of times the shooter touches the ball before taking his shot after receiving the ball
zone → the area in which the shot was taken (refer to the figure given below)
distance → the distance from goal when the shot was taken
shot_angle → how much of the goal the shooter can see when they take the shot. - “straight” → the shooter is directly in front of the goal - “wide” → the shooter is off to the side - “narrow” → the shooter is closer to the end line at a very tight angle
beaten_by → the player that gets past a defender on the dribble leading up to the shot
linkup_play → whether or not there was quick and intricate passing between the lines before the goal
shot_under_pressure → whether or not the opposition defender is close enough to the shooter to affect his shot
defenders_between → how many defenders are directly between the shooter and the goal
defensive_line → the defensive style of the opponents - “low” → the opponents defend deep in their own half remaining very compact - “mid” → the opponents try to control the middle of the field, limiting the offense from entering the final third - “high” → the opponents press the offense in their own half, trying to win the ball and score right away
numbers_in_box → how many defensive players are in the 18 yard box at the time of shot, excluding the goalkeeper
transition → whether or not the shot resulted from an Arsenal transition from defense to attack
defensive_error → whether or not the defense made an error leading up to the shot
opponent_formation → what formation the opponent lined up with
# Installation / Requirements
This project required the following packages - readr - tidyverse - ggplot

# Usage
library(quarto)
quarto::quarto_render("Arsenal Report.qmd")
View the report: Arsenal Report (HTML)

# Key Findings
Shot efficiency: Arsenal generated 291 open-play shots and converted 63 of them (about an 18% success rate). Most of these looks came from central areas inside the box.

Pressure impact: When defenders applied pressure, Arsenal’s shots dropped to an average xG of 0.14. Without pressure, that number nearly doubled to 0.24. Pressure clearly reduced the quality of chances.

Set piece reliance: 22% of goals came from set pieces. For a team that should be controlling open play, that’s a high number and shows how often Arsenal leaned on dead-ball situations to score.

Wing imbalance: The attack tilted heavily to the right side through Saka and Ødegaard. Even though he missed half the season, Saka still accounted for about 30% of successful dribbles. On the left, Martinelli and Trossard weren’t nearly as effective, leaving the attack lopsided.

Dribbling threat: Around 22% of all shots and goals came right after a player beat a defender one-on-one. That’s a huge share and shows how dangerous Arsenal can be when someone breaks a line. With stronger wide players, this could become a consistent weapon.

# Setbacks
small sample sizes for certain shot types and situations
tedious to create this dataset myself by watching videos, ideally I’d have more resources like the professionals do
some variables such as time_of_shot_after_entry, defensive_line, and defenders_between were up to interpretation and weren’t exact
some results weren’t statistically significant, but they still highlighted patterns (such as imbalance between wings) that are meaningful for tactical analysis
# Future Work
add some kind of defensive analysis
add player level breakdowns using websites and webscraping
visualization upgrades such as heatmaps or pitch control
build a fitted model and apply it to data from a different season or team
# Acknowledgements
Thank you to Understat for providing xg data and the opensource tools and libraries in R. Special thanks to the Arsenal community and YouTube highlights for making manual data collection possible.
