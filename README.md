# rugby-power-index
# 
The Rugby Power Index is a statistical method for ranking rugby union
teams, based on their offensive and defensive strengths, recent form and
past results. It is based on Nate Silver's SPI, which you can read about
here: http://www.espn.com/soccer/worldcup/news/_/id/4447078/GuideToSPI

Executing 'rugby-power-index.R' (without changing any parameters) will
calculate offensive (OFF) and defensive (DEF) ratings for each team of
the 2017 Super Rugby season, as well as Rugby Power Index (RPI) ratings.

A team's OFF and DEF ratings can be interpreted as the average number of
points they would be expected to score and concede against an opponent
of average strength. OFF and DEF ratings are capped between 0 and 50,
where higher OFF values indicate strong offence and lower DEF values
indicate strong defence.

The RPI rating for each team indicates their overall relative strength,
compared to the other teams in the competition. RPI ratings lie on a
scale of 0 to 100, and can be interpreted as the percentage of matches
each team would be expected to win if they all played each other twice
(home and away), in an ideal double-round robin tournament.

There are five adjustable parameters that can affect the OFF, DEF and
RPI ratings:

M: The match-id (as listed in super-rugby-results-1996-2017.csv)
indicating the point in time at which the OFF/DEF/RPI ratings should be
calculated. Find the date for which you want to calculate OFF/DEF/RPI
ratings and set M equal to the corresponding match_id.

WND: The window of matches which will contribute to the ratings. By
default all matches played within the previous 365 days are included.

PLT: The time period leading up to the date defined by M in which past
matches are given full weight. By default this is set to 30 days.
Adjusting this allows the OFF/DEF/RPI ratings to reflect either
long-term performance or recent form, depending on whether PLT is large
or small.

CAP: The maximum value permitted for OFF and DEF ratings. This prevents
the ratings being drastically affected by unusually high-scoring
matches. By default this is set to 50.

AVG: The average number of points scored by a team in each match of the
competition. This should remain constant for a given competition, but
should be calculated for the specific competition being evaluated. By
default this is set to 24.815, the average points scored by each team in
a Super Rugby match.


