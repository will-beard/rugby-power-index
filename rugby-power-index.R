# +++++ THE RUGBY POWER INDEX ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(nleqslv)  # library for solving systems of non-linear equations
library(nnet)  # library for multinomial logistic regression

# +++++ LOAD DATA ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load('super-rugby-mlr.rda')  # previously-computed multinomial logistic regression model
matches <- read.csv(file="super-rugby-results-1996-2017.csv", header=TRUE, sep=",")  # results of every Super Rugby match, 1996-2017 seasons
# note match_id '1171' was cancelled due to the 2011 Canterbury earthquakes (for completeness it's recorded in super-rugby-results-1996-2017.csv as a 0-0 draw)

# +++++ PARAMETERS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
M <- 2069  # match_id of match to be processed
WND <- 365  # size of window (in days) over which RPI is calculated
PLT <- 30  # size of plateau before window drops off (matches within this many days of most recent receive full weight, older drop off linearly)
CAP <- 50  # largest absolute value permitted for APS/APC (to prevent ratings from overreacting to blowouts)
AVG <- 24.815  # average points scored per match by each team of the 1996 - 2016 Super Rugby seasons

# +++++ DEFINE OFF/DEF OBJECTIVE FUNCTION ++++++++++++++++++++++++++++++++++++++++++++++++++
#   Takes a vector x of APS, APC, OFF and DEF values and returns a vector g
#   of each evaluated with the appropriate function. Requires the following
#   to be available:
#
#   data: results dataset for matches within WND days of match M
#   w: vector of weights for each match 
#   teams: (unordered) list of teams obtained from 'data' dataset
#   AVG: baseline average points scored by all teams
#
#   Input/output vector ordering:
#   APS_hometeam_match_m
#   APS_awayteam_match_m
#   APC_hometeam_match_m
#   APC_awayteam_match_m
#    ...
#   OFF_team_t
#   DEF_team_t
#
#   where matches 'm' are ordered from oldest to most recent, and teams 't' are
#   ordered according to their ordering in 'teams'

obj <- function(x) {
  
  M <- nrow(data)  # number of matches
  N <- length(teams)  # number of teams
  g <- vector("integer", 4*M+2*N)  # initialise output vector
  m <- 0  # counter to keep track of which match is being processed
  
  for (k in seq.int(1, 4*M, 4)) {  # evaluate APS and APC for each team of each match
    
    m <- m + 1  # increment match counter
    
    iH <- 4*M + 2*which(teams==data$home_team[m])  # get index of home team DEF
    iA <- 4*M + 2*which(teams==data$away_team[m])  # get index of away team DEF
    
    g[k]   <- x[k]   - min(CAP, (data$home_score[m]/x[iA])   * AVG)  # compute APS for home team (capped at CAP)
    g[k+1] <- x[k+1] - min(CAP, (data$away_score[m]/x[iH])   * AVG)  # compute APS for away team (capped at CAP)
    g[k+2] <- x[k+2] - min(CAP, (data$away_score[m]/x[iA-1]) * AVG)  # compute APC for home team (capped at CAP)
    g[k+3] <- x[k+3] - min(CAP, (data$home_score[m]/x[iH-1]) * AVG)  # compute APC for away team (capped at CAP)
  }
  
  t <- 0  # counter to keep track of which team is being processed
  
  for (k in seq.int(4*M+1, 4*M+2*N, 2)) {  # evaluate OFF and DEF for each team 
    
    t <- t + 1  # increment team counter
    
    iH <- 4*which(data$home_team==teams[t]) - 3  # get indicies of APS for each home match
    iA <- 4*which(data$away_team==teams[t]) - 2  # get indicies of APS for each away match
    
    w.team <- w[c(iH + 3, iA + 2)/4]  # pick out weights for each of team t's matches 
    w.team <- w.team/sum(w.team)  # normalise
    
    g[k]   <- x[k]   - min(CAP, (sum(w.team * x[c(iH, iA)])))  # compute weighted average of each APS for team t (capped at CAP)
    g[k+1] <- x[k+1] - min(CAP, (sum(w.team * x[c(iH+2, iA+2)])))  # compute weighted average of each APC for team t (capped at CAP)
  }   
  
  return(g)
}


# +++++ COMPUTE APS, APC, OFF, DEF +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

most.recent <- as.POSIXct(matches$date[M])  # date of most recent match in set
data <- matches[(as.POSIXct(matches$date) >= most.recent - WND*86400) & (as.POSIXct(matches$date) <= most.recent), ]  # get matches going back WND days from most recent
teams <- unique(unlist(data[, c(2,5)]))  # get list of teams who played within WND days of most recent match
ratings <- data.frame(Team=teams, OFF=numeric(length(teams)), DEF=numeric(length(teams)), RPI=numeric(length(teams)))  # initialise data frame to hold calculated ratings

w <- vector("integer", nrow(data))  # initialise weighting vector

for (m in seq.int(nrow(data))) {  # step through matches getting weights for each
  
  dif <- as.numeric(difftime(most.recent, data$date[m], units="days"))  # time difference between most recent match and match m
  
  if (dif <= PLT) {  # matches within PLT days of most recent recieve full weight
    w[m] <- WND
  } else {
    w[m] <- WND + PLT - dif  # older matches receive linearly decreasing weighting
  }
}

x0 <- rep(AVG, 4*nrow(data) + 2*length(teams))  # set initial estimate to AVG
x <- nleqslv(x0, obj)  # solve system of equations for APS, APC, OFF, DEF

ratings$OFF <- x$x[seq.int(1, 2*length(teams) - 1, 2) + 4*nrow(data)]  # extract OFF ratings
ratings$DEF <- x$x[seq.int(2, 2*length(teams)    , 2) + 4*nrow(data)]  # extract DEF ratings


# +++++ CALCULATE RPI RATINGS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

h.probs <- matrix(NaN, length(teams), length(teams))  # initialise matrix to hold home win probabilities
a.probs <- matrix(NaN, length(teams), length(teams))  # initialise matrix to hold away win probabilities

for (home.team in seq.int(1:length(teams))) {

  for (away.team in seq.int(1:length(teams))) {

    if (home.team!=away.team) {  # no need to play each team against itself
      
      offh <- ratings$OFF[home.team]
      defh <- ratings$DEF[home.team]
      offa <- ratings$OFF[away.team]
      defa <- ratings$DEF[away.team]

      p <- predict(mod, data.frame(offh, defh, offa, defa), "probs")  # get probabilities for match
      h.probs[home.team, away.team] <- p[3]  # insert probability of home team winning against away team
      a.probs[home.team, away.team] <- p[2]  # insert probability of away team winning against home team
    }
  }
}

for (t in seq.int(1:length(teams))) {  # step through teams calculating RPI
  ratings$RPI[t] <- 100*mean( c(h.probs[t, ], a.probs[, t]), na.rm=TRUE)  # RPI is average wins in home and away round-robin tournament (expressed as a percentage)
}

print(ratings[order(-ratings$RPI), ])  # print out ratings, sorted by RPI
