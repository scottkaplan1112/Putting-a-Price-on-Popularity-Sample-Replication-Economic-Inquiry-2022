# load in the data
load("D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\StephenCurryData.RData")

library(tidyr)
library(dplyr) 
library(ggplot2)
library(lubridate)# package to handle time and date variables
library(lfe)
library(data.table)


# Create Treatment Time column
create_timetreatment_col <- function(data, date, bin = 14) {
  
  print("Adding Treatment Time column")
  
  # Drop all Superstar == 1 rows that don't correspond to date
  data <- data %>% filter(MatchupDate == date | Superstar == 0)
  
  # Get the treatment time for this matchup
  treatment_time <- unique((data %>% filter(Superstar == 1 & Delta_Round == 0))$HoursToGame)
  
  print(".  Found treatment time")
  
  if (length(treatment_time) < 1) {
    return(0)
  }
  
  # Calculate the time difference for each row (i.e. where the row is w.r.t. HTG)
  data$TimeTreatment <- (treatment_time - data$HoursToGame)*2
  
  # Create binning function to be applied
  bin_fun <- function(n) {
    if (n < -bin) {
      return(-bin)
    } else if (n > bin) {
      return(bin)
    } else {
      return(n)
    }
  }
  
  # Bin this time variable using the apply function
  data$TimeTreatment <- sapply(data$TimeTreatment, bin_fun)
  
  print(".  Binned treatment time")
  
  # Coerce variables as factors for fixed effects
  data$MatchupID <- as.factor(data$MatchupID)
  data$ScrapeDate <- as.factor(data$ScrapeDate)
  data$ScrapeTime <- as.factor(data$ScrapeTime)
  data$SectionName <- as.factor(data$SectionName)
  data$Delta_Round <- as.factor(data$Delta_Round)
  data$Superstar <- as.factor(data$Superstar)
  data$TimeTreatment <- as.factor(data$TimeTreatment)
  
  # Relevent time variable to omit -1 event time
  data <- within(data, TimeTreatment <- relevel(TimeTreatment, ref = "-1"))
  
  return(data)
  
}


# removing unsold listings from the analysis
remove_listings_that_didnt_sell <- function(table) {
  listings <- table %>%
    group_by(ListingID) %>%
    summarize(lastTimestamp = min(HoursToGame))
  badListings <- listings$ListingID[listings$lastTimestamp <= 1.0]
  
  return(table %>%
           filter(!(ListingID %in% badListings)))
}


# regression output function
home_away_heterogeneity_test <- function(PlayerName, data, PlayerTeam, seed = 6392) {
  
  data <- remove_listings_that_didnt_sell(data)
  
  set.seed(seed)
  
  # getting injured and uninjured matchupIDs, in random order, separating home and away
  injured_home <- sample(unique(data$MatchupID[data$Superstar == 1 & data$HomeTeam == PlayerTeam]))
  injured_away <- sample(unique(data$MatchupID[data$Superstar == 1 & data$AwayTeam == PlayerTeam]))
  uninjured <- sample(unique(data$MatchupID[data$Superstar != 1]))
  
  if (length(injured_home) == 0 | length(injured_away) == 0) {
    return(list(home = c(PlayerName, NA, NA), away = c(PlayerName, NA, NA)))
  }
  
  # creating groups for both home and away
  groups_home <- list()
  for (mid in injured_home) {
    if (length(unique((data %>% 
                       filter(MatchupID == mid & Superstar == 1 & Delta_Round == 0))$HoursToGame)) > 0) {
      # if matchupID corresponds to a game which will work within the treatment time function
      groups_home[[length(groups_home) + 1]] <- c(mid)
    }
    # if matchupID is for a game with some data or scrape issue then we omit it from analysis
  }
  
  groups_away <- list()
  for (mid in injured_away) {
    if (length(unique((data %>% 
                       filter(MatchupID == mid & Superstar == 1 & Delta_Round == 0))$HoursToGame)) > 0) {
      # if matchupID corresponds to a game which will work within the treatment time function
      groups_away[[length(groups_away) + 1]] <- c(mid)
    }
    # if matchupID is for a game with some data or scrape issue then we omit it from analysis
  }
  
  # adding untreated matchups to the groups for home
  i <- 1
  for(n in 1:length(uninjured)) {
    groups_home[[i]] <- c(groups_home[[i]], uninjured[n])
    
    if (i == length(groups_home)) {
      i <- 1
    } else {
      i <- i + 1
    }
  }
  
  # doing the same for away
  i <- 1
  for(n in 1:length(uninjured)) {
    groups_away[[i]] <- c(groups_away[[i]], uninjured[n])
    
    if (i == length(groups_away)) {
      i <- 1
    } else {
      i <- i + 1
    }
  }
  
  print(".  Created groups, using following Matchup Dates")
  
  # creating a dataframe based on each of these groups
  create_temp_frame <- function(group) {
    # print(group)
    
    # find matchup date for the treated matchup in this group
    MatchupDate <- unique((data %>% filter(MatchupID == group[1]))$MatchupDate)
    print(paste0(".    ", MatchupDate))
    
    # create a table with only rows corresponding to matchups in this group
    concat_table <- data %>% filter(MatchupID %in% group)
    
    # return the result of the treatment time function on this concatenated table
    return(create_timetreatment_col(concat_table, MatchupDate))
    
  }
  
  # creating a table for each group with the superstar matchup as the treatment game 
  # used to calculate the time treatment column and the rest of the games as the control for that game.
  # "tables" will be a list of individual tables that we will then concatenate
  tables_home <- lapply(groups_home, create_temp_frame)
  tables_away <- lapply(groups_away, create_temp_frame)
  
  print(".  Created tables from groups")
  
  # concatenating each table in tables_home
  master_home <- tables_home[[1]]
  if (length(tables_home) > 1) {
    for (i in 2:length(tables_home)) {
      master_home <- rbind(master_home, tables_home[[i]])
    }  
  }
  
  # concatenating each table in tables_away
  master_away <- tables_away[[1]]
  if (length(tables_away) > 1) {
    for (i in 2:length(tables_away)) {
      master_away <- rbind(master_away, tables_away[[i]])
    }  
  }
  
  print(".  Concatenated master tables")
  
  # creating the Pre/Post variable
  master_home$Post <- as.numeric(as.character(master_home$TimeTreatment)) > 0
  master_away$Post <- as.numeric(as.character(master_away$TimeTreatment)) > 0
  
  print(".  Created heterogeneity variables")
  
  # aggregating prices to section level
  master_home_agg <- master_home %>%
    group_by(SectionID, ScrapeDate, ScrapeTime, ScrapeID, Superstar, MatchupID, 
             TimeTreatment, Post, HoursToGame) %>% #Home
    summarize(MeanListingPrice = weighted.mean(log(ListingPrice), Quantity), SumQuantity = sum(Quantity))
  master_away_agg <- master_away %>%
    group_by(SectionID, ScrapeDate, ScrapeTime, ScrapeID, Superstar, MatchupID, 
             TimeTreatment, Post, HoursToGame) %>% #Home
    summarize(MeanListingPrice = weighted.mean(log(ListingPrice), Quantity), SumQuantity = sum(Quantity))
  
  
  ### RUNNING THE REGRESSIONS
  
  ## Home
  print("Running Home/Away Regression for Home")
  start <- Sys.time()
  HomeOutput <- felm(MeanListingPrice ~  Post*Superstar | #Home
                       MatchupID + SectionID + HoursToGame  |
                       0 | MatchupID, data=master_home_agg)
  end <- Sys.time()
  print(paste0(".  Regression completed in ",
               round(as.numeric(difftime(end, start, units = "mins")), 2), " minutes"))
  HomeCoefData <- as.data.frame(cbind(PlayerName, HomeOutput$coefficients[3,], HomeOutput$cse[3]))
  colnames(HomeCoefData) <- c("Player", "Coefficient", "StandardError")
  rownames(HomeCoefData) <- NULL
  
  ## Away
  print("Running Home/Away Regression for Away")
  start <- Sys.time()
  AwayOutput <- felm(MeanListingPrice ~  Post*Superstar | #Home
                       MatchupID + SectionID + HoursToGame  |
                       0 | MatchupID, data=master_away_agg)
  end <- Sys.time()
  print(paste0(".  Regression completed in ",
               round(as.numeric(difftime(end, start, units = "mins")), 2), " minutes"))
  AwayCoefData <- as.data.frame(cbind(PlayerName, AwayOutput$coefficients[3,], AwayOutput$cse[3]))
  colnames(AwayCoefData) <- c("Player", "Coefficient", "StandardError")
  rownames(AwayCoefData) <- NULL
  
  return(list(home = HomeCoefData, away = AwayCoefData))
}

set.seed(8394)
seeds <- ceiling(runif(3, 1000, 9999))

home_outputs <- data.frame()
away_outputs <- data.frame()

##Setting list of players with paired teams to run aggregate regressions for here

players <- c("Stephen Curry")

teams <- c("Golden State Warriors")

#Identifying players exhibiting parallel pre-trends
pretrend <- c("Stephen Curry")

player_indices <- players %in% pretrend
teams2 <- teams[player_indices]
players2 <- players[player_indices]

for (i in 1:length(teams2)) { 
  team <- teams2[i]
  player <- players2[i]
  print(toupper(player))
  
  data_player_clean <- data_player_clean
  
  for (seed in seeds) {
    results <- home_away_heterogeneity_test(player, data_player_clean, team, seed)
    home_outputs <- rbind(home_outputs, results$home)
    away_outputs <- rbind(away_outputs, results$away)
  }
}


# saving the coefficients to an external file
save(home_outputs,away_outputs, file = "D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\home_away_results_StephenCurry.RData")

# loading in the coefficients from the external file
load("D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\home_away_results_StephenCurry.RData")

# fixing data types
home_outputs$Coefficient <- as.numeric(as.character(home_outputs$Coefficient))
home_outputs$StandardError <- as.numeric(as.character(home_outputs$StandardError))

away_outputs$Coefficient <- as.numeric(as.character(away_outputs$Coefficient))
away_outputs$StandardError <- as.numeric(as.character(away_outputs$StandardError))

# taking the mean of the coefficients/standard errors from three (different counterfactual pairing) regressions
home_agg <- home_outputs %>%
  group_by(Player) %>%
  summarize(Coef = mean(Coefficient), SE = mean(StandardError)) %>%
  ungroup() %>%
  arrange(Player)
away_agg <- away_outputs %>%
  group_by(Player) %>%
  summarize(Coef = mean(Coefficient), SE = mean(StandardError)) %>%
  ungroup() %>%
  arrange(Player)
homeaway <- cbind(home_agg, away_agg$Coef, away_agg$SE)
colnames(homeaway) <- c("Player", "HomeCoef", "HomeSE", "AwayCoef", "AwaySE")
cols <- c("Home"="blue", "Away"="red")

# plotting the results
homeawayplot <- homeaway %>%
  ggplot() +
  geom_point(aes(x = HomeCoef, y = Player, shape = "Away", color = "Home")) +
  geom_errorbarh(aes(xmin = HomeCoef - 1.96*HomeSE, xmax = HomeCoef + 1.96*HomeSE, x = HomeCoef, y = Player, color = "Home"), height = 0.2) +
  geom_point(aes(x = AwayCoef, y = Player, shape = "Home", color = "Away")) +
  geom_errorbarh(aes(xmin = AwayCoef - 1.96*AwaySE, xmax = AwayCoef + 1.96*AwaySE, x = AwayCoef, y = Player, color = "Away"), height = 0.2) +
  geom_vline(aes(xintercept = 0), color = "black") +
  scale_y_discrete(limits = rev(homeaway_levels$Player)) +
  scale_x_continuous(labels = scales::percent, breaks = c(-.40, -.30, -.20, -.10, 0, .10)) +
  theme_bw() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0))) +
  labs(x = "Diff-In-Diff Estimator", y = "Player") +
  scale_color_manual(name = "Type", labels = c("Home", "Away"), values = cols) +
  scale_shape_manual(name = "Type", labels = c("Home", "Away"), values = c(8, 15))


path <- "D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\home_away_StephenCurry.png"
ggsave(path, width=6)


