# load in the data
load("D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\StephenCurryData.RData")

library(tidyr)
library(dplyr) 
library(ggplot2)
library(lubridate)# package to handle time and date variables
library(lfe)
library(data.table)
library(ggpubr)
library(egg)

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
    summarise(lastTimestamp = min(HoursToGame))
  badListings <- listings$ListingID[listings$lastTimestamp <= 1.0]
  
  return(table %>%
           filter(!(ListingID %in% badListings)))
}


# regression output function
get_regression_output <- function(data, PlayerName, seed = 6392) {
  
  set.seed(seed)
  
  # getting injured and uninjured matchupIDs, in random order
  injured <- sample(unique(data$MatchupID[data$Superstar == 1]))
  uninjured <- sample(unique(data$MatchupID[data$Superstar != 1]))
  
  # creating groups which will lead to specific dataframes
  groups <- list()
  for (mid in injured) {
    if (length(unique((data %>% 
                       filter(MatchupID == mid & Superstar == 1 & Delta_Round == 0))$HoursToGame)) > 0) {
      # if matchupID corresponds to a game which will work within the treatment time function
      groups[[length(groups) + 1]] <- c(mid)
    }
    # if matchupID is for a game with some data or scrape issue then we omit it from analysis
  }
  
  # adding untreated matchups to these groups
  i <- 1
  for(n in 1:length(uninjured)) {
    groups[[i]] <- c(groups[[i]], uninjured[n])
    
    if (i == length(groups)) {
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
  tables <- lapply(groups, create_temp_frame)
  
  print(".  Created tables from groups")
  
  # concatenating each table in tables
  master <- tables[[1]]
  if (length(tables) > 1) {
    for (i in 2:length(tables)) {
      master <- rbind(master, tables[[i]])
    }  
  }
  
  print(".  Concatenated master table")
  
  # aggregating prices to section level
  master_agg <- master %>%
    group_by(SectionID, ScrapeDate, ScrapeTime, ScrapeID, Superstar, MatchupID, 
             TimeTreatment, HoursToGame) %>%
    summarize(MeanListingPrice = weighted.mean(log(ListingPrice), Quantity), SumQuantity = sum(Quantity))
  
  master_agg$Post <- as.numeric(as.character(master_agg$TimeTreatment)) > 0
  
  ## running the regression
  start <- Sys.time()
  # m+s+d+t
  output <- felm(MeanListingPrice ~  Post*Superstar | 
                   MatchupID + SectionID + HoursToGame | # ScrapeDate + ScrapeTime
                   0 | MatchupID, data=master_agg)
  
  end <- Sys.time()
  print(paste0(".  Regression completed in ", round(as.numeric(difftime(end, start, units = "mins")), 2), " minutes"))
  
  CoefData <- as.data.frame(cbind(PlayerName, output$coefficients[3,], output$cse[3]))
  colnames(CoefData) <- c("Player", "Coefficients", "StandardError")
  rownames(CoefData) <- 1:nrow(CoefData)
  
  return(CoefData)
}

set.seed(8394)
seeds <- ceiling(runif(3, 1000, 9999))

coefs <- data.frame()

##Setting list of players with paired teams to run aggregate regressions for here

players <- c("Stephen Curry")

teams <- c("Golden State Warriors")

##Running regressions

for (player in players) {
  print(paste0("....>> ", player))
  for (seed in seeds) {
    temp <- remove_listings_that_didnt_sell(data_player_clean)
    coefs <- rbind(coefs, get_regression_output(temp, player, seed))
  }
}

coefs$Coefficients <- as.numeric(as.character(coefs$Coefficients))
coefs$StandardError <- as.numeric(as.character(coefs$StandardError))

# saving the coefficients to an external file
save(coefs, file = "D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\original_analysis_coefs_StephenCurry.RData")

# loading in the coefficients from the external file
load("D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\original_analysis_coefs_StephenCurry.RData")

coefs_agg <- coefs %>%
  group_by(Player) %>%
  summarize(Coef = mean(Coefficients), SE = mean(StandardError)) %>%
  arrange(Coef)

# Identifying players exhibiting parallel pre-trends
pretrend <- c("Stephen Curry")

coefs_agg_filtered <- coefs_agg %>% 
  filter(Player %in% pretrend)

# making the plot
coef_plot <- coefs_agg_filtered %>%
  ggplot() +
  geom_point(aes(x = Coef, y = Player), color = "blue") +
  geom_errorbarh(aes(xmin = Coef - 1.96*SE, xmax = Coef + 1.96*SE, y = Player, x = Coef), color = "blue", height = 0.2) +
  geom_vline(aes(xintercept = 0), color = "red") +
  scale_y_discrete(limits = rev(coefs_agg_filtered$Player)) +
  scale_x_continuous(labels=scales::percent, breaks=c(-.20, -.10, 0, .10)) +
  theme_bw() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = -20, b = 0, l = 0))) +
  labs(x = "Diff-in-Diff Estimator", y = "")

path <- "D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\AggregateDID_StephenCurry.png"

ggsave(path, width=6)
