# load in the data
load("D:\\FacultyData\\Kaplan\\nba-ticket-prices\\_PAPER\\Replication_EconomicInquiry\\StephenCurryData.RData")

library(data.table) # includes function fread() which reads in CSVs quicker than read.csv
library(dplyr) 
library(ggplot2)
library(lubridate)# package to handle time and date variables
library(lfe)


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

# Create list of clean, regression-ready data tables for each player
create_table_list <- function(data_player_clean, PlayerName) {
  
  # Get dates for which player was injured
  treated_matchup_dates <- get_injury_matchups(data_player_clean, PlayerName)
  
  print(".  got treated matchup dates")
  
  # Create table with data for a specific injured game and all appropriate counterfactual games
  create_and_add_table <- function(date, data_player_clean) {
    table <- create_timetreatment_col(data_player_clean, date)
    if (table == 0) {
      return(NA)
    }
    return(list(date, table))
  }
  
  # Apply function to each injured matchup date
  rlist <- lapply(treated_matchup_dates, create_and_add_table, data_player_clean)
  rlist <- rlist[!is.na(rlist)]
  
  return(rlist)
}

# Get dates for matchups where the player is injured
get_injury_matchups <- function(data_player_clean, PlayerName) {
  l <- unique(data_player_clean$MatchupDate[data_player_clean$Player == PlayerName])
  return(l[!is.na(l)])
}

# creates and saves graph from regression output (will be used after regressions are run)
reg_to_graph <- function(output, graphtitle, filename, folder) {
  
  # Create dataframe with coefficients and standard errors
  CoefData <- as.data.frame(cbind(output$coefficients, output$cse))
  colnames(CoefData) <- c("Coefficients", "StandardError")
  CoefData$ci_upper <- CoefData$Coefficients + CoefData$StandardError*1.96
  CoefData$ci_lower <- CoefData$Coefficients - CoefData$StandardError*1.96
  
  # Create omitted variable (minus1) in the dataframes
  m <- matrix(0, ncol = 4, nrow = 1)
  m <- as.data.frame(m)
  colnames(m) <- colnames(CoefData)
  rownames(m) <- "TimeTreatment-1:Superstar1"
  temp4 <- rbind(m,CoefData)
  
  TimeTreatmentMax <- 14
  TimeTreatmentMin <- -14
  
  # Reorder rows of the coefficient data so that it is in right order
  temp4 <- temp4[c(1,(nrow(temp4)-(TimeTreatmentMax-TimeTreatmentMin-1)):nrow(temp4)),]
  temp4 <- temp4[c(2:abs(TimeTreatmentMin), 1, (abs(TimeTreatmentMin)+1):nrow(temp4)),]
  temp4 <- cbind(temp4, index = -14:14)
  
  # Filter out unecessary x-values
  temp4 <- temp4 %>% filter(!is.na(Coefficients))
  
  # Make plot
  graph <- ggplot(temp4) + 
    geom_line(data=temp4, aes(index,Coefficients)) + 
    geom_ribbon(aes(ymin=ci_lower, ymax=ci_upper, x=index, fill = "band"), alpha = 0.3) +
    geom_vline(xintercept=-.5, colour='red') + geom_text(x=2, y=7, label ="Announcement", size=3, angle=0, vjust=-1.4) + 
    geom_hline(yintercept=0, colour='black') + 
    xlab("30-Minute Intervals Since Announcement") + ylab("Proportional Change in Prices") + 
    #ggtitle(graphtitle) +
    theme_bw() + scale_x_discrete(limits=TimeTreatmentMin:TimeTreatmentMax) + 
    theme(legend.position="none", 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          plot.title = element_text(hjust = 0.5, size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  
  # Create directory filepath for graph and save graph
  filepath <- paste0(folder)
  savename <- paste0(filepath, filename)
  ggsave(savename, width=9)
  
  print(paste0(".  Saved ", savename))
  
  return(graph)
} 


## diff-in-diff for log prices with team counterfactual at listing level for all matchups for given player
diff_price_team_listing_all_sold_only <- function(data, PlayerName, seed = 6392,
                                                  folder = "D:/FacultyData/Kaplan/nba-ticket-prices/_PAPER/Replication_EconomicInquiry/", append = "") {
  
  #data <- data_players_Combined[[1]]
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
  
  # Remove listings that didn't sell
  master <- remove_listings_that_didnt_sell(master)
  
  ## running the regression
  start <- Sys.time()
  
  output <- felm(log(ListingPrice) ~  TimeTreatment*Superstar | 
                   MatchupID + SectionID + HoursToGame | 
                   0 | MatchupID, data=master, weights = master$Quantity) 
  
  end <- Sys.time()
  print(paste0(".  Regression completed in ", round(as.numeric(difftime(end, start, units = "mins")), 2), " minutes"))
  
  graphtitle <- paste0("log(Price) Analysis for ", PlayerName, " Absences: Listings That Sold")
  filename <- paste0("graph_", PlayerName, "_dd_logprice_teamcounter_listinglevel", append, ".png")
  
  return(reg_to_graph(output, graphtitle, filename, folder))
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




# performs regressions
perform_price_regressions_sold_only <- function(PlayerName) {
  
  # gets the player data for the player
  data_player_clean <- data_player_clean

  set.seed(8394)
  seeds <- ceiling(runif(3, 1000, 9999))

  for(i in 1:3) {
    print(paste0(i, ": Running SINGLE-variable FE LISTING-level PRICE regression (TEAM counter) for ", PlayerName, " - All Matchups"))
    tryCatch(
      {
        diff_price_team_listing_all_sold_only(data_player_clean,  PlayerName, seed = seeds[i], append = i)
      }, error=function(e){
        print(".  REGRESSION FAILED")
        print(e)
      }
    )
   }
  
}


##Main players to run for (all-star starters/would have been voted in)
perform_price_regressions_sold_only("Stephen Curry")


