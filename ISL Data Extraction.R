start = Sys.time()

# Loading the required packages -----------------------------------------
library(XML)
library(httr)
library(RCurl)
library(rlist)
library(zoo)
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(splitstackshape)
library(stringr)
library(rvest)
library(reshape2)
library(docstring)
library(roxygen2)
library(stats)
library(lubridate)

# Variable Definitions -----------------------------------------
### Set Working Directory to the folder with the R script
WORKING_DIRECTORY = getwd()
setwd(WORKING_DIRECTORY)

# Creating all required variables and lookup tables -----------------------------------------
source("Variables_and_Lookups.R")

# Creating all required functions -----------------------------------------
source("ISL_Functions.R")

# Creating Match History (All_Matches.csv) --------------------------------------------------
# Extract all html pages for Match History
match_history_htmls <- lapply(match_seasons, function(x){get_html_table(x, subpage = MATCH_HISTORY, which = 2)})

#Pre Processing for Match History data
match_history_tables <- data_pre_process(match_history_htmls, seasons, match_history_col_names, 'Season')

#Post Processing for Match History data
match_history_tables %>% separate(col = Season, into = c("Season","Playoff"),sep = " ", convert = TRUE) %>%
  mutate(Round_temp = ifelse(is.na(Team1),Date,NA)) %>%
  group_by(Season) %>%
  do(setDT(.)[,Round_temp := na.locf(na.locf(Round_temp, na.rm=FALSE), fromLast=TRUE)]) %>% 
  filter(!is.na(Team1)) %>% 
  mutate(Date = ifelse(Date == "",NA,Date)) %>% 
  #  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>% 
  #  mutate(DateTime = strptime(paste(Date, Time, sep = " "), format = "%Y-%m-%d %H:%M")) %>% 
  do(setDT(.)[,Date := na.locf(na.locf(Date, na.rm=FALSE), fromLast=TRUE)]) %>% 
  mutate(DateTime = paste(Date, Time, sep = " ")) %>% 
  left_join(round_lookup,by = "Round_temp") %>% 
  select(-c(Playoff,vs,V7,V8,Round_temp, Date, Time)) %>% 
  mutate(Result = gsub("\\(|\\)|,", "",Result))   %>% 
  mutate(PSO = ifelse(str_sub(Result,-3,-1) == 'pso',1,0)) %>% 
  mutate(AET = ifelse(str_sub(Result,-3,-1) == 'aet',1,0)) %>% 
  mutate(Result = ifelse(str_sub(Result,-3,-1) %in% c('pso','aet'),str_sub(Result,1,str_length(Result)-4),Result)) %>% 
  mutate(Result = paste(str_sub(Result,5), str_sub(Result,1,3), sep = ' ')) %>%
  separate(col = Result, into = c("HT","FT"), sep = " ", extra = "merge") %>% 
  separate(col = FT, into = c("FT","ET"), sep = " ", extra = "merge") %>% 
  separate(col = ET, into = c("ET","PS"), sep = " ", extra = "merge") %>% 
  left_join(team_codes, by = c("Team1" = "Team_Name")) %>% 
  rename(Team1_Code = Team_Code) %>% 
  left_join(team_codes, by = c("Team2" = "Team_Name")) %>% 
  rename(Team2_Code = Team_Code) -> match_history

match_history[[8]] <- as.POSIXct(match_history[[8]], format="%d/%m/%Y %H:%M")
match_history <- cSplit(indt = match_history, splitCols = c("HT", "FT", "ET", "PS"), sep = ":")
names(match_history) <- mh_col_names
match_history <- data.table(match_history)
match_history[,MatchID := paste("S", str_pad(.GRP, width=2, side="left", pad="0"), "M", str_pad(seq_len(.N), width=3, side="left", pad="0"), sep = ""), by = Season]

match_history = match_history[,c(1,19,5,6,4,7,8,9,2,11,13,15,17,10,3,12,14,16,18)]

# Teams in each Season (Season_Teams.csv) --------------------------------------------------
# Extract all html pages for Teams in each season
season_teams_htmls <- lapply(match_seasons, function(x){get_html_table(x, subpage = TEAMS_IN_SEASON, which = 2)})

#Pre-processing for Teams in each season data
season_teams_tables <- data_pre_process(season_teams_htmls, seasons, season_teams_col_names, 'Season')

#Post-processing for Teams in each season data
season_teams_tables %>% 
  select(Season, Team_Name) %>%
  separate(col = Season, into = c("Season","Playoff"),sep = " ") %>% 
  replace_na(list(Season = "Unknown", Playoff = "League", Team_Name = "Unknown"))-> season_teams

League <- split(season_teams, with(season_teams,interaction(Playoff)), drop = TRUE)[[1]][,-2]
Playoffs <- split(season_teams, with(season_teams,interaction(Playoff)), drop = TRUE)[[2]]

League %>% 
  left_join(Playoffs,by = c("Season","Team_Name")) %>% 
  mutate(Playoff = ifelse(is.na(Playoff),0,1)) %>% 
  left_join(team_codes, by = "Team_Name") -> season_teams

# Team Manager History (Teams_Manager_History.csv) ----------------------------------------------------
# Extract all html pages for Team Manager History
team_manager_html <- lapply(paste(START_URL, TEAMS, paste(gsub(" ","-",tolower(teams)),"/9/",sep=""),sep="/"), function(x){read_html(x)})

# Extract the table information from the html pages
team_manager_history_tables = team_manager_html %>% lapply(function(x) {
  x %>% html_nodes("div.data > table.standard_tabelle") %>% .[[1]] %>% html_nodes("tr:not(:first-child)") %>% sapply(extract_table, info = 2) %>% t()%>% as.data.frame(stringsAsFactors = FALSE)
}) %>% setNames(teams)

team_manager_history = team_manager_history_tables %>% rbindlist(fill = TRUE, idcol = "team") %>% separate(col = "V1", into = c("period.start","period.end"), sep = " - ")

colnames(team_manager_history) = tmh_col_names

team_manager_profile_links = team_manager_history[,c(1,5)]
team_manager_history = team_manager_history[,-5]
team_manager_history$dob = dmy(team_manager_history$dob)
team_manager_history$period.start = dmy(team_manager_history$period.start)
team_manager_history$period.end = dmy(team_manager_history$period.end)

# Team Profile (Teams_Profile.csv) ----------------------------------------------------
# Extract all the htmls for team profiles
teams_profile_htmls <- lapply(paste(gsub(" ","-",tolower(teams)),"/1/",sep=""), function(x){get_html_table(x, subpage = TEAMS, which = 1)})

#Pre-processing for Team Profile data
team_profile_tables <- data_pre_process(teams_profile_htmls, teams, teams_profile_col_names, 'Team')

#Post-processing for Team Profile data
team_profile_tables$Column_Names <- str_to_title(gsub(":","",team_profile_tables$Column_Names))
team_profile_tables %>% spread(key = Column_Names, value = Values) %>% 
  select(-c(1:2)) %>% 
  mutate(Stadium_Capacity = as.integer(str_remove(gsub('\\.','',str_sub(Stadium,-12,-1)),' Ranks')), Stadium = trimws(str_sub(Stadium,1,str_length(Stadium)-12))) %>% 
  left_join(team_codes, by = c("Team" = "Team_Name")) %>% 
  select(c(8,6,1:5,7))-> team_profile

team_profile[which(team_profile$Team == 'Jamshedpur FC'),]$Homepage = 'https://www.fcjamshedpur.com/'
team_profile[which(team_profile$Team == 'Kerala Blasters'),]$Nickname = 'Manjapada'
team_profile[which(team_profile$Team == 'Kerala Blasters'),]$Stadium = 'Jawaharlal Nehru International Stadium / Kaloor Stadium'
team_profile[which(team_profile$Team == 'Chennaiyin FC'),]$Stadium = 'Jawaharlal Nehru Stadium / Marina Arena'
team_profile[which(team_profile$Team == 'Chennaiyin FC'),]$Founded = '20/08/2014'
team_profile[which(team_profile$Team == 'FC Pune City'),]$Founded = '13/04/2014'
team_profile[which(team_profile$Team == 'FC Goa'),]$Stadium = 'Jawaharlal Nehru Stadium / Fatorda'

# Match Report (Match_Report.csv) --------------------------------------------------
match_history %>% select(Season, Date_Time, Team1_Code, Team1_Name, Team2_Code, Team2_Name, Round, MatchID, PSO, AET) %>% 
  mutate(links = paste("indian-super-league",Season, ifelse(Round == "Semi-finals","playoffs-halbfinale",ifelse(Round == "Final","playoffs-finale","")),tolower(ifelse(Team1_Name %in% c("Atlético de Kolkata","ATK"),ifelse(Date_Time < as.POSIXct("2018-01-25 00:00:00"),"Atletico de Kolkata","ATK"),Team1_Name)), tolower(ifelse(Team2_Name %in% c("Atlético de Kolkata","ATK"),ifelse(Date_Time < as.POSIXct("2018-01-25 00:00:00"),"Atletico de Kolkata","ATK"),Team2_Name)), sep = "-")) %>% 
  select(MatchID, Team1_Code, Team2_Code, links, PSO, AET) %>% 
  mutate(links = gsub(" |/|--", "-", links)) -> match_links

match_details_tables <- apply(match_links, 1, function(x){extract_match_teams(x, MATCH_REPORTS)})
names(match_details_tables) <- match_links[,1]

match_report_tables = lapply(match_details_tables, function(x){x[[1]]})
match_report = match_report_tables %>% rbindlist(fill = TRUE, idcol = "MatchID") %>% mutate_all(funs(trimws)) %>% as.data.frame() 

# Match Info (Match_Info.csv) --------------------------------------------------
match_info_tables = lapply(match_details_tables, function(x){x[[2]]})

match_info = match_info_tables %>% rbindlist(fill = TRUE, idcol = "MatchID")

# Player Bio (Player_Bio.csv) --------------------------------------------------
player_bio_tables = lapply(match_seasons, function(x){get_html_table(x, subpage = PLAYERS_LIST, ext = lapply(1:6,function(page_no){paste("nach-name/",page_no,"/",sep="")}), header = TRUE, which = 2)})

player_bio = player_bio_tables %>% setNames(seasons) %>% lapply(function(x) {
  lapply(x, fix.names)
}) %>% unlist(recursive = FALSE) %>% rbindlist(idcol = "Season", fill = TRUE) %>%
  select(
    season = "Season",
    player = "Player",
    Team_Name = "Team",
    dob = "born",
    height.cm = "Height",
    position = "Position"
  ) %>%
  mutate(height.cm = str_remove(height.cm, " cm$"),
         season = substr(season, 1, nchar(season) - 1)) %>%
  separate(col = "season", into = c("season", "playoffs"), sep = " ") %>% 
  mutate(playoffs = ifelse(is.na(playoffs),"N","Y")) %>%
  filter(!is.na(player)) %>%
  mutate_all(funs(na_if(., "???"))) %>% 
  left_join(team_codes, by = "Team_Name")

# Converting data columns using lubridate
# strptime(), as.Date() not working in dplyr chained mutate()
player_bio_tables$dob = dmy(player_bio_tables$dob)

# Extracting Transfer/Market values from transfermarkt (All_Transfers.csv) ---------------------------------
# Generating all the webpage links
tm_links <- sapply(season_years, function(y){apply(tm_df, 1, function(x){paste(tm_START_URL,gsub(" ","-",tolower(x[2])),"kader/verein",x[1],"saison_id", y, "plus/1",sep="/")})})

# Extract all the htmls from the links above
html_doc = apply(tm_links, 2, function(x){lapply(x, function(y){read_html(y)})})

# Generate a list of lists with required table information from the htmls
tm_transfer_tables <-
  lapply(html_doc, function(years) {
    lapply(years, function(htmls) {
      htmls %>% html_nodes("#yw1 > table > tbody > tr") %>% sapply(extract_table, info = 1) %>% t() %>% as.data.frame(stringsAsFactors = FALSE)
    })
  })

tm_transfer_tables[["2018"]] = lapply(tm_transfer_tables[["2018"]], function(x) {
  x = cbind(x[, 1:3], NA_character_, x[, 4:NCOL(x)])
  setNames(x,
           c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"))
})

# Post processing and combining all information into 1 table
tm_transfer = tm_transfer_tables %>% lapply(function(years) {
  setNames(years, tm_teams)
}) %>%
  unlist(recursive = FALSE) %>%
  rbindlist(idcol = "season.team", fill = TRUE) %>%
  separate(
    col = "season.team",
    into = c("season", "team"),
    sep = "\\.",
    remove = TRUE
  ) %>%
  separate(
    col = "V2",
    into = c("DOB", "age"),
    sep = "[\\(\\)]",
    convert = TRUE
  ) %>%
  mutate(
    DOB = str_trim(DOB, side = "both"),
    V5 = str_remove_all(V5, "[, m]"),
    V10 = trimws(str_replace(V10, ",", ".")),
    V11 = trimws(str_replace(str_replace_all(
      V11, c(
        "Vorheriger Marktwert: " = "",
        "Previous Market Value: " = ""
      )
    ), ",", "."))
  ) %>%
  mutate(V10 = sub("\\s+$", "", str_replace_all(V10, c(
    " Th. €+" = "K", " Mill. €+" = "M"
  ))),
  V11 = str_replace_all(V11, c(
    " Th. €+" = "K", " Mill. €+" = "M"
  ))) %>%
  #Replace all occurances of empty string, - and N/A with NA
  mutate_all(funs(na_if(., ""))) %>%
  mutate_all(funs(na_if(., "-"))) %>%
  mutate_all(funs(na_if(., "N/A")))

# Converting data columns using lubridate
# strptime(), as.Date() not working in dplyr chained mutate()
tm_transfer$DOB = mdy(tm_transfer$DOB)
tm_transfer$V7 = mdy(tm_transfer$V7)
tm_transfer$V9 = dmy(tm_transfer$V9)

names(tm_transfer) = tm_trans_col_names

unique_players = data.frame()
for (s in unique(substring(match_report$MatchID, 1, 3))) {
  for (team in unique(match_report$Team_Code)) {
    unique_players = match_report %>% mutate(season = substring(MatchID, 1, 3)) %>% filter((Team_Code == team) &
                                                                                             (season == s)) %>% select(Team_Code, seasons = "season", Jersey_No, player = Player) %>% distinct() %>% left_join(season_lookup, by = "seasons") %>% select(-c("seasons")) %>% bind_rows(unique_players)
  }
}

player_bio = player_bio %>% left_join(unique_players, by = c("season", "Team_Code", "player"))

nas = player_bio[is.na(player_bio$Jersey_No),]

# Team Players A to Z (will be used to map country, position and dob to player bio) ---------------------------------
teams_players_atoz_htmls <- lapply(paste(gsub(" ","-",tolower(teams)),"/10/",sep=""), function(x){read_html(paste(START_URL,TEAMS, x, sep = "/"))})

teams_players_atoz_tables = teams_players_atoz_htmls %>% lapply(function(x){html_nodes(x, "div.data > table") %>% .[[1]] %>% html_table() %>% setNames(c("player", "ignore", "country", "position", "dob")) %>% select(c(-2)) %>% filter(nchar(position) != 1)}) %>% setNames(teams)

teams_players_atoz = rbindlist(teams_players_atoz_tables, fill = TRUE, idcol = "Team_Name")

# Team Squads ---------------------------------
# Extract all htmls for team squads
teams_squads_htmls <-
  lapply(gsub(" ", "-", tolower(teams)), function(x) {
    lapply(yrs, function(y) {
      read_html(paste(START_URL, TEAMS, x, y, "2", sep = "/"))
    })
  }) %>% setNames(teams)

teams_squads_tables <-
  lapply(teams_squads_htmls, function(t) {
    lapply(t, function(y) {
      y %>% html_nodes("div.data > table.standard_tabelle") %>% .[[1]] %>% html_table(fill=TRUE, header = FALSE) %>% setNames(ts_col_names)
    }) %>% setNames(ts_seasons) %>% rbindlist(idcol = "season", fill = TRUE)
  }) %>% rbindlist(idcol = "Team_Name", fill = TRUE) %>% 
  do(setDT(.)[,position := na.locf(na.locf(position, na.rm=FALSE), fromLast=TRUE)]) %>% 
  mutate_all(funs(na_if(., ""))) %>% 
  filter(is.na(ignore1)) %>% select(-c(3,6))

teams_squads = teams_squads_tables %>% filter(!(position == "Manager"))
teams_managers = teams_squads_tables %>% filter(position == "Manager") %>% select(-c(3,7))

# Next Steps

# Team Performance / Appearances -----------------------------
appearances_htmls =  lapply(teams, function(t) {
  lapply(match_seasons, function(s) {
    read_html(paste(START_URL, APPEARANCE, gsub(" ", "-", t), s, sep = "/"))
  }) %>% setNames(seasons)
}) %>% setNames(teams)

appearances_tables =  lapply(appearances_htmls, function(t) {
  lapply(t, function(s) {
    s %>% html_nodes(".box > .data > table.standard_tabelle") %>% .[[1]] %>% html_nodes("tr:not(:first-child)") %>% sapply(extract_table, info = 4) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>% mutate_all(funs(trimws))
  }) %>% setNames(seasons)
}) %>% setNames(teams)

appearances = appearances_tables %>% lapply(function(t) {
  rbindlist(t, fill = TRUE, idcol = "season")
}) %>% rbindlist(fill = TRUE, idcol = "Team_Name") %>% setNames(appearances_col_names) %>% mutate_all(funs(na_if(., "-")))


# Corrections -----------------------------
# Player Bio:
# Add place of birth from the player profile page
# Add jersey_no from match_report ----Partial
# Correct season column ----Completed

# Match Report
# Correct Goal_Time information ----Completed

# New Data ------------------------------------
# Transfers Data from worldfootball.net - https://www.worldfootball.net/transfers/ind-indian-super-league-2014/
# Match Attendance (Home & Away) - https://www.worldfootball.net/attendance/ind-indian-super-league-2014/1/
# Referees and Assistants - https://www.worldfootball.net/referees/ind-indian-super-league-2014/1/
# Player Club Careers - https://www.worldfootball.net/player_summary/fikru-teferra/
# Team Manager career history - https://www.worldfootball.net/player_summary/carles-cuadrat/

# Write all relevant outputs as pipe separated CSV files --------------------------------------------------
write.table(team_profile, file.path(OUTPUT_FOLDER,TEAM_PROFILE),sep="|", row.names = FALSE)
write.table(team_manager_history, file.path(OUTPUT_FOLDER,TEAM_MANAGER_HISTORY), sep="|", row.names = FALSE)
write.table(season_teams, file.path(OUTPUT_FOLDER,'Season_Teams.csv'), sep="|", row.names = FALSE)
write.table(match_report, file.path(OUTPUT_FOLDER,paste('match_',MATCH_REPORTS,".csv",sep="")),sep="|", row.names = FALSE)
write.table(match_history, file.path(OUTPUT_FOLDER,paste(MATCH_HISTORY,".csv",sep="")),sep="|", row.names = FALSE)
write.table(match_info, file.path(OUTPUT_FOLDER,MATCH_INFO), sep="|", row.names = FALSE)
write.table(player_bio, file.path(OUTPUT_FOLDER,"Player_Bio.csv"), sep="|", row.names = FALSE)
write.table(tm_transfer, file.path(OUTPUT_FOLDER,"All_Transfers.csv"), sep="|", row.names = FALSE)

end = Sys.time()
elapsed = end-start
elapsed #Time difference of 5.328236 mins