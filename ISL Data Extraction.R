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

### Folder where the Ouput Files will be stored
OUTPUT_FOLDER = "Output"

### Output Files
MATCH_HISTORY = "all_matches"
PLAYERS_LIST = "players_list"
TEAMS = "teams"
TEAMS_IN_SEASON = "players"
MATCH_REPORTS = "report"
TEAM_MANAGER_HISTORY = paste(TEAMS,"_manager_history.csv", sep = "")
TEAM_PROFILE = paste(TEAMS,"_profile.csv", sep = "")
MATCH_INFO = "match_info.csv"

START_URL = 'http://www.worldfootball.net'

### Creating the directory if not present.
dir.create(OUTPUT_FOLDER, showWarnings = FALSE)

# Creating All required functions -----------------------------------------

get_html_table <- function(x, subpage, ext = "", which, header = FALSE){
  #' @author Govindan, Ajai Govind | \email{ajaigovindg@@gmail.com}
  #' 
  #' @title Retrieve HTML table(s) from URL
  #' @description Send a URL request and retrieve a particular table from HTML tables
  #' @details Builds a URL starting with base URL in the format START_URL + subpage + x + ext
  #' 
  #' @param x The second level sub-page within the main URL
  #' @param subpage The first level sub-page within the main URL
  #' @param ext Static text to be added in the URL at the end
  #' @param which An integer vector identifying which tables to return from within the document
  #' @param header Logical value indicating if the HTML table has column labels,  or alternatively a character vector giving the names to use for the resulting columns
  #'
  #' @return Data frame of the specified table from the HTML document

  # Package: RCurl
  # Perform the request and retrieve the response
  if(length(ext)==1){
    the_html <- getURL(paste(START_URL, subpage, x, ext, "", sep = '/'),.opts = list(ssl.verifypeer = FALSE), followlocation = TRUE) 
    tables <- readHTMLTable(the_html, header = header, as.data.frame = TRUE, stringsAsFactors = FALSE, which = which)
  }else{
    the_html <- lapply(ext, function(p_no){getURL(paste(START_URL, subpage, x, p_no, "", sep = '/'),.opts = list(ssl.verifypeer = FALSE), followlocation = TRUE)}) 
    tables <- lapply(the_html, function(html_no){readHTMLTable(html_no, header = header, as.data.frame = TRUE, stringsAsFactors = FALSE, which = which)})
  }

  # Package: XML
  # Extracting data from HTML tables

}

extract_match_teams <- function(x, subpage){
  #' @author Govindan, Ajai Govind | \email{ajaigovindg@@gmail.com}
  #' 
  #' @title Extract match details
  #' @description Extract each match details including team members, goals & match info
  #' @details Extract details of each match - starting line up, substitutes, substitution times, goal time, referees & stadium
  #' 
  #' @param x A list
  #' @param subpage The first level sub-page within the main URL
  #'
  #' @return Data frame of the specified table from the HTML document
  cat(paste(paste(x[1],paste(START_URL, subpage, x[4], "", sep ="/"),sep=": "),"\n",sep=""))
  
  
  # Package: xml2
  html_doc <- read_html(paste(START_URL, subpage, x[4], "", sep ="/"))
  
  #Extract entire team information including available substitutes per match
  html_doc %>%
    html_nodes("table") %>%
    .[[6]] %>% html_children() %>% 
    sapply(extract_table, info = 3) -> Team1
  
  html_doc %>%
    html_nodes("table") %>%
    .[[7]] %>% html_children() %>% 
    sapply(extract_table, info = 3) -> Team2
  
  #Extract match information like stadium, referee etc.  
  if(x[5] == 1 | length(Team1) == 1){
    html_doc %>%
      html_nodes("table") %>%
      .[[10]] %>%
      html_table(fill = TRUE) %>%
      select(X3) %>% transpose() -> match_info
  } else{
    html_doc %>%
      html_nodes("table") %>%
      .[[9]] %>%
      html_table(fill = TRUE) %>%
      select(X3) %>% transpose() -> match_info
  }
  
  if(ncol(match_info) == 5){
    match_info = match_info
  } else if(ncol(match_info) == 3){
    match_info <- cbind(match_info, Asst_Referee_1 = NA, Asst_Referee_2=NA)
  } else {
    match_info <- cbind(match_info[1], Attendance = NA, match_info[2], Asst_Referee_1 = NA, Asst_Referee_2=NA)
  }
  
  names(match_info) = c("Stadium", "Attendance", "Referee", "Asst_Referee_1", "Asst_Referee_2")
  
  if(length(Team1) == 1){
    html_doc %>%
      html_nodes("table") %>%
      .[[7]] %>% html_children() %>% 
      sapply(extract_table, info = 3)  -> Team1
    
    html_doc %>%
      html_nodes("table") %>%
      .[[8]] %>% html_children() %>% 
      sapply(extract_table, info = 3)  -> Team2
  }
  
  Team1 %>% match_report_post_process() %>% cbind(x[2], stringsAsFactors = FALSE) -> Team1
  Team2 %>% match_report_post_process() %>% cbind(x[3], stringsAsFactors = FALSE) -> Team2
  
  report_col_names = c("Jersey_No","Player","Card_Time", "Card_Type", "Substitution_Time", "P_S", "Team_Code")
  
  names(Team1) = report_col_names
  names(Team2) = report_col_names
  
  df <- rbind(Team1, Team2)
  
  #Extract information on player and time of goal scoring
  html_doc %>% 
    html_nodes("table") %>%
    .[[4]] %>%
    html_table(fill = TRUE) %>% filter(!X1 == 'goals') %>% 
    separate(col = X2, into = c("Player_Goal_Time", "Support"), extra = "merge", sep = "\\.") -> goals
  
  if(!is.na(goals$Support)){
    goals <- select(goals, Player_Goal_Time) %>% separate(col = 1, into = c("Player", "Goal_Time"), sep = -3)
    goals$Goal_Time <- sprintf("%s'", goals$Goal_Time)
    
    df <- df %>% left_join(goals, by = "Player") %>% list(match_info)
    
  } else{
    goals <- NA
    
    df <- df %>% list(match_info)
  }

  return(df)
}

data_pre_process <- function(html_list, list_names, col_names = FALSE, idcol){
  #' @author Govindan, Ajai Govind | \email{ajaigovindg@@gmail.com}
  #' 
  #' @title Pre-process tables
  #' @description Pre-processing for HTML tables extracted
  #' @details Apply column names and combine multiple tables into 1 table with relevant index column added
  #' 
  #' @param html_list List of all the tables retrieved from the HTML page
  #' @param list_names List of table names for each table in the HTML page
  #' @param col_names A vector of column names for the tables
  #' @param idcol Specify the name of the index column, with individual table names, when binding the tables together
  #'
  #' @return A combined data frame of all the tables from the HTML
  
  names(html_list) <- list_names # Specify individual table names in the list
  html_list <- lapply(html_list,as.data.frame, stringsAsFactors = FALSE) # Convert each table to a data frame
  if(length(col_names)>1){
    html_list <- lapply(html_list, setNames, col_names) # Set column names for each table
  }
  
  
  # Package: data.table
  # Make one data.table from a list of many
  df <- rbindlist(html_list, idcol = idcol)
  
  return(df)
}

fix.names <- function(x) {
  colnames(x) <- make.names(colnames(x), unique = TRUE)
  x
}

extract_table <- function(tr, info) {
  scope <- tr %>% html_children()
  
  if (info == 1) {
    c(
      scope[2] %>% html_node("table > tr > td:nth-child(1) > a > img") %>% html_attr("title"),
      scope[3] %>% html_text(),
      scope[4:5] %>% html_node("img") %>% html_attr("alt"),
      scope[6:(length(scope) - 1)] %>% html_text(),
      scope[length(scope)] %>% html_text(),
      scope[length(scope)] %>% html_node("span") %>% html_attr("title")
    )
  } else if (info == 2) {
    c(
      scope[1] %>% html_text(),
      scope[2] %>% html_text(),
      scope[2] %>% html_node("a") %>% html_attr("href"),
      scope[3] %>% html_node("img") %>% html_attr("alt"),
      scope[4] %>% html_text()
    )
  }else if(info == 3){
    if(length(scope) == 3){
      list(
        scope[1] %>% html_text(),
        scope[2] %>% html_text(),
        scope[2] %>% html_node("img") %>% html_attr("alt"),
        scope[3] %>% html_text()
      )}else{
        list(html_text(scope[1]))
      }
  }
}

match_report_post_process = function(l){
  l %>% lapply(function(x) {
    if (length(x) == 4) {
      setNames(x,
               nm = c(
                 "Jersey_No",
                 "Player_Name",
                 "Card_Type",
                 "Substitution_Time"
               ))
    } else{
      setNames(x, nm = c("Jersey_No"))
    }
  }) %>% rbindlist(fill = TRUE) %>% mutate(
    Player_Name = gsub("\n", ";", gsub("\t", "", Player_Name)),
    P_S = ifelse(Jersey_No == "Substitutes", "Substitute", NA)
  ) %>%
    separate(col = Player_Name,
             into = c("Ignore","Player", "Card_Time"),
             sep = ";") %>% select(c(-2))%>%
    do(setDT(.)[, P_S := na.locf(P_S, na.rm = FALSE)]) %>%
    mutate(P_S = ifelse(is.na(P_S), "Player", P_S)) %>%
    filter(!Jersey_No == "Substitutes")
}

# Archived functions --------------------------------------------------
# getHrefs <- function(node, encoding) {  
#   x <- xmlChildren(node)$a 
#   if (!is.null(x)) paste0("http://", parseURI(url)$server, xmlGetAttr(x, "href"), " | ", xmlValue(x) ) else xmlValue(xmlChildren(node)$text) 
# }

# extract_tr <- function(tr){
#   scope <- tr %>% html_children()
#   c(scope[2] %>% html_node("table > tr > td:nth-child(1) > a > img") %>% html_attr("title"),
#     scope[3] %>% html_text(),
#     scope[4:5] %>% html_node("img") %>% html_attr("alt"),
#     scope[6:(length(scope)-1)] %>% html_text(),
#     scope[length(scope)] %>% html_text(),
#     scope[length(scope)] %>% html_node("span") %>% html_attr("title"))
# }
# 
# extract_team_manager_history = function(tr){
#   scope <- tr %>% html_children()
#   c(scope[1] %>% html_text(),
#     scope[2] %>% html_text(),
#     scope[2] %>% html_node("a") %>% html_attr("href"),
#     scope[3] %>% html_node("img") %>% html_attr("alt"),
#     scope[4] %>% html_text())
# }

# Creating Match History (All_Matches.csv) --------------------------------------------------
#List of all links
match_seasons = c("ind-indian-super-league-2014"
                  ,"ind-indian-super-league-2014-playoffs"
                  ,"ind-indian-super-league-2015"
                  ,"ind-indian-super-league-2015-playoffs"
                  ,"ind-indian-super-league-2016"
                  ,"ind-indian-super-league-2016-playoffs"
                  ,"ind-indian-super-league-2017-2018"
                  ,"ind-indian-super-league-2017-2018-playoffs")
seasons <- c("2014","2014 playoffs","2015","2015 playoffs","2016","2016 playoffs","2017/2018","2017/2018 playoffs")

match_history_col_names = c("Date","Time","Team1","vs","Team2","Result","V7","V8")

team_codes <- data.frame(Team_Code = c("ATK","ATK","CHN","DEL","PUN","GOA","NEU","KER","MUM","BGL","JSD"),
                         Team_Name = c("Atlético de Kolkata","ATK","Chennaiyin FC", "Delhi Dynamos FC", "FC Pune City", "FC Goa", "NorthEast United FC","Kerala Blasters", "Mumbai City FC", "Bengaluru FC", "Jamshedpur FC"),
                         stringsAsFactors = FALSE)

round_lookup <- read_csv("round_lookup.csv", col_names = TRUE, col_types = "ccc")

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
names(match_history) <- c("Season", "Team1_Name", "Team2_Name", "Date_Time", "Round", "Playoffs", "PSO", "AET","Team1_Code", "Team2_Code", "HT_Team1_Goals", "HT_Team2_Goals", "FT_Team1_Goals", "FT_Team2_Goals", "ET_Team1_Goals", "ET_Team2_Goals", "PS_Team1_Goals", "PS_Team2_Goals")
match_history <- data.table(match_history)
match_history[,MatchID := paste("S", str_pad(.GRP, width=2, side="left", pad="0"), "M", str_pad(seq_len(.N), width=3, side="left", pad="0"), sep = ""), by = Season]

match_history = match_history[,c(1,19,5,6,4,7,8,9,2,11,13,15,17,10,3,12,14,16,18)]

# Teams in each Season (Season_Teams.csv) --------------------------------------------------
season_teams_col_names <- c("Team_Logo", "Team_Name", "Country", "Profile", "Matches", "Squad", "Appearances", "Official Homepage")

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
teams <- c("Bengaluru FC", "Chennaiyin FC", "FC Goa", "FC Pune City", "Jamshedpur FC", "Kerala Blasters", "Mumbai City FC", "Delhi Dynamos FC", "ATK", "NorthEast United FC")
team_manager_history_col_names = c("Period", "Manager", "Country", "Born")

team_manager_html <- lapply(paste(START_URL, TEAMS, paste(gsub(" ","-",tolower(teams)),"/9/",sep=""),sep="/"), function(x){read_html(x)})

team_manager_history_tables = lapply(team_manager_html, function(x) {
  x %>% html_nodes("div.data > table.standard_tabelle") %>% .[[1]] %>% html_nodes("tr:not(:first-child)") %>% sapply(extract_table, info = 2) %>% t()%>% as.data.frame(stringsAsFactors = FALSE)
})

team_manager_history_tables = setNames(team_manager_history_tables, teams)

team_manager_history = team_manager_history_tables %>% rbindlist(fill = TRUE, idcol = "team") %>% separate(col = "V1", into = c("period.start","period.end"), sep = " - ")

colnames(team_manager_history) = c("team","period.start","period.end", "name","profile.link","country","dob")

team_manager_profile_links = team_manager_history[,c(1,5)]
team_manager_history = team_manager_history[,-5]
team_manager_history$dob = dmy(team_manager_history$dob)
team_manager_history$period.start = dmy(team_manager_history$period.start)
team_manager_history$period.end = dmy(team_manager_history$period.end)

# Team Profile (Teams_Profile.csv) ----------------------------------------------------
teams_profile_col_names = c("Column_Names", "Values")

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
match_report = match_report_tables %>% rbindlist(fill = TRUE, idcol = "MatchID") %>% apply(2, trimws) %>% as.data.frame()

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

# Variable definitions
tm_codes <- c("41030", "45299", "45080", "45321", "45275", "60816", "45277", "45274", "45276")
tm_teams <- c("Bengaluru FC", "Chennaiyin FC", "Delhi Dynamos FC", "FC Goa", "FC Pune City", "Jamshedpur FC","Kerala Blasters FC","Mumbai City FC","NorthEast United FC")
tm_df <- data.frame(team_codes = tm_codes, team_names = tm_teams)
season_years <- c("2013","2014","2015","2016","2017","2018")
tm_START_URL = "https://www.transfermarkt.com"

tm_links <- sapply(season_years, function(y){apply(tm_df, 1, function(x){paste(tm_START_URL,gsub(" ","-",tolower(x[2])),"kader/verein",x[1],"saison_id", y, "plus/1",sep="/")})})
html_doc = apply(tm_links, 2, function(x){lapply(x, function(y){read_html(y)})})

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

names(tm_transfer) = c("season","team", "player","dob", "age","nationality","current.club","height.cm","foot","joined","contract.start","contract.end","curr.market.value.EUR","prev.market.value.EUR")

season_lookup = data.frame(seasons = c("S01","S02","S03","S04"), season = c("2014","2015","2016","2017/18"))

unique_players = data.frame()
for (s in unique(substring(match_report$MatchID, 1, 3))) {
  for (team in unique(match_report$Team_Code)) {
    unique_players = match_report %>% mutate(season = substring(MatchID, 1, 3)) %>% filter((Team_Code == team) &
                                                                                             (season == s)) %>% select(Team_Code, seasons = "season", Jersey_No, player = Player) %>% distinct() %>% left_join(season_lookup, by = "seasons") %>% select(-c("seasons")) %>% bind_rows(unique_players)
  }
}

player_bio = player_bio %>% left_join(unique_players, by = c("season", "Team_Code", "player"))

nas = player_bio[is.na(player_bio$Jersey_No),]

# Next Steps

# Corrections -----------------------------
# Player Bio:
# Add place of birth from the player profile page
# Add jersey_no from match_report ----Completed
# Correct season column ----Completed

# Match Report
# Correct Goal_Time information

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