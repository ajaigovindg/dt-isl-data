#' @title Variable definitions and mapping/lookup tables for ISL data extraction
#' @details Various variables and lookup tables required for the Hero Indian Super League data extraction

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

# (All_Matches.csv) --------------------------------------------------
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

round_lookup <- read_csv("Input/round_lookup.csv", col_names = TRUE, col_types = "ccc")

mh_col_names <- c("Season", "Team1_Name", "Team2_Name", "Date_Time", "Round", "Playoffs", "PSO", "AET","Team1_Code", "Team2_Code", "HT_Team1_Goals", "HT_Team2_Goals", "FT_Team1_Goals", "FT_Team2_Goals", "ET_Team1_Goals", "ET_Team2_Goals", "PS_Team1_Goals", "PS_Team2_Goals")

# Teams in each Season (Season_Teams.csv) --------------------------------------------------
season_teams_col_names <- c("Team_Logo", "Team_Name", "Country", "Profile", "Matches", "Squad", "Appearances", "Official Homepage")

# Team Manager History (Teams_Manager_History.csv) ----------------------------------------------------
teams <- c("Bengaluru FC", "Chennaiyin FC", "FC Goa", "FC Pune City", "Jamshedpur FC", "Kerala Blasters", "Mumbai City FC", "Delhi Dynamos FC", "ATK", "NorthEast United FC")
team_manager_history_col_names = c("Period", "Manager", "Country", "Born")
tmh_col_names <- c("team","period.start","period.end", "name","profile.link","country","dob")

# Team Profile (Teams_Profile.csv) ----------------------------------------------------
teams_profile_col_names = c("Column_Names", "Values")

# Extracting Transfer/Market values from transfermarkt (All_Transfers.csv) ---------------------------------
tm_codes <- c("41030", "45299", "45080", "45321", "45275", "60816", "45277", "45274", "45276")
tm_teams <- c("Bengaluru FC", "Chennaiyin FC", "Delhi Dynamos FC", "FC Goa", "FC Pune City", "Jamshedpur FC","Kerala Blasters FC","Mumbai City FC","NorthEast United FC")
tm_df <- data.frame(team_codes = tm_codes, team_names = tm_teams)
season_years <- c("2013","2014","2015","2016","2017","2018")
tm_START_URL = "https://www.transfermarkt.com"

season_lookup = data.frame(seasons = c("S01","S02","S03","S04"), season = c("2014","2015","2016","2017/18"))

tm_trans_col_names = c("season","team", "player","dob", "age","nationality","current.club","height.cm","foot","joined","contract.start","contract.end","curr.market.value.EUR","prev.market.value.EUR")

# Team Squads ---------------------------------
yrs = 2014:2020

ts_seasons = c("2013/14","2014/15","2015/16","2016/17","2017/18","2018/19","2019/20")
ts_col_names = c("ignore1","jersey_no","player","ignore2","country","dob","position")
