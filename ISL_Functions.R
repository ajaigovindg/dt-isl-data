#' @title Function definitions for ISL data extraction
#' @details Various functions required for the Hero Indian Super League data extraction
#' get_html_table()
#' extract_match_teams()
#' data_pre_process()
#' fix.names()
#' extract_table()
#' match_report_post_process()

get_html_table <- function(x, subpage, ext = "", which, header = FALSE) {
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
    if (length(ext) == 1) {
      the_html <-
        getURL(
          paste(START_URL, subpage, x, ext, "", sep = '/'),
          .opts = list(ssl.verifypeer = FALSE),
          followlocation = TRUE
        )
      tables <-
        readHTMLTable(
          the_html,
          header = header,
          as.data.frame = TRUE,
          stringsAsFactors = FALSE,
          which = which
        )
    } else{
      the_html <-
        lapply(ext, function(p_no) {
          getURL(
            paste(START_URL, subpage, x, p_no, "", sep = '/'),
            .opts = list(ssl.verifypeer = FALSE),
            followlocation = TRUE
          )
        })
      tables <-
        lapply(the_html, function(html_no) {
          readHTMLTable(
            html_no,
            header = header,
            as.data.frame = TRUE,
            stringsAsFactors = FALSE,
            which = which
          )
        })
    }
    
    # Package: XML
    # Extracting data from HTML tables
    
  }

extract_match_teams <- function(x, subpage) {
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
  cat(paste(paste(
    x[1], paste(START_URL, subpage, x[4], "", sep = "/"), sep = ": "
  ), "\n", sep = ""))
  
  
  # Package: xml2
  html_doc <-
    read_html(paste(START_URL, subpage, x[4], "", sep = "/"))
  
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
  if (x[5] == 1 | length(Team1) == 1) {
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
  
  if (ncol(match_info) == 5) {
    match_info = match_info
  } else if (ncol(match_info) == 3) {
    match_info <-
      cbind(match_info,
            Asst_Referee_1 = NA,
            Asst_Referee_2 = NA)
  } else {
    match_info <-
      cbind(
        match_info[1],
        Attendance = NA,
        match_info[2],
        Asst_Referee_1 = NA,
        Asst_Referee_2 = NA
      )
  }
  
  names(match_info) = c("Stadium",
                        "Attendance",
                        "Referee",
                        "Asst_Referee_1",
                        "Asst_Referee_2")
  
  if (length(Team1) == 1) {
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
  
  report_col_names = c(
    "Jersey_No",
    "Player",
    "Card_Time",
    "Card_Type",
    "Substitution_Time",
    "P_S",
    "Team_Code"
  )
  
  names(Team1) = report_col_names
  names(Team2) = report_col_names
  
  df <- rbind(Team1, Team2) %>% mutate_all(funs(trimws))
  
  #Extract information on player and time of goal scoring
  html_doc %>%
    html_nodes("table") %>%
    .[[4]] %>%
    html_table(fill = TRUE) %>% filter(!X1 == 'goals') %>%
    separate(
      col = X2,
      into = c("Player_Goal_Time", "Support"),
      extra = "merge",
      sep = "\\."
    ) -> goals
  
  if (!is.na(goals$Support)) {
    goals <-
      select(goals, Player_Goal_Time) %>% separate(
        col = 1,
        into = c("Player", "Goal_Time"),
        sep = -3
      ) %>% mutate_all(funs(trimws))
    goals$Goal_Time <- sprintf("%s'", goals$Goal_Time)
    
    df <-
      df %>% left_join(goals, by = "Player") %>% list(match_info)
    
  } else{
    goals <- NA
    
    df <- df %>% list(match_info)
  }
  
  return(df)
}

data_pre_process <- function(html_list, list_names, col_names = FALSE, idcol) {
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
    
    names(html_list) <-
      list_names # Specify individual table names in the list
    html_list <-
      lapply(html_list, as.data.frame, stringsAsFactors = FALSE) # Convert each table to a data frame
    if (length(col_names) > 1) {
      html_list <-
        lapply(html_list, setNames, col_names) # Set column names for each table
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
  } else if (info == 3) {
    if (length(scope) == 3) {
      list(
        scope[1] %>% html_text(),
        scope[2] %>% html_text(),
        scope[2] %>% html_node("img") %>% html_attr("alt"),
        scope[3] %>% html_text()
      )
    } else{
      list(html_text(scope[1]))
    }
  }
}

match_report_post_process = function(l) {
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
    separate(
      col = Player_Name,
      into = c("Ignore", "Player", "Card_Time"),
      sep = ";"
    ) %>% select(c(-2)) %>%
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