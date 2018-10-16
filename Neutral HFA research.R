{
  library(geosphere)
  library(readxl)
  library(readr)
  library(httr)
  library(XML)
  library(xlsx)
  library(ggmap)
  deleteParenthetics = function(vector, expressions = c("\\(", "\\[", "\\{"), 
                                delete.space = TRUE){
    # removes EVERYTHING after the opening parentheses/bracket
    for(i in expressions){
      locs = gregexpr(i, vector)
      index = integer(0)
      end.loc = integer(0)
      tryCatch({
        index = which(locs != -1)
        end.loc = as.numeric(locs[index]) - 1
        # print("error not caught")
      }, error = function(e){
        # print("error caught")
        for(j in 1:length(vector)){
          this.loc = locs[[j]][1]
          if(this.loc != -1){
            index <<- append(index, j)
            end.loc <<- append(end.loc, this.loc - 1)
          }
        }
      })
      
      if(delete.space){
        end.loc = end.loc - 1
      }
      vector[index] = substr(vector[index], 1, end.loc)
    }
    return(vector)
  }
  getLatLong = function(city, state){
    index = which((US_cities_database$city == city | 
                     US_cities_database$city_ascii == city) & 
                    US_cities_database$state_name == state)
    latitude = US_cities_database[index, "lat"]
    longitude = US_cities_database[index, "lng"]
    both = as.numeric(c(latitude, longitude))
    names(both) = c("Lat", "Long")
    return(both)
  }
  getDistance = function(city1, state1, city2, state2){
    return(distHaversine(getLatLong(city1, state1)[2:1], 
                         getLatLong(city2, state2)[2:1], r = 3959))
  }
  getDistance = function(coords1, coords2){
    return(distHaversine(coords1[2:1], coords2[2:1], r = 3959))
  }
  getCollegeCoords = function(team){
    team.index = match(team, d1.colleges.data$Team)
    team.coords = d1.colleges.data[team.index, c("Latitude", "Longitude")]
    rownames(team.coords) = NULL
    return(team.coords)
  }
  getCollegeDistance = function(team1, team2){
    return(getDistance(getCollegeCoords(team1), getCollegeCoords(team2)))
  }
  getNcaafSchedule = function(week = wk.ncaaf){
    espn.url = paste("http://www.espn.com/college-football/schedule/_/week/", 
                     week, "/group/90/year/", season, sep = "")
    espn.html = rawToChar(GET(espn.url)$content)
    espn.sched.tables = readHTMLTable(espn.html)
    if(length(espn.sched.tables) == 0){
      stop("No games scheduled")
    }
    {
      table.caption.locs = gregexpr("table-caption", espn.html)[[1]]
      dates = character(length = length(table.caption.locs))
      for(i in 1:length(table.caption.locs)){
        short.string = substr(espn.html, table.caption.locs[i] + 15, 
                              table.caption.locs[i] + 40)
        # comma.loc = gregexpr(",", short.string)[[1]]
        tag.end = gregexpr("<", short.string)[[1]][1]
        dates[i] = substr(short.string, 1, tag.end - 1)
      }
      dates = as.Date(dates, format = "%A, %B %d")
    } # getting the dates
    {
      neutral.home.teams = NULL
      neutral.away.teams = NULL
      neutral.locs = gregexpr('data-home-text="vs"', espn.html)[[1]]
      if(neutral.locs[1] != -1){
        for(i in neutral.locs){
          next.thou.chars = substr(espn.html, i, i + 1000)
          span.loc = gregexpr("<span>", next.thou.chars)[[1]]
          end.span.loc = gregexpr("</span>", next.thou.chars)[[1]]
          if(length(end.span.loc) > length(span.loc) & 
             end.span.loc[1] < span.loc[1]){
            end.span.loc = end.span.loc[2]
          } else {
            end.span.loc = end.span.loc[1]
          }
          span.loc = span.loc[1]
          # after.span = substr(next.thou.chars, span.loc + 6, span.loc + 50)
          team.name = substr(next.thou.chars, span.loc + 6, end.span.loc - 1)
          # game.index = match(team.name, sched.table$Home.Team)
          # if(!is.na(game.index)){
          #   sched.table[game.index, "Neutral"] = 1
          # }
          neutral.home.teams = append(neutral.home.teams, team.name)
          
          {
            prev.thou.chars = substr(espn.html, i - 1000, i)
            smaller.string = substr(
              prev.thou.chars, regexpr("data-is-neutral-site", 
                                       prev.thou.chars)[[1]], 1000)
            span.loc = gregexpr("<span>", smaller.string)[[1]]
            end.span.loc = gregexpr("</span>", smaller.string)[[1]]
            if(length(end.span.loc) > length(span.loc) & 
               end.span.loc[1] < span.loc[1]){
              end.span.loc = end.span.loc[2]
            } else {
              end.span.loc = end.span.loc[1]
            }
            span.loc = span.loc[1]
            team.name = substr(smaller.string, span.loc + 6, end.span.loc - 1)
            neutral.away.teams = append(neutral.away.teams, team.name)
          } # away team
        }
        neutral.home.teams = ESPN_Abbreviations[match(
          neutral.home.teams, ESPN_Abbreviations$ESPN.Name), "Team"]
        neutral.away.teams = ESPN_Abbreviations[match(
          neutral.away.teams, ESPN_Abbreviations$ESPN.Name), "Team"]
        # change espn name to kalin name
      }
      
    } # neutral games
    # future schedules have 7 cols
    # past schedules have 6 cols
    past.schedule = data.frame()
    future.sched = data.frame()
    for(i in 1:length(espn.sched.tables)){
      sched.table = espn.sched.tables[[i]]
      if(ncol(sched.table) == 6){ # past games
        sched.table[, 4:6] = NULL
        names(sched.table) = c("Away", "Home", "Result")
        sched.table$Away.Team = NA
        sched.table$Home.Team = NA
        sched.table$Away.Pts = NA
        sched.table$Home.Pts = NA
        sched.table$Neutral = 0
        sched.table$Away.Abr = NA
        sched.table$Home.Abr = NA
        sched.table$OTimes = 0
        
        
        for(j in 1:2){
          sched.table[, j] = as.character(sched.table[, j])
          ranked.teams = which(regexpr("#", sched.table[, j]) == 1)
          if(length(ranked.teams) > 0){
            space.spot = regexpr(" ", sched.table[ranked.teams, j])
            sched.table[ranked.teams, j] = 
              substr(sched.table[ranked.teams, j], space.spot + 1, 
                     nchar(sched.table[ranked.teams, j]))
          }
          space.locs = gregexpr(" ", sched.table[, j])
          for(k in 1:nrow(sched.table)){
            last.space = max(space.locs[[k]])
            sched.table[k, j] = paste(substr(sched.table[k, j], 1, 
                                             last.space - 1), 
                                      substr(sched.table[k, j], last.space + 1, 
                                             nchar(sched.table[k, j])), sep = "")
          } # remove space between name, abbrev
          name.index = match(sched.table[, j], ESPN_Abbreviations$Full.Name)
          sched.table[, j + 3] = ESPN_Abbreviations[name.index, "Team"]
          sched.table[, j + 8] = ESPN_Abbreviations[name.index, "Abbrev"]
        }
        div2.games = which(is.na(sched.table$Away.Team) | 
                             is.na(sched.table$Home.Team))
        # div2.games = which(sched.table$Away %in% div2.teams | 
        #                      sched.table$Home %in% div2.teams)
        fake.games = which(sched.table$Result %in% 
                             c("Postponed", "Canceled", "Suspended"))
        bad.rows = c(div2.games, fake.games)
        if(length(bad.rows) > 0){
          sched.table = sched.table[-c(bad.rows), ]
        }
        
        if(nrow(sched.table) == 0){
          next()
        }
        for(j in 1:nrow(sched.table)){
          away.abr = sched.table[j, "Away.Abr"]
          home.abr = sched.table[j, "Home.Abr"]
          result.string = as.character(sched.table[j, "Result"])
          away.loc = gregexpr(away.abr, result.string)[[1]]
          home.loc = gregexpr(home.abr, result.string)[[1]]
          overtime.loc = gregexpr("OT)", result.string)[[1]]
          if(length(away.loc) > 1){
            if(1 %in% away.loc){
              away.loc = gregexpr(paste(away.abr, " ", sep = ""), 
                                  result.string)[[1]]
            } else {
              away.loc = gregexpr(paste(" ", away.abr, sep = ""), 
                                  result.string)[[1]] + 1
            }
          }
          if(length(home.loc) > 1){
            if(1 %in% home.loc){
              home.loc = gregexpr(paste(home.abr, " ", sep = ""), 
                                  result.string)[[1]]
            } else {
              home.loc = gregexpr(paste(" ", home.abr, sep = ""), 
                                  result.string)[[1]] + 1
            }
          }
          if(overtime.loc != -1){
            how.many.ot = substr(result.string, overtime.loc - 1, 
                                 overtime.loc - 1) # imma assume no 10 OT games
            if(how.many.ot == "("){
              sched.table[j, "OTimes"] = 1
            } else {
              sched.table[j, "OTimes"] = as.numeric(how.many.ot)
            }
            result.string = substr(result.string, 1, 
                                   regexpr("\\(", result.string) - 2)
          } # if it goes into overtime
          if(away.loc == 1){
            away.start = nchar(away.abr) + 2
            away.end = regexpr(",", result.string) - 1
            home.start = home.loc + nchar(home.abr) + 1
            home.end = nchar(result.string)
          } else if (home.loc == 1){
            home.start = nchar(home.abr) + 2
            home.end = regexpr(",", result.string) - 1
            away.start = away.loc + nchar(away.abr) + 1
            away.end = nchar(result.string)
          }
          away.pts = as.numeric(substr(result.string, away.start, away.end))
          home.pts = as.numeric(substr(result.string, home.start, home.end))
          
          sched.table[j, "Away.Pts"] = away.pts
          sched.table[j, "Home.Pts"] = home.pts
        }
        neutral.games = which(sched.table$Home.Team %in% neutral.home.teams & 
                                sched.table$Away.Team %in% neutral.away.teams)
        sched.table[neutral.games, "Neutral"] = 1
        sched.table$Date = dates[i]
        past.schedule = rbind(past.schedule, sched.table)
      } else if(ncol(sched.table) == 7){
        sched.table[, 3:7] = NULL
        names(sched.table) = c("Away", "Home")
        sched.table$Away.Team = NA
        sched.table$Home.Team = NA
        sched.table$Neutral = 0
        sched.table$Away.Abr = NA
        sched.table$Home.Abr = NA
        
        for(j in 1:2){
          sched.table[, j] = as.character(sched.table[, j])
          ranked.teams = which(regexpr("#", sched.table[, j]) == 1)
          if(length(ranked.teams) > 0){
            space.spot = regexpr(" ", sched.table[ranked.teams, j])
            sched.table[ranked.teams, j] = 
              substr(sched.table[ranked.teams, j], space.spot + 1, 
                     nchar(sched.table[ranked.teams, j]))
          }
          space.locs = gregexpr(" ", sched.table[, j])
          for(k in 1:nrow(sched.table)){
            last.space = max(space.locs[[k]])
            sched.table[k, j] = paste(substr(sched.table[k, j], 1, 
                                             last.space - 1), 
                                      substr(sched.table[k, j], last.space + 1, 
                                             nchar(sched.table[k, j])), sep = "")
          } # remove space between name, abbrev
          name.index = match(sched.table[, j], ESPN_Abbreviations$Full.Name)
          sched.table[, j + 2] = ESPN_Abbreviations[name.index, "Team"]
          sched.table[, j + 5] = ESPN_Abbreviations[name.index, "Abbrev"]
        }
        div2.games = which(is.na(sched.table$Away.Team) | 
                             is.na(sched.table$Home.Team))
        # div2.games = which(sched.table$Away %in% div2.teams | 
        #                      sched.table$Home %in% div2.teams)
        fake.games = which(sched.table$Result %in% 
                             c("Postponed", "Canceled", "Suspended"))
        bad.rows = c(div2.games, fake.games)
        if(length(bad.rows) > 0){
          sched.table = sched.table[-c(bad.rows), ]
        }
        
        if(nrow(sched.table) == 0){
          next()
        }
        neutral.games = which(sched.table$Home.Team %in% neutral.home.teams & 
                                sched.table$Away.Team %in% neutral.away.teams)
        sched.table[neutral.games, "Neutral"] = 1
        sched.table$Date = dates[i]
        future.sched = rbind(future.sched, sched.table)
        
      } else {
        print("Not 6 or 7 cols")
      }
      
      
      
    }
    excel.path = paste(
      "/Users/malexk999/Desktop/Miscellaneous/Football/NCAAF/Schedules/", 
      "Old schedules/Sched.",season, ".Wk.", week, ".xlsx", sep = "")
    past.sheet.exists = FALSE
    future.sheet.exists = FALSE
    old.file = FALSE
    if(file.exists(excel.path)){
      old.file = TRUE
      workbook = loadWorkbook(excel.path)
      sheet.names = names(getSheets(workbook))
      past.sheet.exists = "Past" %in% sheet.names
      future.sheet.exists = "Future" %in% sheet.names
      write.xlsx(data.frame("Nothing" = NA), excel.path, 
                 sheetName = "Placeholder", append = TRUE)
      if(past.sheet.exists){
        removeSheet(workbook, "Past")
      }
      if(future.sheet.exists){
        removeSheet(workbook, "Future")
      }
      saveWorkbook(workbook, excel.path)
    }
    if(nrow(past.schedule) > 0){
      past.schedule[, c(1:3, 9:10)] = NULL
      write.xlsx(past.schedule, excel.path, 
                 row.names = FALSE, sheetName = "Past", append = TRUE)
    }
    if(nrow(future.sched) > 0){
      future.sched[, c(1:2, 6:7)] = NULL
      write.xlsx(future.sched, excel.path, 
                 row.names = FALSE, sheetName = "Future", append = TRUE)
    }
    if(old.file){
      workbook = loadWorkbook(excel.path)
      removeSheet(workbook, "Placeholder")
      saveWorkbook(workbook, excel.path)
    }
  }
  getHfaPts = function(distance){
    return(log(distance + 150) * 0.8968110 - 3.0848390)
  }
  getHfaProb = function(distance){
    exponent = -0.486928 + 0.142786 * log(distance + 150)
    return(exp(exponent) / (1 + exp(exponent)))
  }
  getEloDiff = function(win.prob){
    return(-400*log10(1/win.prob - 1))
  }
  getEloHfa = function(distance){
    return(getEloDiff(getHfaProb(distance)))
  }
  matchupEloHfa = function(team1, team2){
    return(getEloHfa(getCollegeDistance(team1, team2)))
  }
  matchupHfaProb = function(team1, team2){
    return(getHfaProb(getCollegeDistance(team1, team2)))
  }
  getNeuHfa = function(team1, team2, city, state){
    team1.disadv = getEloHfa(getDistance(getCollegeCoords(team1), 
                                         getLatLong(city, state)))
    team2.disadv = getEloHfa(getDistance(getCollegeCoords(team2), 
                                         getLatLong(city, state)))
    hfa.diff = team1.disadv - team2.disadv # positive favors team 2
    return(hfa.diff)
    
  }
} # packages and functions
{
  # US_cities_database <- as.data.frame(
  #   suppressMessages(read_csv("~/Desktop/Miscellaneous/US cities database.csv")))
  distHaversine(getLatLong("Wilmington", "Delaware")[2:1], 
                getLatLong("College Park", "Maryland")[2:1], r = 3959) # miles
  NCAAM_sportsref_teams <- as.data.frame(
    read_excel("~/Desktop/Miscellaneous/NCAAM/NCAAM sportsref teams.xlsx"))
  Wiki_d1_colleges_info <- as.data.frame(
    read_excel("~/Desktop/Miscellaneous/NCAAM/Wiki d-1 colleges info.xlsx"))
} # data import
{
  d1.colleges.data = data.frame("Team" = NA, 
                                "Full.Name" = Wiki_d1_colleges_info$School, 
                                "Nickname" = Wiki_d1_colleges_info$Team,
                                "City" = Wiki_d1_colleges_info$City, 
                                "State" = Wiki_d1_colleges_info$State, 
                                "Conf" = Wiki_d1_colleges_info$`Primary Conference`, 
                                "Latitude" = NA, "Longitude" = NA)
  for(i in 1:6){
    d1.colleges.data[, i] = unlist(as.character(d1.colleges.data[, i]))
    d1.colleges.data[, i] = deleteParenthetics(d1.colleges.data[, i], 
                                               delete.space = FALSE)
    d1.colleges.data[, i] = gsub(intToUtf8(160), " ", d1.colleges.data[, i])
  }
  and.locs = gregexpr(" and", d1.colleges.data$Nickname)
  lady.teams = which(and.locs != -1)
  for(i in lady.teams){
    d1.colleges.data[i, "Nickname"] = substr(d1.colleges.data[i, "Nickname"], 
                                             1, and.locs[[i]] - 1)
  }
  
  NCAAM_sportsref_teams$City = NA
  NCAAM_sportsref_teams$State = NA
  {
    city.state = NCAAM_sportsref_teams$`City, State`
    city.state = gsub("Washington, D.C.",
                      "Washington, District of Columbia", city.state)
    city.state = gsub("Normal, Alabama", "Huntsville, Alabama", city.state)
    city.state = gsub("Teaneck", "Hackensack", city.state)
    NCAAM_sportsref_teams$`City, State` = city.state
    # 
    NCAAM_sportsref_teams$School = gsub("RedHawks", "Redhawks",
                                        NCAAM_sportsref_teams$School)
    d1.colleges.data$City = gsub("Homewood", "Birmingham", d1.colleges.data$City)
    d1.colleges.data$City = gsub("Fairborn", "Dayton", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Chestnut Hill", "Newton", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Smithfield", "Georgiaville", 
    #                              d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Northridge", "San Fernando", 
    #                              d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Fairfield", "Bridgeport", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Honolulu", "Aiea", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Urbana–", "", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Riverdale", "Bronx", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("–Saint Paul", "", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Hamden", "New Haven", d1.colleges.data$City)
    # d1.colleges.data$City = gsub(" Township", "", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("St\\.", "Saint", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Jamaica", "Queens", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("South Orange", "Newark", d1.colleges.data$City)
    # d1.colleges.data$City = gsub("Radnor", "Villanova", d1.colleges.data$City)
    # 
    
    {
      # city.names = US_cities_database$city
      # city.names = gsub(" Center", "", city.names) # amherst, vestal, others
      # US_cities_database$city = city.names
    }
  }
  comma.locs = as.numeric(gregexpr(", ", NCAAM_sportsref_teams$`City, State`))
  NCAAM_sportsref_teams$City = substr(NCAAM_sportsref_teams$`City, State`, 
                                      1, comma.locs - 1)
  NCAAM_sportsref_teams$State = substr(NCAAM_sportsref_teams$`City, State`, 
                                       comma.locs + 2, 
                                       nchar(NCAAM_sportsref_teams$`City, State`))
  for(i in 1:nrow(d1.colleges.data)){
    nickname = d1.colleges.data[i, "Nickname"]
    state = d1.colleges.data[i, "State"]
    nickname.locs = as.numeric(gregexpr(nickname, NCAAM_sportsref_teams$School))
    sportsref.index = which(nickname.locs != -1 & 
                              state == NCAAM_sportsref_teams$State)
    if(length(sportsref.index) > 1){
      city = d1.colleges.data[i, "City"]
      sportsref.index = which(nickname.locs != -1 & 
                                state == NCAAM_sportsref_teams$State & 
                                city == NCAAM_sportsref_teams$City)
    }
    if(length(sportsref.index) == 0){
      print(d1.colleges.data[i, "Full.Name"])
    } else {
      team.name = substr(NCAAM_sportsref_teams[sportsref.index, "School"], 1, 
                         nickname.locs[sportsref.index] - 2)
      d1.colleges.data[i, "Team"] = team.name
    }
    
  }
  
} # consolidating the two datasets
{
  d1.colleges.data$Team = gsub("cut State", "cut", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Green State", "Green", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("State", "St.", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Albany \\(NY)", "Albany", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Brigham Young", "BYU", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("University of Cal", "Cal", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Citadel", "The Citadel", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("State", "St.", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("International", "Intl", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Grambling", "Grambling St.", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("State", "St.", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("aii", "ai'i", d1.colleges.data$Team)
  d1.colleges.data$Team = replaceValue("Louisiana", "Louisiana-Lafayette", 
                                       d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Louisiana St. Fighting", "LSU", 
                               d1.colleges.data$Team)
  d1.colleges.data$Team = gsub(" \\(FL)", "", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("North Carolina St\\.", "NC State", 
                               d1.colleges.data$Team)
  d1.colleges.data$Team = replaceValue("Mississippi", "Ole Miss", 
                                       d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("View", "View A&M", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Southern Methodist", "SMU", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Southeastern", "SE", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Southern Mississippi", "Southern Miss", 
                               d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Saint F", "St. F", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Texas Christian", "TCU", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Alabama-Birmingham", "UAB", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("UC-", "UC ", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Central Florida", "UCF", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Nevada-Las Vegas", "UNLV", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Southern California", "USC", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Texas-El Paso", "UTEP", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Texas-San Antonio", "UTSA", d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Virginia Military Institute", "VMI", 
                               d1.colleges.data$Team)
  d1.colleges.data$Team = gsub("Tennessee-Martin", "UT Martin", d1.colleges.data$Team)
  
  
} # name change
{
  d1.colleges.data$city.coords =  FALSE
  not.found = NULL
  importCollegeCoords = function(team.name){
    i = match(team.name, d1.colleges.data$Team)
    univ.name = d1.colleges.data[i, "Full.Name"]
    tryCatch({
      coords = suppressMessages(geocode(univ.name))
    }, warning = function(w){
      city = d1.colleges.data[i, "City"]
      state = d1.colleges.data[i, "State"]
      tryCatch({
        coords <<- suppressMessages(geocode(paste(univ.name, city, state)))
      }, warning = function(x){
        tryCatch({
          coords <<- suppressMessages(geocode(paste(city, ", ", state, sep = "")))
          d1.colleges.data[i, "city.coords"] <<- TRUE
        }, warning = function(y){
          print(d1.colleges.data[i, "Team"])
          not.found <<- append(not.found, d1.colleges.data[i, "Team"])
          coords <<- NA
        })
      })
    })
    return(coords)
  }
  for(i in 1:nrow(d1.colleges.data)){
    coords = importCollegeCoords(d1.colleges.data[i, "Team"])
    d1.colleges.data[i, c("Longitude", "Latitude")] = coords
    
    
    # tryCatch({
    #   d1.colleges.data[i, c("Latitude", "Longitude")] = getLatLong(
    #     d1.colleges.data[i, "City"], d1.colleges.data[i, "State"])
    # }, error = function(e){
    #   print(d1.colleges.data[i, "Team"])
    # })
  } # getting longitude, latitude
  city.coords.used = d1.colleges.data[which(d1.colleges.data$city.coords), "Team"]
  run.again = match(c(not.found, city.coords.used), d1.colleges.data$Team)
  while(length(not.found) > 0){
    not.found = NULL
    for(i in run.again){
      coords = importCollegeCoords(d1.colleges.data[i, "Team"])
      if(!is.na(coords)){
        d1.colleges.data[i, c("Longitude", "Latitude")] = coords
      }
      
    }
    city.coords.used = d1.colleges.data[which(d1.colleges.data$city.coords), "Team"]
    run.again = match(c(not.found, city.coords.used), d1.colleges.data$Team)
  }
  write.xlsx(d1.colleges.data, 
             "/Users/malexk999/Desktop/Miscellaneous/Football/NCAAF/Div1.Colleges.Data.xlsx")
  
} # getting the coordinates (this might take a while)

{
  # sched.url = 
  #   "http://www.espn.com/college-football/schedule/_/week/4/group/90/year/2017"
  # espn.url = # this works tho!
  #   "http://www.espn.com/college-football/scoreboard/_/group/80/year/2017/seasontype/2/week/4"
  # espn.html = rawToChar(GET(espn.url)$content)
  # gregexpr("College Park", espn.html)
  # gregexpr("UCF", espn.html)
  # gregexpr("Dover", espn.html)
  # venue.locs = gregexpr("venue", espn.html)[[1]]
  # venues = data.frame("Name" = NA, "City" = NA, "State" = NA)
  # venues$I.value = NA
  # for(i in 1:length(venue.locs)){
  #   next.chars = substr(espn.html, venue.locs[i], venue.locs[i] + 500)
  #   city.loc = gregexpr("city", next.chars)[[1]][1]
  #   state.loc = gregexpr("state", next.chars)[[1]]
  #   if(city.loc != -1 & state.loc != -1){
  #     city.string = substr(next.chars, city.loc + 7, city.loc + 50)
  #     city.end = gregexpr("\"", city.string)[[1]][1]
  #     city.name = substr(city.string, 1, city.end - 1)
  #     state.string = substr(next.chars, state.loc + 8, state.loc + 50)
  #     state.end = gregexpr("\"", state.string)[[1]][1]
  #     this.state.abr = substr(state.string, 1, state.end - 1)
  #     this.state.name = state.name[match(this.state.abr, state.abb)]
  #     # venue.coords = getLatLong(city.name, this.state.name)
  #     fullname.loc = gregexpr("fullName", next.chars)[[1]][1]
  #     fullname.string = substr(next.chars, fullname.loc + 11, fullname.loc + 50)
  #     fullname.end = gregexpr("\"", fullname.string)[[1]][1]
  #     venue.name = substr(fullname.string, 1, fullname.end - 1)
  #     index = nrow(venues) + 1
  #     if(is.na(venues[1,1])){
  #       index = 1
  #     }
  #     venues[index, ] = c(venue.name, city.name, this.state.name, i)
  #   }
  #   
  # }
  # 
} # neutral venues (didn't really work)
{
  years = 2017:2002
  dws = didnt.work.seasons
  dww = didnt.work.weeks
  didnt.work.seasons = NULL
  didnt.work.weeks = NULL
  didnt.work.messages = NULL
  {
    # for(i in years){
    #   season = i
    #   for(j in 1:15){
    #     tryCatch({
    #       getNcaafSchedule(j)
    #     }, error = function(e){
    #       didnt.work.seasons <<- append(didnt.work.seasons, season)
    #       didnt.work.weeks <<- append(didnt.work.weeks, j)
    #       didnt.work.messages <<- append(didnt.work.messages, e$message)
    #     })
    #     
    #   }
    # }
  } # original
  for(i in 1:length(dws)){
    season = dws[i]
    j = dww[i]
    # for(j in 1:15){
    tryCatch({
      getNcaafSchedule(j)
    }, error = function(e){
      didnt.work.seasons <<- append(didnt.work.seasons, season)
      didnt.work.weeks <<- append(didnt.work.weeks, j)
      didnt.work.messages <<- append(didnt.work.messages, e$message)
    })
    
    # }
  }
  all.ncaaf.games = data.frame()
  for(i in years){
    for(j in 1:15){
      tryCatch({
        excel.path = paste(
          "/Users/malexk999/Desktop/Miscellaneous/Football/NCAAF/Schedules/", 
          "Old schedules/Sched.", i, ".Wk.", j, ".xlsx", sep = "")
        this.week.games = as.data.frame(read_excel(excel.path))
        this.week.games$Date = as.Date(paste(format(this.week.games$Date, "%m/%d"), 
                                             i, sep = "/"), format = "%m/%d/%Y")
        this.week.games$Season = i
        this.week.games$Week = j
        all.ncaaf.games = rbind(all.ncaaf.games, this.week.games)
        # if(j == 1){
        #   print(i)
        # }
      }, error = function(e){
        dw.index = match(i, didnt.work.seasons)
        if(!is.na(dw.index) & j == didnt.work.weeks[dw.index]){
          # print("Error caught correctly")
          ten=5+5
        } else {
          stop(e)
        }
      })
    }
  }
  write.xlsx(all.ncaaf.games,
             "/Users/malexk999/Desktop/Miscellaneous/Football/NCAAF/Schedules/All NCAAF games.xlsx")
  
} # getting all ncaaf scores data
{
  non.neutral.games = all.ncaaf.games[which(all.ncaaf.games$Neutral == 0), ]
  non.neutral.games$Dist = NA
  non.neutral.games$Away.Rate = NA
  non.neutral.games$Home.Rate = NA
  for(i in 1:nrow(non.neutral.games)){
    away.team = non.neutral.games[i, "Away.Team"]
    home.team = non.neutral.games[i, "Home.Team"]
    non.neutral.games[i, "Dist"] = getCollegeDistance(away.team, home.team)
    season = non.neutral.games[i, "Season"]
    away.index = match(away.team, ncaaf.teams)
    home.index = match(home.team, ncaaf.teams)
    col.name = paste("Kal", season, sep = ".")
    non.neutral.games[i, "Away.Rate"] = ratings.since.2000[away.index, col.name]
    non.neutral.games[i, "Home.Rate"] = ratings.since.2000[home.index, col.name]
  }
  empty.data = which(is.na(non.neutral.games$Dist) | 
                       is.na(non.neutral.games$Away.Rate) | 
                       is.na(non.neutral.games$Home.Rate))
  if(length(empty.data) > 0){
    non.neutral.games = non.neutral.games[-c(empty.data), ]
  }
  non.neutral.games$Pts.Margin = non.neutral.games$Home.Pts - 
    non.neutral.games$Away.Pts
  non.neutral.games$Home.Win = non.neutral.games$Pts.Margin > 0
  coef.value = 150
  non.neutral.games$Dist.Log = log(non.neutral.games$Dist + coef.value)
  pts.mgn.model = lm(Pts.Margin ~ Away.Rate + Home.Rate + Dist.Log, 
                     data = non.neutral.games)
  # base hfa of 1.99 plus 1 ponit for each 1026 miles
  # elo divisor closer to 18.5 (I use 19.5)
  win.prob.model = glm(Home.Win ~ Away.Rate + Home.Rate + Dist.Log, 
                       data = non.neutral.games, family = binomial(link = 'logit'))
  # base hfa of 0.576 win prob (+53 elo pts)
  non.neutral.games$Mgn.Model = fitted(pts.mgn.model)
  non.neutral.games$Win.Prob = fitted(win.prob.model)
  model.func = function(coef.value){
    non.neutral.games$Dist.Log = log(non.neutral.games$Dist + coef.value)
    model = lm(Pts.Margin ~ Away.Rate + Home.Rate + Dist.Log, 
               data = non.neutral.games)
    return(model)
  }
} # researching non-neutral games
# elo boost from travel distance
test.distances = c(0, 10, 25, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000, 2500, 
                   3000, 4000, 5000)
distance.hfa = data.frame("Distance" = test.distances, "WP.ELO" = NA, 
                          "Pts.ELO" = NA)
base.wp.hfa = getEloDiff(getLogisticProb(predict(win.prob.model, data.frame(
  Away.Rate = 500, Home.Rate = 500, Dist.Log = log(0 + 150)))))
base.pts.hfa = predict(pts.mgn.model, data.frame(
  Away.Rate = 500, Home.Rate = 500, Dist.Log = log(0 + 150))) * elo.to.pts
for(i in 1:nrow(distance.hfa)){
  dist.miles = distance.hfa[i, "Distance"]
  distance.hfa[i, "WP.ELO"] = round(getEloDiff(getLogisticProb(predict(
    win.prob.model, data.frame(Away.Rate = 500, Home.Rate = 500, Dist.Log = log(
      dist.miles + 150))))) - base.wp.hfa, 2)
  distance.hfa[i, "Pts.ELO"] = round(predict(pts.mgn.model, data.frame(
    Away.Rate = 500, Home.Rate = 500, Dist.Log = log(dist.miles + 150))) * 
      elo.to.pts - base.pts.hfa, 2)
}








