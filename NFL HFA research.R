seasons = 2017:2002
{
  library(httr)
  library(XML)
  library(readxl)
  library(xlsx)
  library(geosphere)
  library(ggmap)
  
  ConvertFactorData = function(df){
    for(i in 1:ncol(df)){
      if(class(df[, i]) == "factor"){
        df[, i] = as.character(df[, i])
        tryCatch({
          df[, i] = as.numeric(df[, i])
        }, warning = function(w){
          # do nothing
        })
      }
    }
    return(df)
  }
  eloDiff = function(win.pct){
    difference = -400*log10(1/win.pct - 1)
    return(difference)
  }
  GetNflDist = function(team1, team2){
    index1 = match(team1, nfl.names.data$Kalin)
    index2 = match(team2, nfl.names.data$Kalin)
    coords1 = nfl.names.data[index1, c("Longitude", "Latitude")]
    coords2 = nfl.names.data[index2, c("Longitude", "Latitude")]
    distance = distHaversine(coords1, coords2, r = 3959)
    return(distance)
  }
  GetHistDist = function(team1, team2){
    index1 = match(team1, nfl.names.data$Full)
    index2 = match(team2, nfl.names.data$Full)
    coords1 = nfl.names.data[index1, c("Longitude", "Latitude")]
    coords2 = nfl.names.data[index2, c("Longitude", "Latitude")]
    distance = distHaversine(coords1, coords2, r = 3959)
    return(distance)
  }
} # packages and functions
{
  nfl.hist.ratings = data.frame("Team" = nfl.teams)
  nfl.hist.ratings[, 1 + 1:length(seasons)] = NA
  names(nfl.hist.ratings)[1 + 1:length(seasons)] = paste("Kal", seasons, sep = ".")
  {
    nfl.pfr.ratings = nfl.hist.ratings
    names(nfl.pfr.ratings) = gsub("Kal", "PFR", names(nfl.pfr.ratings))
    full.team.names = NULL
    # all.nicknames = NULL
    for(i in seasons){
      pfr.url = paste("https://www.pro-football-reference.com/years/", 
                      i, sep = "")
      pfr.html = rawToChar(GET(pfr.url)$content)
      pfr.data = readHTMLTable(pfr.html)
      pfr.table = rbind(pfr.data$AFC, pfr.data$NFC)
      header.rows = which(is.na(pfr.table$W))
      if(length(header.rows) > 0){
        pfr.table = pfr.table[-c(header.rows), ]
      }
      pfr.table = ConvertFactorData(pfr.table)
      pfr.table$Tm = gsub("\\*", "", pfr.table$Tm)
      pfr.table$Tm = gsub("\\+", "", pfr.table$Tm)
      team.nicknames = character(nNflTeams)
      space.locs = gregexpr(" ", pfr.table$Tm)
      for(j in 1:nNflTeams){
        start.char = max(space.locs[[j]]) + 1
        team.nicknames[j] = substr(pfr.table[j, "Tm"], start.char, 
                            nchar(pfr.table[j, "Tm"]))
      }
      # all.nicknames = append(all.nicknames, team.nicknames)
      full.team.names = append(full.team.names, pfr.table$Tm)
      team.index = integer(nNflTeams)
      for(j in 1:nNflTeams){
        team.index[j] = grep(team.nicknames[j], nfl.teams)
      }
      pfr.table$Pythag = pfr.table$PF ^ 2.37 / 
        (pfr.table$PF ^ 2.37 + pfr.table$PA ^ 2.37)
      pfr.table$Pyth.ELO = 
        round(eloDiff(pfr.table$Pythag) + pfr.table$SoS * 27.5 + 500, 2)
      pfr.table$Srs.ELO = round(pfr.table$SRS * 27.5 + 500, 2)
      pfr.table$Pfr.ELO = round((pfr.table$Pyth.ELO + pfr.table$Srs.ELO) / 2, 2)
      col.name = paste("PFR", i, sep = ".")
      nfl.pfr.ratings[team.index, col.name] = pfr.table$Pfr.ELO
    }
  } # pfr ratings (pythag, srs)
  {
    {
      full.team.names = unique(full.team.names)
      full.team.names = sort(full.team.names)
      nfl.names.data = data.frame("Full" = full.team.names, "Location" = NA, 
                                  "Nickname" = NA, "Kalin" = NA, 
                                  stringsAsFactors = FALSE)
      space.locs = gregexpr(" ", full.team.names)
      for(i in 1:nrow(nfl.names.data)){
        last.space = max(space.locs[[i]])
        nfl.names.data[i, "Location"] = substr(full.team.names[i], 1,
                                               last.space - 1)
        nickname = substr(full.team.names[i], last.space + 1, 
                          nchar(full.team.names[i]))
        nfl.names.data[i, "Nickname"] = nickname
        nfl.names.data[i, "Kalin"] = grep(nickname, nfl.teams, value = TRUE)
      }
      nfl.names.data = nfl.names.data[which(!(nfl.names.data$Location %in%
                                                c("St. Louis", "San Diego"))), ]
      write.xlsx(nfl.names.data, "/Users/malexk999/Desktop/Miscellaneous/Football/NFL Names Data.xlsx", 
                 row.names = FALSE)
    } # team names
    nfl.massey.ratings = nfl.hist.ratings
    names(nfl.massey.ratings) = gsub("Kal", "Mas", names(nfl.hist.ratings))
    massey.excel.path = 
      "~/Desktop/Miscellaneous/Football/NFL Ratings/NFL massey ratings.xlsx"
    didnt.work = NULL
    for(i in seasons){
      raw.massey.data = as.data.frame(read_excel(massey.excel.path, 
                                                 sheet = as.character(i)))
      header.rows = which(raw.massey.data$Team %in% c("Team", "Correlation"))
      if(length(header.rows) > 0){
        raw.massey.data = raw.massey.data[-c(header.rows), ]
      }
      odd.rows = which(1:nrow(raw.massey.data) %% 2 == 1) # even rows are +1
      massey.raw.teams = raw.massey.data[odd.rows, "Team"]
      power.ratings = as.numeric(raw.massey.data[odd.rows + 1, "Pwr"])
      rate.col = paste("Mas", i, sep = ".")
      for(j in 1:length(massey.raw.teams)){
        massey.name = massey.raw.teams[j]
        location.index = which(nfl.names.data$Location == massey.name)
        if(length(location.index) == 1){
          team.index = match(nfl.names.data[location.index, "Kalin"], nfl.teams)
        } else {
          if(massey.name %in% nfl.teams){
            team.index = match(massey.name, nfl.teams)
          } else {
            didnt.work = append(didnt.work, massey.name)
            next()
          }
        }
        nfl.massey.ratings[team.index, rate.col] = power.ratings[j] * 27.5 + 500
      }
    }
  } # massey ratings
  {
    massey.weight = 0.6
    for(i in seasons){
      col.num = 2019 - i
      nfl.hist.ratings[, col.num] = 
        nfl.massey.ratings[, col.num] * massey.weight + 
        nfl.pfr.ratings[, col.num] * (1 - massey.weight)
    }
  } # combination of both
} # historical ratings
{
  get.location.data = TRUE
  NFL_stadiums <- as.data.frame(
    read_excel("~/Desktop/Miscellaneous/Football/NFL Ratings/NFL stadiums.xlsx"))
  if(get.location.data){
    NFL_stadiums$Stadium = gsub("double-dagger", "", NFL_stadiums$Stadium)
    NFL_stadiums$Stadium = gsub("dagger", "", NFL_stadiums$Stadium)
    NFL_stadiums$Longitude = NA
    NFL_stadiums$Latitude = NA
    nfl.names.data[, c("Latitude", "Longitude")] = NA
    cities.used = NULL
    not.found = which(is.na(NFL_stadiums$Latitude))
    while(length(not.found) > 0){
      for(i in not.found){
        # full.team.name = NFL_stadiums[i, "Team"]
        # stadium.index = match(full.team.name, NFL_stadiums$Team)
        # tryCatch({
        #   stadium.name = NFL_stadiums[i, "Stadium"]
        #   city = NFL_stadiums[i, "City"]
        #   coords = suppressMessages(geocode(paste(stadium.name, city)))
        # }, warning = function(w){
        #   coords <<- suppressMessages(geocode(city))
        #   cities.used <<- append(cities.used, full.team.name)
        # })
        city = NFL_stadiums[i, "City"]
        coords = suppressMessages(geocode(city))
        NFL_stadiums[i, c("Longitude", "Latitude")] = coords
      }
      not.found = which(is.na(NFL_stadiums$Latitude))
    }
    write.xlsx(NFL_stadiums, 
               "/Users/malexk999/Desktop/Miscellaneous/Football/NFL Ratings/NFL stadiums.xlsx", 
               row.names = FALSE)
  } 
  names.index = match(nfl.names.data$Full, NFL_stadiums$Team)
  nfl.names.data[, c("Longitude", "Latitude")] = 
    NFL_stadiums[names.index, c("Longitude", "Latitude")]
} # team coordinates
{
  {
    espn.url = "http://www.espn.com/nfl/schedule/_/week/1/year/2017/seasontype/2"
    espn.html = rawToChar(GET(espn.url)$content)
    espn.data = readHTMLTable(espn.html)
    {
      title.loc = gregexpr("title=", espn.html)[[1]]
      this.week.teams = character(length = length(title.loc))
      for(j in 1:length(title.loc)){
        title.substr = substr(espn.html, title.loc[j], title.loc[j] + 50)
        end.name.loc = regexpr('\">', title.substr)
        team.name = substr(title.substr, 8, end.name.loc - 1)
        this.week.teams[j] = team.name
      }
    } # teams full names
    espn.table = data.frame()
    for(j in 1:length(espn.data)){
      espn.table = rbind(espn.table, espn.data[[j]])
    }
    espn.table[, 7] = NULL
    names(espn.table) = c("Away", "Home", "Result", "Pass.Ldr", "Rush.Ldr", 
                          "Rec.Ldr")
    espn.table = ConvertFactorData(espn.table)
    espn.table$Away.Abr = NA
    espn.table$Home.Abr = NA
    espn.abbrevs = NULL
    for(j in 1:2){
      words = strsplit(espn.table[, j], " ")
      for(i in 1:nrow(espn.table)){
        espn.table[i, j + 6] = tail(words[[i]], n = 1)
      }
    }
    espn.table$Away.Full = this.week.teams[(1:16) * 2 - 1]
    espn.table$Home.Full = this.week.teams[(1:16) * 2]
    all.abr = c(espn.table$Away.Abr, espn.table$Home.Abr)
    all.full = c(espn.table$Away.Full, espn.table$Home.Full)
    name.index = match(all.full, nfl.names.data$Full)
    # nfl.names.data$ESPN.Abr = NA
    nfl.names.data[name.index, "ESPN.Abr"] = all.abr
    nfl.names.data[match("San Diego", nfl.names.data$Location), "ESPN.Abr"] = "SD"
    nfl.names.data[match("St. Louis", nfl.names.data$Location), "ESPN.Abr"] = "STL"
  } # espn abbreviations
  # nfl.hfa.data = data.frame("Season" = NA, "Away.Team" = NA, "Home.Team" = NA, 
  #                           "Away.Pts" = NA, "Home.Pts" = NA, "Away.Rate" = NA, 
  #                           "Home.Rate" = NA, "Dist" = NA)
  nfl.hfa.data = data.frame()
  for(i in seasons){
    for(week in 1:17){
      espn.url = paste("http://www.espn.com/nfl/schedule/_/week/", week, 
                       "/year/", i, "/seasontype/2", sep = "")
      espn.html = rawToChar(GET(espn.url)$content)
      espn.data = readHTMLTable(espn.html)
      {
        title.loc = gregexpr("title=", espn.html)[[1]]
        this.week.teams = character(length = length(title.loc))
        for(j in 1:length(title.loc)){
          title.substr = substr(espn.html, title.loc[j], title.loc[j] + 50)
          end.name.loc = regexpr('\">', title.substr)
          team.name = substr(title.substr, 8, end.name.loc - 1)
          this.week.teams[j] = team.name
        }
      } # teams full names
      {
        neutral.home.teams = NULL
        neutral.locs = gregexpr('data-home-text="vs"', espn.html)[[1]]
        if(neutral.locs[1] != -1){
          for(j in neutral.locs){
            next.thou.chars = substr(espn.html, j, j + 1000)
            # span.loc = gregexpr("<span>", next.thou.chars)[[1]]
            # end.span.loc = gregexpr("</span>", next.thou.chars)[[1]]
            # if(length(end.span.loc) > length(span.loc) & 
            #    end.span.loc[1] < span.loc[1]){
            #   end.span.loc = end.span.loc[2]
            # } else {
            #   end.span.loc = end.span.loc[1]
            # }
            # span.loc = span.loc[1]
            # team.name = substr(next.thou.chars, span.loc + 6, end.span.loc - 1)
            title.loc = regexpr("title=", next.thou.chars)
            title.substr = substr(next.thou.chars, title.loc, title.loc + 50)
            end.name.loc = regexpr('\">', title.substr)
            team.name = substr(title.substr, 8, end.name.loc - 1)
            name.index = match(team.name, nfl.names.data$Full)
            espn.name = paste(nfl.names.data[name.index, "Location"], 
                              nfl.names.data[name.index, "ESPN.Abr"])
            neutral.home.teams = append(neutral.home.teams, espn.name)
            
          }
        }
      } # neutral games
      espn.table = data.frame()
      for(j in 1:length(espn.data)){
        if(ncol(espn.data[[j]]) > 1){
          espn.table = rbind(espn.table, espn.data[[j]])
        }
        # otherwise it's teams on bye
      }
      espn.table = espn.table[, 1:3]
      espn.table = ConvertFactorData(espn.table)
      names(espn.table) = c("Away", "Home", "Result")
      {
        neutral.games = which(espn.table$Home %in% neutral.home.teams)
        if(length(neutral.games) > 0){
          espn.table = espn.table[-c(neutral.games), ]
          this.week.teams = this.week.teams[-c(neutral.games * 2 - 1:0)]
        }
        # DO NOT REMOVE NEUTRAL GAMES IN THE NFL SCRAPER
      } # removing neutral games
      espn.table$Away.Team = NA
      espn.table$Home.Team = NA
      espn.table$Away.Pts = NA
      espn.table$Home.Pts = NA
      espn.table$Dist = NA
      for(j in 1:nrow(espn.table)){
        teams = this.week.teams[j * 2 - 1:0]
        kalin.teams = nfl.names.data[match(teams, nfl.names.data$Full), "Kalin"]
        espn.table[j, "Dist"] = GetHistDist(teams[1], teams[2])
        espn.table[j, c("Away.Team", "Home.Team")] = kalin.teams
      }
      {
        fake.games = which(espn.table$Result %in% c("Postponed", "Canceled"))
        if(length(fake.games) > 0){
          espn.table = espn.table[-c(fake.games), ]
        }
      } # eliminating postponed games
      for(j in 1:2){
        words = strsplit(espn.table[, j], " ")
        for(k in 1:nrow(espn.table)){
          espn.abr = tail(words[[k]], n = 1)
          result.loc = regexpr(espn.abr, espn.table[k, "Result"]) + 
            nchar(espn.abr)
          pts.scored = substr(espn.table[k, "Result"], result.loc + 1, 
                              result.loc + 2)
          pts.scored = as.numeric(gsub(",", "", pts.scored))
          espn.table[k, j + 5] = pts.scored
        }
      }
      espn.table$Season = i
      espn.table$Week = week
      espn.table = espn.table[, c("Season", "Week", "Away.Team", "Home.Team", 
                                  "Away.Pts", "Home.Pts", "Dist")]
      nfl.hfa.data = rbind(nfl.hfa.data, espn.table)
    }
  }
} # getting historical game data
{
  nfl.hfa.data$Away.Rate = NA
  nfl.hfa.data$Home.Rate = NA
  for(i in 1:nrow(nfl.hfa.data)){
    col.name = paste("Kal", nfl.hfa.data[i, "Season"], sep = ".")
    away.index = match(nfl.hfa.data[i, "Away.Team"], nfl.teams)
    home.index = match(nfl.hfa.data[i, "Home.Team"], nfl.teams)
    nfl.hfa.data[i, "Away.Rate"] = nfl.hist.ratings[away.index, col.name]
    nfl.hfa.data[i, "Home.Rate"] = nfl.hist.ratings[home.index, col.name]
  }
} # filling in each team's rating
{
  nfl.hfa.data$Elo.Diff = nfl.hfa.data$Home.Rate - nfl.hfa.data$Away.Rate
  nfl.hfa.data$Pt.Diff = nfl.hfa.data$Home.Pts - nfl.hfa.data$Away.Pts
  {
    ties = which(nfl.hfa.data$Pt.Diff == 0)
    if(length(ties) > 0){
      nfl.hfa.data = nfl.hfa.data[-c(ties), ]
    }
    same.city = which(nfl.hfa.data$Dist == 0)
    if(length(same.city) > 0){
      nfl.hfa.data = nfl.hfa.data[-c(same.city), ]
    }
  } # removing unwanted data
  nfl.hfa.data$Home.Win = as.numeric(nfl.hfa.data$Pt.Diff > 0)
  
  log.hfa.model = glm(Home.Win ~ Elo.Diff + log(Dist), data = nfl.hfa.data, 
                      family = binomial())
  lin.hfa.model = lm(Pt.Diff ~ Elo.Diff + log(Dist), data = nfl.hfa.data)
  {
    find.coef = function(lower.bound, upper.bound, model.func = model.function, 
                         threshold = 10^-5){
      # model.func must have one attribute: the value for the coefficient
      # model.func must return the linear model (retVal = lm(y~x))
      n.vals = 11
      values = seq(lower.bound, upper.bound, length.out = n.vals)
      resid.sq.error = numeric(n.vals)
      # r.squared = NA
      for(i in 1:length(values)){
        model = model.func(values[i])
        resid.sq.error[i] = summary(model)$sigma
        # r.squared[i] = summary(model)$r.squared
      }
      min.index = match(min(resid.sq.error), resid.sq.error)
      if(min.index == 1){
        new.low.bound = lower.bound - (upper.bound - lower.bound) / 2
        new.upp.bound = upper.bound - (upper.bound - lower.bound) / 2
        return(find.coef(new.low.bound, new.upp.bound, model.func = model.func, 
                         threshold = threshold))
      } else if(min.index == n.vals){
        new.low.bound = lower.bound + (upper.bound - lower.bound) / 2
        new.upp.bound = upper.bound + (upper.bound - lower.bound) / 2
        return(find.coef(new.low.bound, new.upp.bound, model.func = model.func, 
                         threshold = threshold))
      } else {
        ratio1 = resid.sq.error[min.index - 1] / resid.sq.error[min.index]
        ratio2 = resid.sq.error[min.index + 1] / resid.sq.error[min.index]
        if((ratio1 - 1) < threshold & (ratio2 - 1) < threshold){
          return(values[min.index])
        } else {
          new.low.bound = values[min.index - 1]
          new.upp.bound = values[min.index + 1]
          return(find.coef(new.low.bound, new.upp.bound, model.func = model.func, 
                           threshold = threshold))
        }
      }
    }
    model.function = function(coef.value){
      model = lm(Pt.Diff ~ Elo.Diff + log(Dist + coef.value), 
                 data = nfl.hfa.data)
      return(model)
    }
  } # dist + x
  
  # log.hfa.model = glm(Home.Win ~ Elo.Diff, data = nfl.hfa.data, 
                      # family = binomial())
  # lin.hfa.model = lm(Pt.Diff ~ Elo.Diff, data = nfl.hfa.data)
  NflDistHfa = function(distance){
    hfa = eloDiff(1 / (1 + exp(-(0.082593 * log(distance) - 0.156340))))
    return(max(hfa, 0))
  }
  GetNflHfa = function(team1, team2){
    return(NflDistHfa(GetNflDist(team1, team2)))
  }
  nfl.hfa.data$Home.Wpct = fitted(log.hfa.model)
} # regression analysis
