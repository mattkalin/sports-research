# espn, cbs, nfl, ff today, number fire, fantasypros, fantasy sharks, 
# real time fantasy
# yahoo? (no projections?)
{
  library(httr)
  library(XML)
  library(readxl)
  library(plyr)
} # packages
fantasy.positions = c("QB", "RB", "WR", "TE")
lowercase.positions = tolower(fantasy.positions)
position.viewed = c(40, 120, 120, 40)
min.attempts = c(30, 20, 10) # will remove stuff like 10 yds on 0.5 att (20 y/a)
# Pass.Att, Rush.Att, Rec.Rec
{
  fantasy.positions = c("QB", "RB", "WR", "TE")
  espn.pages = c(1, 3, 3, 1)
  espn.proj = data.frame()
  for(i in 1:length(fantasy.positions)){
    for(page in 1:espn.pages[i]){
      espn.url = paste(
        "http://games.espn.com/ffl/tools/projections?leagueId=0&slotCategoryId=", 
                       (i - 1) * 2, "&startIndex=", (page - 1) * 40, sep = "")
      espn.data = readHTMLTable(rawToChar(GET(espn.url)$content))[[2]]
      espn.data = FirstRowAsHeader(espn.data)
      espn.data = ConvertFactorData(espn.data)
      espn.data$Pos = fantasy.positions[i]
      espn.proj = rbind(espn.proj, espn.data)
    }
  }
  espn.proj$Name = substr(espn.proj$`PLAYER, TEAM POS`, 1, 
                             regexpr(",", espn.proj$`PLAYER, TEAM POS`) - 1)
  espn.proj$PTS = espn.proj$PTS - espn.proj$REC # non-PPR
  espn.proj$Pass.Att = as.numeric(substr(
    espn.proj$`C/A`, regexpr("/", espn.proj$`C/A`) + 1, nchar(espn.proj$`C/A`)))
  
  {
    espn.names = espn.proj$Name
    espn.names = gsub(" II", "", espn.names)
    espn.names = gsub(" Jr.", "", espn.names)
    espn.names = gsub(" Sr.", "", espn.names)
    espn.names = gsub("J\\.J\\.", "JJ", espn.names)
    espn.names = gsub("Fuller V", "Fuller", espn.names)
    espn.names = gsub(" III", "", espn.names)
    espn.names = gsub("\\*", "", espn.names)
    espn.names = gsub("JJ Smith-Schuster", "JuJu Smith-Schuster", espn.names)
    espn.names = gsub("DJ", "D.J.", espn.names)
    # espn.names = gsub("Mitchell Trubisky", "Mitch Trubisky", espn.names)
    
    # TJJnes?
    espn.proj$Name = espn.names
  } # name change
  names(espn.proj)[c(4:5, 8:9, 11:12, 6:7, 10)] = 
    c("Pass.Yds", "Pass.TD", "Rush.Yds", "Rush.TD", "Rec.Yds", "Rec.TD", 
      "Pass.Int", "Rush.Att", "Rec.Rec")
  espn.proj$Yd.Pass = espn.proj$Pass.Yds / espn.proj$Pass.Att
  espn.proj$TD.Pass = espn.proj$Pass.TD / espn.proj$Pass.Att
  espn.proj$Int.Pass = espn.proj$Pass.Int / espn.proj$Pass.Att
  espn.proj$Yd.Rush = espn.proj$Rush.Yds / espn.proj$Rush.Att
  espn.proj$TD.Rush = espn.proj$Rush.TD / espn.proj$Rush.Att
  espn.proj$Yd.Rec = espn.proj$Rec.Yds / espn.proj$Rec.Rec
  espn.proj$TD.Rec = espn.proj$Rec.TD / espn.proj$Rec.Rec
  
  espn.proj[which(espn.proj$Pass.Att < min.attempts[1]), 
            c("Yd.Pass", "TD.Pass", "Int.Pass")] = NA
  espn.proj[which(espn.proj$Rush.Att < min.attempts[2]), 
            c("Yd.Rush", "TD.Rush")] = NA
  espn.proj[which(espn.proj$Rec.Rec < min.attempts[3]), 
            c("Yd.Rec", "TD.Rec")] = NA
} # ESPN projections
{
  cbs.stat.types = c("Passing", "Rushing", "Receiving", "Misc")
  cbs.stat.cols = c(7, 4, 4, 2)
  stat.prefixes = c("Pass", "Rush", "Rec", "Misc")
  cbs.proj = data.frame()
  important.cols = c("Name", "Pass.Att", "Pass.Yd", "Pass.TD", "Pass.INT", 
                     "Rush.Att", "Rush.Yd", "Rush.TD", 
                     "Rec.Recpt", "Rec.Yd", "Rec.TD")
  for(i in 1:length(fantasy.positions)){
    cbs.url = paste("https://www.cbssports.com/fantasy/football/stats/weeklyprojections/", 
                    fantasy.positions[i], "?&print_rows=", position.viewed[i], 
                    sep = "")
    cbs.data = readHTMLTable(rawToChar(GET(cbs.url)$content))[[1]]
    {
      stat.categories = cbs.data[1, ]
      x = NULL
      for(j in 1:length(stat.categories)){
        y = as.character(stat.categories[1, j])
        if(isTRUE(nchar(y) > 1)){
          x = append(x, y)
        }
      }
      stat.categories = x
      
    } # which stats are being presented
    cbs.data = cbs.data[-c(1, nrow(cbs.data)), ]
    cbs.data = FirstRowAsHeader(cbs.data)
    cbs.data = ConvertFactorData(cbs.data)
    cbs.data$Name = substr(cbs.data$Player, 1, regexpr(",", cbs.data$Player) - 1)
    stat.type.index = match(stat.categories, cbs.stat.types)
    stat.type.cols = cbs.stat.cols[stat.type.index]
    for(j in 1:length(stat.categories)){
      stat.col.start = sum(stat.type.cols[1:j]) - stat.type.cols[j] + 2
      stat.col.end = stat.col.start + stat.type.cols[j] - 1
      names(cbs.data)[stat.col.start:stat.col.end] = 
        paste(stat.prefixes[stat.type.index[j]], 
              names(cbs.data)[stat.col.start:stat.col.end], sep = ".")
    }
    missing.stat = setdiff(cbs.stat.types, stat.categories)
    volume.name = ifelse(missing.stat == "Receiving", "Recpt", "Att")
    new.col.names = character(0)
    for(j in 1:length(missing.stat)){
      new.col.names = c(new.col.names, paste(
        stat.prefixes[match(missing.stat[j], cbs.stat.types)], 
        c(volume.name[j], "Yd", "TD"), sep = "."))
    }
    if("Passing" %in% missing.stat){
      new.col.names = c(new.col.names, "Pass.INT")
    }
    cbs.data[, new.col.names] = 0
    cbs.data = cbs.data[, important.cols]
    cbs.proj = rbind(cbs.proj, cbs.data)
  }
  {
    cbs.proj$Name = gsub("A.J. McCarron", "AJ McCarron", cbs.proj$Name)
    cbs.proj$Name = gsub(" Sr.", "", cbs.proj$Name)
    cbs.proj$Name = gsub("DJ", "D.J.", cbs.proj$Name)
    cbs.proj$Name = gsub("J.J.", "JJ", cbs.proj$Name)
    cbs.proj$Name = gsub("JJ Smith", "JuJu Smith", cbs.proj$Name)
    
  } # name change
  names(cbs.proj) = gsub("Recpt", "Rec", names(cbs.proj))
  cbs.proj$Yd.Pass = cbs.proj$Pass.Yd / cbs.proj$Pass.Att
  cbs.proj$TD.Pass = cbs.proj$Pass.TD / cbs.proj$Pass.Att
  cbs.proj$Int.Pass = cbs.proj$Pass.INT / cbs.proj$Pass.Att
  cbs.proj$Yd.Rush = cbs.proj$Rush.Yd / cbs.proj$Rush.Att
  cbs.proj$TD.Rush = cbs.proj$Rush.TD / cbs.proj$Rush.Att
  cbs.proj$Yd.Rec = cbs.proj$Rec.Yd / cbs.proj$Rec.Rec
  cbs.proj$TD.Rec = cbs.proj$Rec.TD / cbs.proj$Rec.Rec
  
  cbs.proj[which(cbs.proj$Pass.Att < min.attempts[1]), 
            c("Yd.Pass", "TD.Pass", "Int.Pass")] = NA
  cbs.proj[which(cbs.proj$Rush.Att < min.attempts[2]), 
            c("Yd.Rush", "TD.Rush")] = NA
  cbs.proj[which(cbs.proj$Rec.Rec < min.attempts[3]), 
            c("Yd.Rec", "TD.Rec")] = NA
} # CBS projections
{
  pages.needed = position.viewed %/% 40
  important.cols = c("Name", "Pass.Att", "Pass.Yard", "Pass.TD", "Pass.INT", 
                     "Rush.Att", "Rush.Yard", "Rush.TD", 
                     "Rec.Rec", "Rec.Yard", "Rec.TD")
  ff2day.proj = data.frame()
  for(i in 1:length(fantasy.positions)){
    for(page in 1:pages.needed[i]){
      ff2day.url = paste("http://fftoday.com/rankings/playerproj.php?PosID=", 
                         i, "0&LeagueID=1&cur_page=", page - 1, sep = "")
      ff2day.data = readHTMLTable(rawToChar(GET(ff2day.url)$content))[[11]]
      ff2day.data = FirstRowAsHeader(ff2day.data)
      for(j in 1:ncol(ff2day.data)){
        ff2day.data[, j] = gsub(",", "", ff2day.data[, j])
      }
      ff2day.data = ConvertFactorData(ff2day.data)
      ff2day.data$Name = substr(ff2day.data[, 2], 3, nchar(ff2day.data[, 2]))
      
      pass.cols = numeric(0)
      rush.cols = numeric(0)
      rec.cols = numeric(0)
      comp.index = match("Comp", names(ff2day.data))
      if(is.na(comp.index)){
        att.index = match("Att", names(ff2day.data))
        if(!is.na(att.index)){
          rush.cols = att.index:(att.index + 2)
        }
        rec.index = match("Rec", names(ff2day.data))
        rec.cols = rec.index:(rec.index + 2)
      } else {
        pass.cols = comp.index:(comp.index + 4)
        rush.cols = (max(pass.cols) + 1):(max(pass.cols) + 3)
      }
      orig.col.names = names(ff2day.data)
      if(length(pass.cols) == 0){
        ff2day.data[, c("Pass.Att", "Pass.Yard", "Pass.TD", "Pass.INT")] = 0
      } else {
        names(ff2day.data)[pass.cols] = 
          paste("Pass", orig.col.names[pass.cols], sep = ".")
      }
      if(length(rush.cols) == 0){
        ff2day.data[, c("Rush.Att", "Rush.Yard", "Rush.TD")] = 0
      } else {
        names(ff2day.data)[rush.cols] = 
          paste("Rush", orig.col.names[rush.cols], sep = ".")
      }
      if(length(rec.cols) == 0){
        ff2day.data[, c("Rec.Rec", "Rec.Yard", "Rec.TD")] = 0
      } else {
        names(ff2day.data)[rec.cols] = 
          paste("Rec", orig.col.names[rec.cols], sep = ".")
      }
      ff2day.data = ff2day.data[, important.cols]
      ff2day.proj = rbind(ff2day.proj, ff2day.data)
    }
  }
  {
    ff2day.proj$Name = gsub(" Jr.", "", ff2day.proj$Name)
    ff2day.proj$Name = gsub("Ben Watson", "Benjamin Watson", ff2day.proj$Name)
    ff2day.proj$Name = gsub("J.J.", "JJ", ff2day.proj$Name)
    ff2day.proj$Name = gsub("JJ Smith", "JuJu Smith", ff2day.proj$Name)
  } # name change
  ff2day.proj$Yd.Pass = ff2day.proj$Pass.Yard / ff2day.proj$Pass.Att
  ff2day.proj$TD.Pass = ff2day.proj$Pass.TD / ff2day.proj$Pass.Att
  ff2day.proj$Int.Pass = ff2day.proj$Pass.INT / ff2day.proj$Pass.Att
  ff2day.proj$Yd.Rush = ff2day.proj$Rush.Yard / ff2day.proj$Rush.Att
  ff2day.proj$TD.Rush = ff2day.proj$Rush.TD / ff2day.proj$Rush.Att
  ff2day.proj$Yd.Rec = ff2day.proj$Rec.Yard / ff2day.proj$Rec.Rec
  ff2day.proj$TD.Rec = ff2day.proj$Rec.TD / ff2day.proj$Rec.Rec
  
  ff2day.proj[which(ff2day.proj$Pass.Att < min.attempts[1]), 
            c("Yd.Pass", "TD.Pass", "Int.Pass")] = NA
  ff2day.proj[which(ff2day.proj$Rush.Att < min.attempts[2]), 
            c("Yd.Rush", "TD.Rush")] = NA
  ff2day.proj[which(ff2day.proj$Rec.Rec < min.attempts[3]), 
            c("Yd.Rec", "TD.Rec")] = NA
} # fantasy football today
{
  NumFireCleanup=function(df){
    stats=df[[2]]
    players=df[[1]]
    df = stats
    df[, ncol(df) + 1] = as.data.frame(players)
    df = df[, c(ncol(df), 1:(ncol(df) - 1))]
    df = ConvertFactorData(df)
    names(df) = gsub("\n", "", gsub(" ", "", names(df)))
    names(df) = gsub(".1", "", gsub(" ", "", names(df)))
    df[,1] = substr(df[,1], 1, regexpr("\n", df[,1])-1)
    # df[,1]=gsub("\n", "", df[,1])
    # df[,1]=gsub("    ", "", df[,1])
    
    return(df)
  }
  important.cols = c("Name", "Pass.Att", "Pass.Yds", "Pass.TDs", "Pass.Ints", 
                     "Rush.Att", "Rush.Yds", "Rush.TDs", 
                     "Rec.Rec", "Rec.Yds", "Rec.TDs")
  numfire.proj = data.frame()
  for(i in 1:length(fantasy.positions)){
    numfire.url = paste("http://www.numberfire.com/nfl/fantasy/remaining-projections/", 
                        lowercase.positions[i], sep = "")
    numfire.data = readHTMLTable(rawToChar(GET(numfire.url)$content))
    numfire.data = NumFireCleanup(numfire.data)
    
    pass.cols = which(names(numfire.data) == "C/A") + 0:3
    rush.cols = which(names(numfire.data) == "Att") + 0:2
    rec.cols = which(names(numfire.data) == "Rec") + 0:2
    orig.col.names = names(numfire.data)
    if(length(pass.cols) == 0){
      numfire.data[, c("Pass.Att", "Pass.Yds", "Pass.TDs", "Pass.Ints")] = 0
    } else {
      numfire.data$Pass.Att = 
        as.numeric(substr(numfire.data$`C/A`, regexpr("/", numfire.data$`C/A`) 
                          + 1, nchar(numfire.data$`C/A`)))
      names(numfire.data)[pass.cols] = 
        paste("Pass", orig.col.names[pass.cols], sep = ".")
    }
    if(length(rush.cols) == 0){
      numfire.data[, c("Rush.Att", "Rush.Yds", "Rush.TDs")] = 0
    } else {
      names(numfire.data)[rush.cols] = 
        paste("Rush", orig.col.names[rush.cols], sep = ".")
    }
    if(length(rec.cols) == 0){
      numfire.data[, c("Rec.Rec", "Rec.Yds", "Rec.TDs")] = 0
    } else {
      names(numfire.data)[rec.cols] = 
        paste("Rec", orig.col.names[rec.cols], sep = ".")
    }
    names(numfire.data)[1] = "Name" # instead of "Player"
    numfire.data = numfire.data[, important.cols]
    numfire.proj = rbind(numfire.proj, numfire.data)
  }
  {
    numfire.proj$Name = gsub(" Jr.", "", numfire.proj$Name)
    numfire.proj$Name = gsub("Michael Ge", "Mike Ge", numfire.proj$Name)
    numfire.proj$Name = gsub("A.J. Mc", "AJ Mc", numfire.proj$Name)
    numfire.proj$Name = gsub("J.J.", "JJ", numfire.proj$Name)
    numfire.proj$Name = gsub("JJ Smith", "JuJu Smith", numfire.proj$Name)
  } # name change
  numfire.proj$Yd.Pass = numfire.proj$Pass.Yds / numfire.proj$Pass.Att
  numfire.proj$TD.Pass = numfire.proj$Pass.TDs / numfire.proj$Pass.Att
  numfire.proj$Int.Pass = numfire.proj$Pass.Ints / numfire.proj$Pass.Att
  numfire.proj$Yd.Rush = numfire.proj$Rush.Yds / numfire.proj$Rush.Att
  numfire.proj$TD.Rush = numfire.proj$Rush.TDs / numfire.proj$Rush.Att
  numfire.proj$Yd.Rec = numfire.proj$Rec.Yds / numfire.proj$Rec.Rec
  numfire.proj$TD.Rec = numfire.proj$Rec.TDs / numfire.proj$Rec.Rec
  
  numfire.proj[which(numfire.proj$Pass.Att < min.attempts[1]), 
            c("Yd.Pass", "TD.Pass", "Int.Pass")] = NA
  numfire.proj[which(numfire.proj$Rush.Att < min.attempts[2]), 
            c("Yd.Rush", "TD.Rush")] = NA
  numfire.proj[which(numfire.proj$Rec.Rec < min.attempts[3]), 
            c("Yd.Rec", "TD.Rec")] = NA
} # number fire
{
  important.cols = c("Name", "Pass.ATT", "Pass.YDS", "Pass.TDS", "Pass.INTS", 
                     "Rush.ATT", "Rush.YDS", "Rush.TDS", 
                     "Rec.REC", "Rec.YDS", "Rec.TDS")
  fpros.proj = data.frame()
  for(i in 1:length(fantasy.positions)){
    fpros.url = paste("https://www.fantasypros.com/nfl/projections/", 
                      lowercase.positions[i], ".php", sep = "")
    fpros.data = readHTMLTable(rawToChar(GET(fpros.url)$content))[[1]]
    for(j in 1:ncol(fpros.data)){
      fpros.data[, j] = gsub(",", "", fpros.data[, j])
    }
    fpros.data = ConvertFactorData(fpros.data)
    fpros.data$Name = NA
    for(j in 1:nrow(fpros.data)){
      player = fpros.data[j, "Player"]
      last.space = max(gregexpr(" ", player)[[1]])
      fpros.data[j, "Name"] = substr(player, 1, last.space - 1)
    }
    
    pass.cols = which(names(fpros.data) == "CMP") + -1:3
    tryCatch({
      rush.cols = max(which(names(fpros.data) == "ATT")) + 0:2
    }, warning = function(w){
      rush.cols <<- integer(0)
    })
    rec.cols = which(names(fpros.data) == "REC") + 0:2
    orig.col.names = names(fpros.data)
    if(length(pass.cols) == 0){
      fpros.data[, c("Pass.ATT", "Pass.YDS", "Pass.TDS", "Pass.INTS")] = 0
    } else {
      names(fpros.data)[pass.cols] = 
        paste("Pass", orig.col.names[pass.cols], sep = ".")
    }
    if(length(rush.cols) == 0){
      fpros.data[, c("Rush.ATT", "Rush.YDS", "Rush.TDS")] = 0
    } else {
      names(fpros.data)[rush.cols] = 
        paste("Rush", orig.col.names[rush.cols], sep = ".")
    }
    if(length(rec.cols) == 0){
      fpros.data[, c("Rec.REC", "Rec.YDS", "Rec.TDS")] = 0
    } else {
      names(fpros.data)[rec.cols] = 
        paste("Rec", orig.col.names[rec.cols], sep = ".")
    }
    fpros.data = fpros.data[, important.cols]
    fpros.proj = rbind(fpros.proj, fpros.data)
  }
  {
    fpros.proj$Name = gsub(" II", "", fpros.proj$Name)
    fpros.proj$Name = gsub(" Jr.", "", fpros.proj$Name)
    fpros.proj$Name = gsub("Devante", "DeVante", fpros.proj$Name)
    fpros.proj$Name = 
      gsub("Mitch Trubisky", "Mitchell Trubisky", fpros.proj$Name)
    fpros.proj$Name = gsub("J.J.", "JJ", fpros.proj$Name)
    fpros.proj$Name = gsub("JJ Smith", "JuJu Smith", fpros.proj$Name)
  } # name change
  names(fpros.proj)[c(2, 6, 9)] = c("Pass.Att", "Rush.Att", "Rec.Rec")
  fpros.proj$Yd.Pass = fpros.proj$Pass.YDS / fpros.proj$Pass.Att
  fpros.proj$TD.Pass = fpros.proj$Pass.TDS / fpros.proj$Pass.Att
  fpros.proj$Int.Pass = fpros.proj$Pass.INTS / fpros.proj$Pass.Att
  fpros.proj$Yd.Rush = fpros.proj$Rush.YDS / fpros.proj$Rush.Att
  fpros.proj$TD.Rush = fpros.proj$Rush.TDS / fpros.proj$Rush.Att
  fpros.proj$Yd.Rec = fpros.proj$Rec.YDS / fpros.proj$Rec.Rec
  fpros.proj$TD.Rec = fpros.proj$Rec.TDS / fpros.proj$Rec.Rec
  
  fpros.proj[which(fpros.proj$Pass.Att < min.attempts[1]), 
            c("Yd.Pass", "TD.Pass", "Int.Pass")] = NA
  fpros.proj[which(fpros.proj$Rush.Att < min.attempts[2]), 
            c("Yd.Rush", "TD.Rush")] = NA
  fpros.proj[which(fpros.proj$Rec.Rec < min.attempts[3]), 
            c("Yd.Rec", "TD.Rec")] = NA
  
  # tavon austin is on here twice lmfaooooo (he's a RB and WR)
  # best part is the RB and WR have different projections
  fpros.proj[which(fpros.proj$Name == "Ryan Griffin" & fpros.proj$Pass.Att > 0), 
             "Name"] = "Ryan Griffin 2"
} # fantasy pros
{
  list.names = c("Index", "Pass.Att", "Rush.Att", "Rec.Rec", "Yd.Pass", 
                 "TD.Pass", "Int.Pass", "Yd.Rush", "TD.Rush", 
                 "Yd.Rec", "TD.Rec")
  comp.proj = vector('list', length = length(list.names))
  names(comp.proj) = list.names
  comp.proj$Index = data.frame("Name" = espn.proj$Name, 
                               "ESPN" = 1:nrow(espn.proj), "CBS" = NA, 
                               "FF2Day" = NA, "NumFire" = NA, "FPros" = NA, 
                               stringsAsFactors = FALSE)
  for(i in 1:nrow(comp.proj$Index)){
    name = comp.proj$Index[i, "Name"]
    comp.proj$Index[i, "CBS"] = match(name, cbs.proj$Name)
    comp.proj$Index[i, "FF2Day"] = match(name, ff2day.proj$Name)
    comp.proj$Index[i, "NumFire"] = match(name, numfire.proj$Name)
    comp.proj$Index[i, "FPros"] = match(name, fpros.proj$Name)
  }
  volume.stats = c("Pass.Att", "Rush.Att", "Rec.Rec")
  efficiency.stats = setdiff(names(comp.proj), c(volume.stats, "Index"))
  for(j in 2:length(list.names)){
    comp.proj[[j]] = comp.proj$Index
    comp.proj[[j]]$Avg = NA
    col.name = list.names[j]
    for(i in 1:nrow(comp.proj$Index)){
      comp.proj[[j]][i, "ESPN"] = espn.proj[comp.proj$Index[i, "ESPN"], col.name]
      comp.proj[[j]][i, "CBS"] = cbs.proj[comp.proj$Index[i, "CBS"], col.name]
      comp.proj[[j]][i, "FF2Day"] = 
        ff2day.proj[comp.proj$Index[i, "FF2Day"], col.name]
      comp.proj[[j]][i, "NumFire"] = 
        numfire.proj[comp.proj$Index[i, "NumFire"], col.name]
      comp.proj[[j]][i, "FPros"] = 
        fpros.proj[comp.proj$Index[i, "FPros"], col.name]
      proj.data = as.numeric(comp.proj[[j]][i, 2:6])
      if(col.name %in% volume.stats){
        proj.data[which(is.na(proj.data))] = 0
      }
      comp.proj[[j]][i, "Avg"] = mean(proj.data, na.rm = TRUE)
      comp.proj[[j]][i, "SD"] = sd(proj.data, na.rm = TRUE)
    }
    if(col.name %in% volume.stats){
      sd.pct = 0.1
    } else {
      sd.pct = 0.05
    }
    comp.proj[[j]][, "+/-"] = comp.proj[[j]]$SD + sd.pct * comp.proj[[j]]$Avg
  }
  missing.players = NULL
  for(i in 1:nrow(comp.proj$Index)){
    touch.metric = comp.proj$Pass.Att[i, "ESPN"] + 
      4 * comp.proj$Rush.Att[i, "ESPN"] + 4 * comp.proj$Rec.Rec[i, "ESPN"]
    if(touch.metric >= 100 & sum(is.na(comp.proj$Index[i, ])) > 0){
      missing.players = append(missing.players, comp.proj$Index[i, "Name"])
    }
  }
  RemoveMissingPlayer = function(name){
    player.index = which(missing.players == name)
    if(length(player.index) > 0){
      missing.players <<- missing.players[-c(player.index)]
    }
  }
  RemoveMissingPlayer("Baker Mayfield")
  RemoveMissingPlayer("Jacoby Brissett")
  RemoveMissingPlayer("Chase Edmonds")
  RemoveMissingPlayer("John Kelly")
  RemoveMissingPlayer("Joe Williams")
  RemoveMissingPlayer("Justin Jackson")
  RemoveMissingPlayer("Mark Walton")
  RemoveMissingPlayer("Willie Snead")
  RemoveMissingPlayer("John Ross")
  RemoveMissingPlayer("Curtis Samuel")
  RemoveMissingPlayer("Boston Scott")
  RemoveMissingPlayer("Carlos Henderson")
  RemoveMissingPlayer("Jeremy Kerley")
  RemoveMissingPlayer("Gerald Everett")
  RemoveMissingPlayer("Jake Butt")
  RemoveMissingPlayer("Nick Vannett")
  RemoveMissingPlayer("Rico Gathers")
  RemoveMissingPlayer("Mark Andrews")
  RemoveMissingPlayer("Jermaine Gresham")
  RemoveMissingPlayer("T.J. Logan")
  
  
  player.positions = espn.proj$Pos
  # print(missing.players)
  # View(comp.proj$Index)
} # composite projections
{
  season = 2018
  prev.yrs = 4
  {
    pfr.url = paste("https://www.pro-football-reference.com/years/", 
                    season - 1, sep = "")
    pfr.team.data = readHTMLTable(rawToChar(GET(pfr.url)$content))
    pfr.team.data = rbind(pfr.team.data$AFC, pfr.team.data$NFC)
    pfr.team.data = pfr.team.data[-c(which(is.na(pfr.team.data$W))), ]
    pfr.team.data = ConvertFactorData(pfr.team.data)
    pfr.team.data$Tm = gsub("\\*", "", pfr.team.data$Tm)
    pfr.team.data$Tm = gsub("\\+", "", pfr.team.data$Tm)
    pfr.team.data$Pyth = pfr.team.data$PF ^ 2.37 / 
      (pfr.team.data$PF ^ 2.37 + pfr.team.data$PA ^ 2.37)
    pfr.team.data$PApg = pfr.team.data$PA / 16
    pfr.team.data$ESPN.Abr = substr(pfr.team.data$Tm, 1, 3)
    ChangeEspnAbr = function(team, new.abr){
      index = match(team, pfr.team.data$Tm)
      pfr.team.data[index, "ESPN.Abr"] <<- new.abr
    }
    ChangeEspnAbr("Green Bay Packers", "GB")
    ChangeEspnAbr("Jacksonville Jaguars", "Jax")
    ChangeEspnAbr("Kansas City Chiefs", "KC")
    ChangeEspnAbr("Los Angeles Chargers", "LAC")
    ChangeEspnAbr("Los Angeles Rams", "LAR")
    ChangeEspnAbr("New England Patriots", "NE")
    ChangeEspnAbr("New Orleans Saints", "NO")
    ChangeEspnAbr("New York Giants", "NYG")
    ChangeEspnAbr("New York Jets", "NYJ")
    ChangeEspnAbr("San Francisco 49ers", "SF")
    ChangeEspnAbr("Tampa Bay Buccaneers", "TB")
    ChangeEspnAbr("Washington Redskins", "Wsh")
    
    player.teams = substr(espn.proj$`PLAYER, TEAM POS`, 
                          regexpr(",", espn.proj$`PLAYER, TEAM POS`) + 2, 
                          regexpr(",", espn.proj$`PLAYER, TEAM POS`) + 4)
    player.teams = gsub(" ", "", player.teams)
    player.teams = gsub(intToUtf8(160), "", player.teams)
    
  } # team stats from prev season
  {
    pfr.all.data = data.frame()
    important.cols = c("Player", "Year", "Pass.Att", "Yd.Pass", "TD.Pass", 
                       "Int.Pass", "Rush.Att", "Yd.Rush", "TD.Rush", "Rec", 
                       "Yd.Rec", "TD.Rec")
    {
      pfr.qb.data = data.frame()
      for(k in (season - 1:prev.yrs)){
        pfr.url = paste("https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=", 
                        k, "&year_max=", k, 
                        "&season_start=1&season_end=-1&pos%5B%5D=qb&pos%5B%5D=rb&pos%5B%5D=wr&pos%5B%5D=te&pos%5B%5D=e&pos%5B%5D=t&pos%5B%5D=g&pos%5B%5D=c&pos%5B%5D=ol&pos%5B%5D=dt&pos%5B%5D=de&pos%5B%5D=dl&pos%5B%5D=ilb&pos%5B%5D=olb&pos%5B%5D=lb&pos%5B%5D=cb&pos%5B%5D=s&pos%5B%5D=db&pos%5B%5D=k&pos%5B%5D=p&draft_year_min=1936&draft_year_max=2018&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c1stat=pass_att&c1comp=gt&c1val=30&c2stat=rush_att&c2comp=gt&c2val=0&c5val=1.0&order_by=pass_att", 
                        sep = "")
        pfr.data = readHTMLTable(rawToChar(GET(pfr.url)$content))[[1]]
        header.rows = which(pfr.data$Rk == "Rk")
        if(length(header.rows) > 0){
          pfr.data = pfr.data[-c(header.rows), ]
        }
        pfr.data = ConvertFactorData(pfr.data)
        pfr.qb.data = rbind(pfr.qb.data, pfr.data)
      }
      names(pfr.qb.data)[c(20, 28, 29, 31, 11)] = c("Sack.Yds", "Rush.Att", 
                                                "Rush.Yds", "Rush.TD", "Pass.Att")
      pfr.qb.data$Yd.Pass = pfr.qb.data$Yds / pfr.qb.data$Pass.Att
      pfr.qb.data$TD.Pass = pfr.qb.data$TD / pfr.qb.data$Pass.Att
      pfr.qb.data$Int.Pass = pfr.qb.data$Int / pfr.qb.data$Pass.Att
      pfr.qb.data$Yd.Rush = pfr.qb.data$Rush.Yds / pfr.qb.data$Rush.Att
      pfr.qb.data$TD.Rush = pfr.qb.data$Rush.TD / pfr.qb.data$Rush.Att
      
      pfr.qb.data$Rec = 0
      pfr.qb.data$Yd.Rec = NA
      pfr.qb.data$TD.Rec = NA
      
      pfr.all.data = rbind(pfr.all.data, pfr.qb.data[, important.cols])
      rookie.qbs = c("Josh Rosen", "Sam Darnold", "Josh Allen", "Baker Mayfield", 
                     "Lamar Jackson")
      rookie.draft.picks = c(10, 3, 7, 1, 32)
      # rookie qb draft picks
    } # QB
    {
      pfr.rb.data = data.frame()
      for(k in (season - 1:prev.yrs)){
        next.page = TRUE
        page = 1
        while(next.page){
          offset.number = 100 * (page - 1)
          pfr.url = paste("https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=", 
                          k, "&year_max=", k, 
                          "&season_start=1&season_end=-1&pos%5B%5D=rb&draft_year_min=1936&draft_year_max=2018&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c1stat=rush_att&c1comp=gt&c1val=10&c2stat=rec&c2comp=gt&c2val=0&c5val=1.0&order_by=rush_att&offset=", 
                          offset.number, sep = "")
          pfr.data = readHTMLTable(rawToChar(GET(pfr.url)$content))[[1]]
          header.rows = which(pfr.data$Rk == "Rk")
          if(length(header.rows) > 0){
            pfr.data = pfr.data[-c(header.rows), ]
          }
          pfr.data = ConvertFactorData(pfr.data)
          pfr.rb.data = rbind(pfr.rb.data, pfr.data)
          next.page = nrow(pfr.data) == 100
          page = page + 1
        }
        
      }
      names(pfr.rb.data)[c(11, 13, 17, 19, 10)] = c("Rush.Yds", "Rush.TD", 
                                                "Rec.Yds", "Rec.TD", "Rush.Att")
      pfr.rb.data$Yd.Rush = pfr.rb.data$Rush.Yds / pfr.rb.data$Rush.Att
      pfr.rb.data$TD.Rush = pfr.rb.data$Rush.TD / pfr.rb.data$Rush.Att
      pfr.rb.data$Yd.Rec = pfr.rb.data$Rec.Yds / pfr.rb.data$Rec
      pfr.rb.data$TD.Rec = pfr.rb.data$Rec.TD / pfr.rb.data$Rec
      
      pfr.rb.data$Pass.Att = 0
      pfr.rb.data$Yd.Pass = NA
      pfr.rb.data$TD.Pass = NA
      pfr.rb.data$Int.Pass = NA
      
      pfr.all.data = rbind(pfr.all.data, pfr.rb.data[, important.cols])
    } # RB
    {
      pfr.wr.data = data.frame()
      for(k in (season - 1:prev.yrs)){
        next.page = TRUE
        page = 1
        while(next.page){
          offset.number = 100 * (page - 1)
          pfr.url = paste("https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=", 
                          k, "&year_max=", k, "&season_start=1&season_end=-1&pos%5B%5D=wr&draft_year_min=1936&draft_year_max=2018&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c1stat=rush_att&c1comp=gt&c1val=0&c2stat=rec&c2comp=gt&c2val=10&c5val=1.0&order_by=rec&offset=", 
                          offset.number, sep = "")
          pfr.data = readHTMLTable(rawToChar(GET(pfr.url)$content))[[1]]
          header.rows = which(pfr.data$Rk == "Rk")
          if(length(header.rows) > 0){
            pfr.data = pfr.data[-c(header.rows), ]
          }
          pfr.data = ConvertFactorData(pfr.data)
          pfr.wr.data = rbind(pfr.wr.data, pfr.data)
          next.page = nrow(pfr.data) == 100
          page = page + 1
        }
        
      }
      names(pfr.wr.data)[c(11, 13, 17, 19, 10)] = c("Rush.Yds", "Rush.TD", 
                                                "Rec.Yds", "Rec.TD", "Rush.Att")
      pfr.wr.data$Yd.Rush = pfr.wr.data$Rush.Yds / pfr.wr.data$Rush.Att
      pfr.wr.data$TD.Rush = pfr.wr.data$Rush.TD / pfr.wr.data$Rush.Att
      pfr.wr.data$Yd.Rec = pfr.wr.data$Rec.Yds / pfr.wr.data$Rec
      pfr.wr.data$TD.Rec = pfr.wr.data$Rec.TD / pfr.wr.data$Rec
      
      pfr.wr.data$Pass.Att = 0
      pfr.wr.data$Yd.Pass = NA
      pfr.wr.data$TD.Pass = NA
      pfr.wr.data$Int.Pass = NA
      
      pfr.all.data = rbind(pfr.all.data, pfr.wr.data[, important.cols])
    } # WR
    {
      pfr.te.data = data.frame()
      for(k in (season - 1:prev.yrs)){
        next.page = TRUE
        page = 1
        while(next.page){
          offset.number = 100 * (page - 1)
          pfr.url = paste("https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=", 
                          k, "&year_max=", k, "&season_start=1&season_end=-1&pos%5B%5D=te&draft_year_min=1936&draft_year_max=2018&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c1stat=rush_att&c1comp=gt&c1val=0&c2stat=rec&c2comp=gt&c2val=10&c5val=1.0&order_by=rec&offset=", 
                          offset.number, sep = "")
          pfr.data = readHTMLTable(rawToChar(GET(pfr.url)$content))[[1]]
          header.rows = which(pfr.data$Rk == "Rk")
          if(length(header.rows) > 0){
            pfr.data = pfr.data[-c(header.rows), ]
          }
          pfr.data = ConvertFactorData(pfr.data)
          pfr.te.data = rbind(pfr.te.data, pfr.data)
          next.page = nrow(pfr.data) == 100
          page = page + 1
        }
        
      }
      names(pfr.te.data)[c(11, 13, 17, 19, 10)] = c("Rush.Yds", "Rush.TD", 
                                                    "Rec.Yds", "Rec.TD", "Rush.Att")
      pfr.te.data$Yd.Rush = pfr.te.data$Rush.Yds / pfr.te.data$Rush.Att
      pfr.te.data$TD.Rush = pfr.te.data$Rush.TD / pfr.te.data$Rush.Att
      pfr.te.data$Yd.Rec = pfr.te.data$Rec.Yds / pfr.te.data$Rec
      pfr.te.data$TD.Rec = pfr.te.data$Rec.TD / pfr.te.data$Rec
      
      pfr.te.data$Pass.Att = 0
      pfr.te.data$Yd.Pass = NA
      pfr.te.data$TD.Pass = NA
      pfr.te.data$Int.Pass = NA
      
      pfr.all.data = rbind(pfr.all.data, pfr.te.data[, important.cols])
    } # TE
    {
      pfr.names = pfr.all.data$Player
      pfr.names = gsub("Mitch T", "Mitchell T", pfr.names)
      pfr.names = gsub("A.J. M", "AJ M", pfr.names)
      pfr.names = gsub("T.J. J", "TJ J", pfr.names)
      pfr.names = gsub("Ben Watson", "Benjamin Watson", pfr.names)
      
      pfr.all.data$Player = pfr.names
    } # name change # trubisky, mccarron, tj jones, benjamin watson, 
  } # pfr data
  {
    kalin.proj = data.frame("Name" = espn.proj$Name, "Pos" = player.positions, 
                            "Team" = player.teams, "Yd.Pass" = NA, 
                            "TD.Pass" = NA, "Int.Pass" = NA, "Yd.Rush" = NA, 
                            "TD.Rush" = NA, "Yd.Rec" = NA, "TD.Rec" = NA, 
                            stringsAsFactors = FALSE)
    rookies = NULL
    for(i in 1:nrow(kalin.proj)){
      name = kalin.proj[i, "Name"]
      pfr.index = which(pfr.all.data$Player == name)
      pfr.seasons = pfr.all.data[pfr.index, "Year"]
      if(!isTRUE(all.equal(pfr.seasons, season - 1:prev.yrs))){
        new.pfr.index = rep(NA, prev.yrs)
        for(j in 1:prev.yrs){
          new.pfr.index[j] = pfr.index[match(season - j, pfr.seasons)]
        }
        pfr.index = new.pfr.index
      }
      proj.pass = comp.proj$Pass.Att[i, "Avg"]
      proj.rush = comp.proj$Rush.Att[i, "Avg"]
      proj.rec = comp.proj$Rec.Rec[i, "Avg"]
      if(mean(is.na(pfr.index)) == 1){
        draft.pick = rookie.draft.picks[match(name, rookie.qbs)]
        rookies = append(rookies, name)
      }
      if(proj.pass > 0){
        hist.ypp = pfr.all.data[pfr.index, "Yd.Pass"]
        hist.tdp = pfr.all.data[pfr.index, "TD.Pass"]
        hist.int.pct = pfr.all.data[pfr.index, "Int.Pass"]
        hist.pass.att = pfr.all.data[pfr.index, "Pass.Att"]
        
        team.abr = player.teams[i]
        team.index = match(team.abr, pfr.team.data$ESPN.Abr)
        team.papg = pfr.team.data[team.index, "PApg"]
        team.pyth = pfr.team.data[team.index, "Pyth"]
        
        proj.ypp = ProjectYdPass(hist.ypp[1:2], hist.pass.att[1:2], 
                                 team.papg, team.pyth)
        proj.tdp = ProjectTdPass(hist.tdp[1:3], hist.pass.att[1:3], 
                                 team.papg, team.pyth)
        proj.int.pass = ProjectIntPass(hist.int.pct[1:3], hist.pass.att[1:3], 
                                       draft.pick)
      } else {
        proj.ypp = 0
        proj.tdp = 0
        proj.int.pass = 0
      } # passing projections
      if(proj.rush > 0){
        hist.ypr = pfr.all.data[pfr.index, "Yd.Rush"]
        hist.tdr = pfr.all.data[pfr.index, "TD.Rush"]
        hist.rush.att = pfr.all.data[pfr.index, "Rush.Att"]
        
        is.qb = player.positions[i] == "QB"
        if(is.qb){
          proj.ypr = ProjectQbYdRush(hist.ypr[1], hist.rush.att[1])
          proj.tdr = ProjectQbTDRush()
        } else {
          proj.ypr = ProjectYdRush(hist.ypr[1:2], hist.rush.att[1:2])
          proj.tdr = ProjectRbTdRush(hist.tdr[1], hist.rush.att[1])
        }
      } else {
        proj.ypr = 0
        proj.tdr = 0
      } # rushing projections
      if(proj.rec > 0){
        hist.ydrec = pfr.all.data[pfr.index, "Yd.Rec"]
        hist.tdrec = pfr.all.data[pfr.index, "TD.Rec"]
        hist.rec = pfr.all.data[pfr.index, "Rec"]
        is.rb = player.positions[i] == "RB"
        
        proj.ydrec = ProjectYdRec(hist.ydrec, hist.rec, is.rb)
        proj.tdrec = ProjectTdRec(hist.tdrec, hist.rec, is.rb)
      } else {
        proj.ydrec = 0
        proj.tdrec = 0
      }
      kalin.proj[i, "Yd.Pass"] = proj.ypp
      kalin.proj[i, "TD.Pass"] = proj.tdp
      kalin.proj[i, "Int.Pass"] = proj.int.pass
      kalin.proj[i, "Yd.Rush"] = proj.ypr
      kalin.proj[i, "TD.Rush"] = proj.tdr
      kalin.proj[i, "Yd.Rec"] = proj.ydrec
      kalin.proj[i, "TD.Rec"] = proj.tdrec
    }
    # print(rookies)
    # rookies[which(kalin.proj[match(rookies, kalin.proj$Name), "Pos"] == "QB")]
  } # the projections
} # kalin projections (efficiency only)
{
  kalin.pct = 0.4 # 40% me, 60% online projections
  online.pct = 1 - kalin.pct
  final.proj = data.frame("Name" = kalin.proj$Name, "Pos" = kalin.proj$Pos, 
                          "Team" = kalin.proj$Team, 
                          "Pass.Att" = comp.proj$Pass.Att$Avg, "Yd.Pass" = NA, 
                          "TD.Pass" = NA, "Int.Pass" = NA, "Pass.Yds" = NA, 
                          "Pass.TD" = NA, "Int" = NA, 
                          "Rush.Att" = comp.proj$Rush.Att$Avg, "Yd.Rush" = NA, 
                          "TD.Rush" = NA, "Rush.Yds" = NA, "Rush.TD" = NA, 
                          "Rec" = comp.proj$Rec.Rec$Avg, "Yd.Rec" = NA, 
                          "TD.Rec" = NA, "Rec.Yds" = NA, "Rec.TD" = NA, 
                          "Pts" = NA, stringsAsFactors = FALSE)
  efficiency.stats = c("Yd.Pass", "TD.Pass", "Int.Pass", "Yd.Rush", "TD.Rush", 
                       "Yd.Rec", "TD.Rec")
  for(i in 1:length(efficiency.stats)){
    stat.name = efficiency.stats[i]
    online.proj = comp.proj[[stat.name]]$Avg
    online.proj[which(is.na(online.proj))] = 0
    final.proj[, stat.name] = kalin.pct * kalin.proj[, stat.name] + 
      online.pct * online.proj
  }
  
  final.proj$Pass.Yds = final.proj$Yd.Pass * final.proj$Pass.Att
  final.proj$Pass.TD = final.proj$TD.Pass * final.proj$Pass.Att
  final.proj$Int = final.proj$Int.Pass * final.proj$Pass.Att
  final.proj$Rush.Yds = final.proj$Yd.Rush * final.proj$Rush.Att
  final.proj$Rush.TD = final.proj$TD.Rush * final.proj$Rush.Att
  final.proj$Rec.Yds = final.proj$Yd.Rec * final.proj$Rec
  final.proj$Rec.TD = final.proj$TD.Rec * final.proj$Rec
  final.proj$Pts = final.proj$Pass.Yds / 25 + final.proj$Pass.TD * 4 + 
    final.proj$Int * -2 + (final.proj$Rush.Yds + final.proj$Rec.Yds) / 10 + 
    (final.proj$Rush.TD + final.proj$Rec.TD) * 6
  
  final.proj$Pass.Rate = NA
  for(i in 1:nrow(final.proj)){
    if(final.proj[i, "Pass.Att"] > 0){
      comp.pct = min((0.63 - 0.3) * 5, 2.375)
      yds.att = min((final.proj[i, "Yd.Pass"] - 3) * 0.25, 2.375)
      tds.att = min(final.proj[i, "TD.Pass"] * 20, 2.375)
      int.att = 2.375 - final.proj[i, "Int.Pass"] * 25
      final.proj[i, "Pass.Rate"] = 
        (comp.pct + yds.att + tds.att + int.att) / 6 * 100
    }
  }
} # final projections
{
  plus.minus.proj = final.proj
  site.names = names(comp.proj$Index)[2:6]
  for(i in volume.stats){
    plus.minus.proj[, i] = comp.proj[[i]]$`+/-`
  }
  for(i in efficiency.stats){
    kalin.stat.proj = kalin.proj[, i]
    avg.proj = final.proj[, i]
    for(j in 1:nrow(plus.minus.proj)){
      comp.proj[[i]][j, "+/-"] = sd(c(as.numeric(comp.proj[[i]][j, site.names]), 
                                      kalin.stat.proj[j]), na.rm = TRUE) + 
        0.05 * avg.proj[j]
    }
    comp.proj[[i]][which(is.na(comp.proj[[i]]$`+/-`)), "+/-"] = 0
    plus.minus.proj[, i] = comp.proj[[i]]$`+/-`
  }
  plus.minus.proj$Rec = plus.minus.proj$Rec.Rec
  plus.minus.proj$Rec.Rec = NULL
  plus.minus.proj$Pass.Yds = 
    GetProductSD(final.proj$Pass.Att, plus.minus.proj$Pass.Att, 
                 final.proj$Yd.Pass, plus.minus.proj$Yd.Pass)
  plus.minus.proj$Pass.TD = 
    GetProductSD(final.proj$Pass.Att, plus.minus.proj$Pass.Att, 
                 final.proj$TD.Pass, plus.minus.proj$TD.Pass)
  plus.minus.proj$Int = 
    GetProductSD(final.proj$Pass.Att, plus.minus.proj$Pass.Att, 
                 final.proj$Int.Pass, plus.minus.proj$Int.Pass)
  plus.minus.proj$Rush.Yds = 
    GetProductSD(final.proj$Rush.Att, plus.minus.proj$Rush.Att, 
                 final.proj$Yd.Rush, plus.minus.proj$Yd.Rush)
  plus.minus.proj$Rush.TD = 
    GetProductSD(final.proj$Rush.Att, plus.minus.proj$Rush.Att, 
                 final.proj$TD.Rush, plus.minus.proj$TD.Rush)
  plus.minus.proj$Rec.Yds = 
    GetProductSD(final.proj$Rec, plus.minus.proj$Rec, 
                 final.proj$Yd.Rec, plus.minus.proj$Yd.Rec)
  plus.minus.proj$Rec.TD = 
    GetProductSD(final.proj$Rec, plus.minus.proj$Rec, 
                 final.proj$TD.Rec, plus.minus.proj$TD.Rec)
  final.stats = c("Pass.Yds", "Pass.TD", "Int", "Rush.Yds", "Rush.TD", 
                  "Rec.Yds", "Rec.TD")
  point.multipliers = c(1/25, 4, -2, 1/10, 6, 1/10, 6)
  names(point.multipliers) = final.stats
  passing.pts.sd = NA # passing attempts * points per pass
  rushing.pts.sd = NA
  receiving.pts.sd = NA
  for(i in 1:nrow(plus.minus.proj)){
    # plus.minus.proj[i, "Pts"] = GetSumSD(abs(as.numeric(
    #   plus.minus.proj[i, final.stats] * point.multipliers)))
    passing.pts.sd[i] = GetProductSD(
      final.proj[i, "Pass.Att"], plus.minus.proj[i, "Pass.Att"], 
      sum(final.proj[i, efficiency.stats[1:3]] * 
            point.multipliers[1:3]), 
      GetSumSD(abs(as.numeric(plus.minus.proj[i, efficiency.stats[1:3]] * 
                                point.multipliers[1:3]))))
    rushing.pts.sd[i] = GetProductSD(
      final.proj[i, "Rush.Att"], plus.minus.proj[i, "Rush.Att"], 
      sum(final.proj[i, efficiency.stats[4:5]] * 
            point.multipliers[4:5]), 
      GetSumSD(abs(as.numeric(plus.minus.proj[i, efficiency.stats[4:5]] * 
                                point.multipliers[4:5]))))
    receiving.pts.sd[i] = GetProductSD(
      final.proj[i, "Rec"], plus.minus.proj[i, "Rec"], 
      sum(final.proj[i, efficiency.stats[6:7]] * 
            point.multipliers[6:7]), 
      GetSumSD(abs(as.numeric(plus.minus.proj[i, efficiency.stats[6:7]] * 
                                point.multipliers[6:7]))))
    plus.minus.proj[i, "Pts"] = 
      GetSumSD(c(passing.pts.sd[i], rushing.pts.sd[i], receiving.pts.sd[i]))
  }
  # this is all based on one standard deviation
} # uncertainty 
{
  clean.proj = final.proj[, c("Name", "Pos", "Team", "Pts")]
  clean.proj$Pts = round(clean.proj$Pts, 2)
  clean.proj$PAR = NA
  clean.proj$PAW = NA
  clean.proj$Pos.Rk = NA
  replacement.ranks = c(12, 29, 29, 14)
  waivers.ranks = c(19, 55, 65, 23)
  replacement.pts = numeric(4)
  waivers.pts = numeric(4)
  for(i in 1:length(fantasy.positions)){
    index = which(clean.proj$Pos == fantasy.positions[i])
    fant.pts = clean.proj[index, "Pts"]
    sorted.pts = sort(fant.pts, decreasing = TRUE)
    replacement.pts[i] = sorted.pts[replacement.ranks[i] + 1]
    waivers.pts[i] = sorted.pts[waivers.ranks[i] + 1]
    clean.proj[index, "PAR"] = round(clean.proj[index, "Pts"] - 
                                       replacement.pts[i], 2)
    clean.proj[index, "PAW"] = round(clean.proj[index, "Pts"] - 
                                       waivers.pts[i], 2)
    clean.proj[index, "Pos.Rk"] = length(index) - 
      rank(fant.pts, ties.method = "last") + 1
  }
  clean.proj$Par.Rk = nrow(clean.proj) -
    rank(clean.proj$PAR, ties.method = "last") + 1
  clean.proj$SD = round(plus.minus.proj$Pts, 2)
  confidence.interval.sd = 1
  clean.proj$Floor = clean.proj$Pts - clean.proj$SD * confidence.interval.sd
  clean.proj$Ceiling = clean.proj$Pts + clean.proj$SD * confidence.interval.sd
  
} # clean projections
{
  {
    espn.adp.data = data.frame()
    for(i in 1:length(fantasy.positions)){
      adp.url = paste("http://games.espn.com/ffl/livedraftresults?position=", 
                      fantasy.positions[i], sep = "")
      adp.table = readHTMLTable(rawToChar(GET(adp.url)$content))[[2]]
      adp.table = adp.table[-c(1), ]
      adp.table = FirstRowAsHeader(adp.table)
      adp.table = ConvertFactorData(adp.table)
      espn.adp.data = rbind(espn.adp.data, adp.table)
    }
    espn.adp.data$Name = substr(espn.adp.data$`PLAYER, TEAM`, 1, 
                                regexpr(",", espn.adp.data$`PLAYER, TEAM`) - 1)
    {
      espn.names = espn.adp.data$Name
      espn.names = gsub(" II", "", espn.names)
      espn.names = gsub(" Jr.", "", espn.names)
      espn.names = gsub(" Sr.", "", espn.names)
      espn.names = gsub("J\\.J\\.", "JJ", espn.names)
      espn.names = gsub("Fuller V", "Fuller", espn.names)
      espn.names = gsub(" III", "", espn.names)
      espn.names = gsub("\\*", "", espn.names)
      espn.names = gsub("JJ Smith-Schuster", "JuJu Smith-Schuster", espn.names)
      espn.names = gsub("DJ", "D.J.", espn.names)
      # espn.names = gsub("Mitchell Trubisky", "Mitch Trubisky", espn.names)
      
      # TJJnes?
      espn.adp.data$Name = espn.names
    } # name change
    
    adp.index = match(clean.proj$Name, espn.adp.data$Name)
    clean.proj[which(is.na(adp.index)), "Name"]
    clean.proj$ADP = espn.adp.data[adp.index, "AVG PICK"]
    clean.proj[which(clean.proj$ADP == 170), "ADP"] = 200
    clean.proj[which(is.na(clean.proj$ADP)), "ADP"] = 200
  } # getting raw data from ESPN
  {
    espn.url = "http://www.espn.com/fantasy/football/story/_/page/18RanksPreseason300PPR/2018-fantasy-football-ppr-rankings-top-300"
    espn.html = rawToChar(GET(espn.url)$content)
    espn.rankings = ConvertFactorData(readHTMLTable(espn.html)[[2]])
    espn.rankings$Rank = NA
    espn.rankings$Name = NA
    for(i in 1:nrow(espn.rankings)){
      words = strsplit(espn.rankings[i, "Rank, Player"], "\\. ")[[1]]
      espn.rankings[i, "Rank"] = words[1]
      espn.rankings[i, "Name"] = paste(tail(words, -1), collapse = ". ")
    }
    espn.rankings$Rank = as.numeric(espn.rankings$Rank)
    espn.rankings$Round = GetFantasyRound(espn.rankings$Rank)
    {
      espn.names = espn.rankings$Name
      espn.names = gsub(" II", "", espn.names)
      espn.names = gsub(" Jr.", "", espn.names)
      espn.names = gsub(" Sr.", "", espn.names)
      espn.names = gsub("J\\.J\\.", "JJ", espn.names)
      espn.names = gsub("Fuller V", "Fuller", espn.names)
      espn.names = gsub(" III", "", espn.names)
      espn.names = gsub("\\*", "", espn.names)
      espn.names = gsub("JJ Smith-Schuster", "JuJu Smith-Schuster", espn.names)
      espn.names = gsub("DJ", "D.J.", espn.names)
      # espn.names = gsub("Mitchell Trubisky", "Mitch Trubisky", espn.names)
      
      # TJJnes?
      espn.rankings$Name = espn.names
    } # name change
    rank.index = match(clean.proj$Name, espn.rankings$Name)
    clean.proj$ESPN.Rk = espn.rankings[rank.index, "Rank"]
    clean.proj[which(is.na(clean.proj$ESPN.Rk)), "ESPN.Rk"] = 301
  } # espn rankings
  
  
  {
    drafted.players = clean.proj[which(!is.na(clean.proj$ADP)), ]
    # plot(drafted.players$ADP, (200 - drafted.players$PAW))
    # 
    # starters = clean.proj[which(!is.na(clean.proj$ADP) & clean.proj$ADP < 12*7), ]
    # plot(starters$ADP, (starters$PAW))
    # find.coef = function(lower.bound, upper.bound, model.func = model.function, 
    #                      threshold = 10^-5){
    #   # model.func must have one attribute: the value for the coefficient
    #   # model.func must return the linear model (retVal = lm(y~x))
    #   n.vals = 11
    #   values = seq(lower.bound, upper.bound, length.out = n.vals)
    #   resid.sq.error = NA
    #   # r.squared = NA
    #   for(i in 1:length(values)){
    #     model = model.func(values[i])
    #     resid.sq.error[i] = summary(model)$sigma
    #     # r.squared[i] = summary(model)$r.squared
    #   }
    #   min.index = match(min(resid.sq.error), resid.sq.error)
    #   if(min.index == 1){
    #     new.low.bound = lower.bound - (upper.bound - lower.bound) / 2
    #     new.upp.bound = upper.bound - (upper.bound - lower.bound) / 2
    #     return(find.coef(new.low.bound, new.upp.bound, model.func = model.func, 
    #                      threshold = threshold))
    #   } else if(min.index == n.vals){
    #     new.low.bound = lower.bound + (upper.bound - lower.bound) / 2
    #     new.upp.bound = upper.bound + (upper.bound - lower.bound) / 2
    #     return(find.coef(new.low.bound, new.upp.bound, model.func = model.func, 
    #                      threshold = threshold))
    #   } else {
    #     ratio1 = resid.sq.error[min.index - 1] / resid.sq.error[min.index]
    #     ratio2 = resid.sq.error[min.index + 1] / resid.sq.error[min.index]
    #     if((ratio1 - 1) < threshold & (ratio2 - 1) < threshold){
    #       return(values[min.index])
    #     } else {
    #       new.low.bound = values[min.index - 1]
    #       new.upp.bound = values[min.index + 1]
    #       return(find.coef(new.low.bound, new.upp.bound, model.func = model.func, 
    #                        threshold = threshold))
    #     }
    #   }
    # }
    # model.function = function(coef.value){
    #   starters$PAR.Plus50 = starters$PAR + 50
    #   starters$ADP = starters$ADP ^ coef.value
    #   model = lm(PAR.Plus50 ~ ADP, data = starters)
    #   return(model)
    # }
    # coef.value = find.coef(0, 2)
    # starters$PAR.Plus50 = starters$PAR + 50
    # starters$ADP.x = starters$ADP ^ coef.value
    # adp.model = lm(PAR.Plus50 ~ ADP.x, data = starters)
    # starters$Model = fitted(adp.model) - 50
    # 
    # draft.value = data.frame("Pick" = 1:84, "Value" = NA)
    # draft.value$Value = predict(adp.model, data.frame(
    #   ADP.x = draft.value$Pick ^ coef.value)) - 50
    # draft.value$Round = (draft.value$Pick - 1) %/% 12 + 1
  } # getting PAR from ADP (useless)
  {
    # adp.model = lm(ADP ~ exp(PAR) + exp(PAW), data = drafted.players)
    # adp.model = lm(ADP ~ PAR + PAW, data = drafted.players)
    # plot(fitted(adp.model), drafted.players$ADP)
    # plot(fitted(adp.model), resid(adp.model))
    # plot(drafted.players$PAR, resid(adp.model))
    # plot(drafted.players$PAW, resid(adp.model))
    # start.vals = list(a = 50, d = 50, b = exp(coef(adp.model)[2]), 
    #           e = exp(coef(adp.model)[3]), g = 0)
    # # start.vals = list(a = coef(adp.model)[2], d = coef(adp.model)[2], b = 1, 
    # #                   e = 1, g = coef(adp.model)[1])
    # # start.vals = list(a = -1/3, b = -2/3, c = 115, d = 0, e = 0)
    # start.vals = list(a = -1, c = 125, d = 1)
    # adp.model = nls(ADP ~ (a * (PAW + d) + c), 
    #                 data = drafted.players, 
    #                 start = start.vals, trace = TRUE)
    drafted.players$Draft.Value = round(1/3 * drafted.players$PAR + 
      2/3 * drafted.players$PAW, 2)
    drafted.players = 
      drafted.players[which(drafted.players$Draft.Value > 0), ]
    adp.model = lm(log(ADP) ~ Draft.Value, data = drafted.players)
    GetValueFromPick = function(draft.pick){
      return(as.numeric((log(draft.pick) - 
                           coef(adp.model)[1]) / coef(adp.model)[2]))
      
    }
    draft.value = data.frame("Pick" = 1:192, "Value" = NA)
    draft.value$Value = GetValueFromPick(draft.value$Pick)
    draft.value$Round = (draft.value$Pick - 1) %/% 12 + 1
    draft.value$Value = draft.value$Value - min(draft.value$Value)
  } # combining PAR and PAW (the way to go)
  clean.proj$True.Pick = round(exp(predict(adp.model, data.frame(
    Draft.Value = 1/3 * clean.proj$PAR + 2/3 * clean.proj$PAW))), 2)
  clean.proj$Steal.Index = round(1/3 * clean.proj$PAR + 2/3 * clean.proj$PAW - 
                                   GetValueFromPick(clean.proj$ADP), 2)
  true.undraft = which(clean.proj$True.Pick > 200)
  clean.proj[true.undraft, "True.Pick"] = 200
  clean.proj[true.undraft, "Steal.Index"] = 
    round(GetValueFromPick(200) - 
            GetValueFromPick(clean.proj[true.undraft, "ADP"]), 2)
  
} # espn ADP
{
  other.positions = c("D/ST", "K")
  pos.id = c(16, 17)
  important.cols = c("RNK", "PLAYER, TEAM POS", "PTS")
  other.proj = data.frame()
  for(i in 1:2){
    pos = other.positions[i]
    espn.url = paste(
      "http://games.espn.com/ffl/tools/projections?leagueId=0&slotCategoryId=", 
      pos.id[i], sep = "")
    espn.data = readHTMLTable(rawToChar(GET(espn.url)$content))[[2]]
    espn.data = FirstRowAsHeader(espn.data)
    espn.data = ConvertFactorData(espn.data)
    espn.data = espn.data[important.cols]
    espn.data$Pos = pos
    other.proj = rbind(other.proj, espn.data)
  }
  other.repl.ranks = c(12, 12)
  other.waiv.ranks = c(16, 14)
  other.repl.pts = numeric(0)
  other.waiv.pts = numeric(0)
  other.proj$PAR = NA
  other.proj$PAW = NA
  other.proj$Name = NA
  for(i in 1:2){
    pos = other.positions[i]
    other.repl.pts[i] = other.proj[which(other.proj$Pos == pos & other.proj$RNK 
                                      == (other.repl.ranks[i] + 1)), "PTS"]
    other.waiv.pts[i] = other.proj[which(other.proj$Pos == pos & other.proj$RNK 
                                         == (other.waiv.ranks[i] + 1)), "PTS"]
  }
  kicker.index = which(other.proj$Pos == "K")
  defense.index = which(other.proj$Pos == "D/ST")
  for(i in kicker.index){
    other.proj[i, "Name"] = 
      strsplit(other.proj[i, "PLAYER, TEAM POS"], ", ")[[1]][1]
  }
  other.proj[defense.index, "Name"] = 
    substr(other.proj[defense.index, "PLAYER, TEAM POS"], 1,
           nchar(other.proj[defense.index, "PLAYER, TEAM POS"]) - 5)
  other.proj[kicker.index, "PAR"] = 
    other.proj[kicker.index, "PTS"] - other.repl.pts[2]
  other.proj[kicker.index, "PAW"] = 
    other.proj[kicker.index, "PTS"] - other.waiv.pts[2]
  other.proj[defense.index, "PAR"] = 
    other.proj[defense.index, "PTS"] - other.repl.pts[1]
  other.proj[defense.index, "PAW"] = 
    other.proj[defense.index, "PTS"] - other.waiv.pts[1]
  
  names(other.proj)[3] = "Pts"
} # defense, kicker projections
{
  cheat.sheet = clean.proj
  cheat.sheet[, c("SD", "Par.Rk")] = NULL
  cheat.sheet = cheat.sheet[which(cheat.sheet$True.Pick < 250 | 
                                    !is.na(cheat.sheet$ADP)), ]
} # cheat sheet
{
  GetFantasyRound = function(pick, teams = 12){
    return((pick - 1) %/% teams + 1)
  }
  UpdateCheatSheet = function(pick){
    lowest.pick = pick * (1 - 0.15)
    pick.avg = (cheat.sheet$ADP + 2 * cheat.sheet$ESPN.Rk) / 3
    new.index = which(pick.avg >= lowest.pick)
    cheat.sheet2 = cheat.sheet[new.index, ]
    cheat.sheet2 = arrange(cheat.sheet2, cheat.sheet2$True.Pick)
    View(cheat.sheet2)
  }
  # some ofther stuff maybe
} # prep functions
{
  important.cols = c("Name", "Pos", "Pts", "PAR", "PAW")
  all.proj = rbind(clean.proj[, important.cols], other.proj[important.cols])
  
  AnalyzeMockDraft = function(draft.number, view.summary = TRUE){
    {
      raw.data = as.data.frame(
        read_excel(paste("/Users/malexk999/Desktop/Miscellaneous/Football/", 
                         "Fantasy Football/Mock Drafts/2018/Mock Draft ", 
                         draft.number, ".xlsx", sep = ""), col_names = FALSE))
      round.headers = grep("ROUND", raw.data[, 1])
      raw.data = raw.data[-c(round.headers), ]
      odd.rows = which(1:nrow(raw.data) %% 2 == 1)
      even.rows = which(1:nrow(raw.data) %% 2 == 0)
      draft.sheet = cbind(raw.data[odd.rows, ], raw.data[even.rows, 1])
      draft.sheet = ConvertFactorData(draft.sheet)
      names(draft.sheet) = c("Pick", "Fant.Tm", "Player", "Pos, NFL.Tm")
      draft.sheet$Round = GetFantasyRound(draft.sheet$Pick)
      draft.sheet$Pos = NA
      draft.sheet$NFL.Tm = NA
      pos.nfltm = strsplit(draft.sheet$`Pos, NFL.Tm`, ", ")
      for(i in 1:nrow(draft.sheet)){
        draft.sheet[i, c("Pos", "NFL.Tm")] = pos.nfltm[[i]]
      }
      draft.sheet$`Pos, NFL.Tm` = NULL
      {
        espn.names = draft.sheet$Player
        espn.names = gsub(" II", "", espn.names)
        espn.names = gsub(" Jr.", "", espn.names)
        espn.names = gsub(" Sr.", "", espn.names)
        espn.names = gsub("J\\.J\\.", "JJ", espn.names)
        espn.names = gsub("Fuller V", "Fuller", espn.names)
        espn.names = gsub(" III", "", espn.names)
        espn.names = gsub("\\*", "", espn.names)
        espn.names = gsub("JJ Smith-Schuster", "JuJu Smith-Schuster", espn.names)
        espn.names = gsub("DJ", "D.J.", espn.names)
        # espn.names = gsub("Mitchell Trubisky", "Mitch Trubisky", espn.names)
        
        # TJJnes?
        draft.sheet$Player = espn.names
      } # name change
    } # import and clean raw data
    {
      kalin.index = match(draft.sheet$Player, all.proj$Name)
      {
        not.found.index = which(is.na(kalin.index))
        not.found.pos = draft.sheet[not.found.index, "Pos"]
        skill.not.found = draft.sheet[not.found.index[which(
          !(not.found.pos %in% c("D/ST", "K")))], "Player"]
      } # not found players
      draft.sheet[, c("Pts", "PAR", "PAW")] = 
        all.proj[kalin.index, c("Pts", "PAR", "PAW")]
    } # looking up player projections
    {
      team.names = unique(draft.sheet$Fant.Tm)
      roster.sums = data.frame("Team" = team.names, "St.Pts" = NA, 
                               "Abs.PAR" = NA, "Abs.PAW" = NA)
      numTeams = length(team.names)
      team.rosters = vector("list", length = numTeams)
      names(team.rosters) = team.names
      all.pos = c(fantasy.positions, other.positions)
      pos.starters = c(1, 2, 2, 1, 1, 1)
      flex.elig = c("RB", "WR", "TE")
      for(i in 1:numTeams){
        index = which(draft.sheet$Fant.Tm == team.names[i])
        roster = draft.sheet[index, ]
        roster$Fant.Tm = NULL
        roster$Starter = FALSE
        for(j in 1:length(all.pos)){
          pos = all.pos[j]
          starter.index = head(which(roster$Pos == pos), pos.starters[j])
          roster[starter.index, "Starter"] = TRUE
        }
        flex.starter = 
          head(which(roster$Pos %in% flex.elig & !roster$Starter), 1)
        roster[flex.starter, "Starter"] = TRUE
        team.rosters[[i]] = roster
        roster.sums[i, "St.Pts"] = sum(roster[which(roster$Starter), "Pts"], 
                                       na.rm = TRUE)
        roster.sums[i, "Abs.PAR"] = sum(abs(roster$PAR), na.rm = TRUE)
        roster.sums[i, "Abs.PAW"] = sum(abs(roster$PAW), na.rm = TRUE)
      }
    } # team rosters
    {
      assign(paste("mock.draft", draft.number, "picks", sep = "."), 
             draft.sheet, envir = .GlobalEnv)
      assign(paste("mock.draft", draft.number, "summary", sep = "."), 
             roster.sums, envir = .GlobalEnv)
      assign(paste("mock.draft", draft.number, "rosters", sep = "."), 
             team.rosters, envir = .GlobalEnv)
    } # assign to global variables
    if(view.summary){
      # df.name = paste("mock.draft", draft.number, "summary", sep = ".")
      # View(get(df.name))
      View(roster.sums)
    }
  }
  {
    # r2.picks = sort(c(24 * (0:7) + 11, 24 * 0:7 + 14))
    # raw.data[which(raw.data$X__1 %in% r2.picks), "X__2"] = "Team R2"
    # excel.file = paste("/Users/malexk999/Desktop/Miscellaneous/Football/", 
    #                    "Fantasy Football/Mock Drafts/2018/Mock Draft ", 
    #                    2, ".xlsx", sep = "")
    # write.xlsx(raw.data, excel.file, row.names = FALSE)
  } # fixing mock draft 2 team name duplicate
} # mock draft analysis
# gronk was ranked 21st in this mock draft
