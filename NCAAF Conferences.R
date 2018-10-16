{
  ESPN_Abbreviations <- as.data.frame(
    read_excel("/Users/malexk999/Desktop/Miscellaneous/Football/NCAAF/ESPN abbreviations.xlsx"))
  ESPN_Abbreviations$Full.Name = paste(ESPN_Abbreviations$ESPN.Name, 
                                       ESPN_Abbreviations$ESPN.Abr)
  addAbbrev = function(team, abbreviation, espn.name = NA){
    if(is.na(espn.name)){
      espn.name = team
    }
    abbreviation = toupper(abbreviation)
    full.name = paste(espn.name, abbreviation, sep = " ")
    new.row = c(team, espn.name, abbreviation, full.name)
    ESPN_Abbreviations <<- rbind(ESPN_Abbreviations, new.row)
  }
  addAbbrev(idaho, "IDAHO")
  addAbbrev(akron, "AKRON")
  addAbbrev(arkansas.st, "ARKST", "Arkansas State")
  addAbbrev(connecticut, "UCONN", "UConn")
  addAbbrev(idaho, "IDAHO")
  addAbbrev(louisiana.monroe, "ULM", "UL Monroe")
  # ut martin?
  addAbbrev(utsa, "UTSA", espn.name = utsa)
  
} # espn abbreviations
standings.urls = c("http://www.espn.com/college-football/standings", 
                   "http://www.espn.com/college-football/standings/_/view/fcs")
fbs.confs = c("American", "ACC", "Big 12", "Big Ten", "Conference USA", 
              "Independents(FBS)", "MAC", "Mountain West", "Pac-12", "SEC", 
              "Sun Belt")
fcs.confs = c("Big Sky", "Big South", "Colonial", "Independents(FCS)", 
              "Ivy League", "MEAC", "Missouri Valley", "Northeast", "Ohio Valley", 
              "Patriot", "Pioneer", "Southern", "Southland", "SWAC")
ncaaf.conf.names = list(fbs.confs, fcs.confs)
ncaaf.conf.info = data.frame()
for(k in 1:2){
  espn.url = standings.urls[k]
  espn.html = rawToChar(GET(espn.url)$content)
  espn.data = readHTMLTable(espn.html)
  good.index = (1:length(ncaaf.conf.names[[k]])) * 4 - 2
  ncaaf.conf.data = espn.data[good.index]
  names(ncaaf.conf.data) = ncaaf.conf.names[[k]]
  for(i in 1:length(ncaaf.conf.data)){
    df = ncaaf.conf.data[[i]]
    names(df) = "Team"
    df$Team = as.character(df$Team)
    conf.name = ncaaf.conf.names[[k]][i]
    df$Conference = conf.name
    df$Div = NA
    # teams.in.conf = sum(ncaaf.confs == conf.name)
    div.index = which(!grepl(" ", df$Team))
    if(!length(div.index) == 2){
      df$Div = "One"
    } else {
      div.names = df[div.index, "Team"]
      df[which(2:nrow(df) < div.index[2]) + 1, "Div"] = div.names[1]
      df[which(1:nrow(df) > div.index[2]), "Div"] = div.names[2]
      df = df[-c(div.index), ]
    }
    kalin.teams = character(nrow(df))
    match.lengths = integer(nrow(df))
    match.char.index = match.lengths + Inf
    # conf.teams = ncaaf.teams[which(ncaaf.confs == conf.name)]
    # espn.names.data = ESPN_Abbreviations[which(ESPN_Abbreviations$Team %in% 
                                                 # conf.teams), ]
    espn.names.data = ESPN_Abbreviations
    repeat.loop = TRUE
    while(repeat.loop){
      full.espn.names = paste(espn.names.data$ESPN.Abr, espn.names.data$ESPN.Name, 
                              sep = "")
      for(j in 1:length(full.espn.names)){
        # match.length = 0
        match.info = gregexpr(full.espn.names[j], df$Team)
        index = which(match.info != -1)
        # match.length = nchar()
        if(length(index) == 1){
          if(nchar(full.espn.names[j]) < match.lengths[index] | 
             match.info[[index]] > match.char.index[index]){
            next()
          }
          kalin.name = espn.names.data[j, "Team"]
          match.lengths[index] = nchar(full.espn.names[j])
          match.char.index[index] = match.info[[index]]
        } else {
          match.info = gregexpr(espn.names.data[j, "ESPN.Abr"], df$Team)
          tryCatch({
            index = which(match.info != -1)
          }, error = function(e){
            for(m in 1:length(match.info)){
              match.info[[m]] = min(match.info[[m]])
            }
            index <<- which(match.info != -1)
          })
          
          if(length(index) == 1){
            if(nchar(espn.names.data[j, "ESPN.Abr"]) < match.lengths[index] | 
               match.info[[index]] > match.char.index[index]){
              next()
            }
            kalin.name = espn.names.data[j, "Team"]
            match.lengths[index] = nchar(espn.names.data[j, "ESPN.Abr"])
            match.char.index[index] = match.info[[index]]
          } else {
            match.info = gregexpr(espn.names.data[j, "ESPN.Name"], df$Team)
            index = which(match.info != -1)
            if(length(index) == 1){
              if(nchar(espn.names.data[j, "ESPN.Name"]) > match.lengths[index] | 
                 match.info[[index]] > match.char.index[index]){
                next()
              }
              kalin.name = espn.names.data[j, "Team"]
              match.lengths[index] = nchar(espn.names.data[j, "ESPN.Name"])
              match.char.index[index] = match.info[[index]]
            }
          }
        }
        if(length(index) == 1){
          kalin.teams[index] = kalin.name 
        } else if(length(index) > 1){
          # print(df[index, "Team"])
        }
        
      } 
      not.found.index = which(kalin.teams == "")
      repeat.loop = length(not.found.index) > 0
      if(nrow(espn.names.data) > 50){
        repeat.loop = FALSE
      }
      if(repeat.loop){
        espn.names.data = ESPN_Abbreviations
      }
    }
    {
      # if(length(not.found.index) > 0){
      #   # espn.names.data = ESPN_Abbreviations[which(ESPN_Abbreviations$Team %in% conf.teams), ]
      #   espn.names.data = ESPN_Abbreviations
      #   full.espn.names = paste(espn.names.data$ESPN.Abr, espn.names.data$ESPN.Name, 
      #                           sep = "")
      #   for(j in 1:length(full.espn.names)){
      #     index = grep(full.espn.names[j], df$Team)
      #     if(length(index) == 1){
      #       kalin.name = espn.names.data[j, "Team"]
      #     } else {
      #       index = grep(espn.names.data[j, "ESPN.Abr"], df$Team)
      #       if(length(index) == 1){
      #         kalin.name = espn.names.data[j, "Team"]
      #       } else {
      #         index = grep(espn.names.data[j, "ESPN.Name"], df$Team)
      #         if(length(index) == 1){
      #           kalin.name = espn.names.data[j, "Team"]
      #         }
      #       }
      #     }
      #     if(isTRUE(index %in% not.found.index)){
      #       kalin.teams[index] = kalin.name
      #     }
      #     
      #   }
      # }
    } # another iteration
    not.found.index = which(kalin.teams == "")
    if(length(not.found.index) > 0){
      print(df[not.found.index, "Team"])
    }
    df$Team = kalin.teams
    ncaaf.conf.info = rbind(ncaaf.conf.info, df)
    
  }
}
duplicate.teams = NULL
for(i in 1:nrow(ncaaf.conf.info)){
  if(sum(ncaaf.conf.info$Team == ncaaf.conf.info[i, "Team"]) > 1){
    duplicate.teams = append(duplicate.teams, ncaaf.conf.info[i, "Team"])
  }
}
duplicate.teams = unique(duplicate.teams)
ncaaf.conf.info = arrange(ncaaf.conf.info, ncaaf.conf.info$Team)
file.name = paste("/Users/malexk999/Desktop/Miscellaneous/Football/NCAAF/", 
                  "NCAAF Teams and Confs.xlsx", sep = "")
write.xlsx(ncaaf.conf.info, file.name, row.names = FALSE)


