{
  library(httr)
  library(XML)
  library(xlsx)
  library(readxl)
  library(boot)
  library(plyr)
  RemoveHtml = function(data.text){
    open.tag.end = gregexpr(">", data.text)[[1]]
    close.tag.start = max(gregexpr("</", data.text)[[1]])
    html.start = min(gregexpr("<", data.text)[[1]])
    html.end = max(gregexpr(">", data.text)[[1]])
    if(html.start > 1){
      text.before = substr(data.text, 1, html.start - 1)
    } else {
      text.before = ""
    }
    if(html.end < nchar(data.text)){
      text.after = substr(data.text, html.end + 1, nchar(data.text))
    } else {
      text.after = ""
    }
    inner.html.text = ""
    for(cts in close.tag.start){
      ote = max(open.tag.end[which(open.tag.end < cts)])
      inner.html.text = paste(inner.html.text, substr(
        data.text, ote + 1, cts - 1), sep = "")
    }
    data.text = paste(text.before, inner.html.text, text.after, sep = "")
    return(data.text)
  }
  IsSingleValue = function(x){
    return(length(unique(x)) == 1)
  }
  CreateProjectionModel = function(ind.cols, dep.col, df, alpha = 0.20){
    model.formula = 
      as.formula(paste(dep.col, "~", paste(ind.cols, collapse = "+")))
    model = lm(model.formula, data = df)
    coef.df = as.data.frame(summary(model)$coefficients) 
    p.vals = coef.df$`Pr(>|t|)`
    coef.vals = coef.df$Estimate
    sig.cols = which(p.vals <= alpha & coef.vals > 0) - 1 # col 0 is intercept
    if(sum(sig.cols) == sum(1:length(ind.cols))){
      return(model)
    } else {
      return(CreateProjectionModel(head(ind.cols, -1), dep.col, df, alpha))
    }
  }
  GetDraftValue = function(draft.pick, position){
    # return(draft.pick ^ (-0.3) + 7)
    avg.av = rookie.avg.av[[position]]
    avg.pick = rookie.avg.pick[[position]]
    base.model.value = avg.pick ^ (-0.3) + 7
    multiplier = avg.av / base.model.value
    proj.av = (draft.pick ^ (-0.3) + 7) * multiplier
    return(proj.av)
  }
  GetInterceptWgt = function(nfl.yrs){
    # returns relative weight of draft intercept
    # 0 experience --> 1
    # 1 experience --> like 0.8
    # 5 experience --> 1/3
    # 10 exp --> close to 0
    return(exp(-nfl.yrs/5)) # work in progress
  }
  ProjectAV = function(position, hist.av, hist.adj.starts, draft.pick, nfl.yrs,
                       av.per.szn = FALSE){
    draft.value = GetDraftValue(draft.pick, position)
    # divide this by (1 - (sum of prev yr wgts from model))
    with.draft.model = models.with.draft[[position]]
    draft.intercept = as.numeric(
      (coef(with.draft.model)[1] + coef(with.draft.model)[2] * draft.value) / 
      (1 - sum(tail(coef(with.draft.model), -2))))
    hist.wgts = c(intercept.wgt[[position]], prev.yrs.wgts[[position]])
    prev.yrs.needed = length(hist.wgts) - 1
    # make the intercept weight include 12 adjusted starts
    if(length(hist.av) < prev.yrs.needed){
      hist.av = c(hist.av, rep(NA, prev.yrs.needed - length(hist.av)))
      hist.adj.starts = 
        c(hist.adj.starts, rep(NA, prev.yrs.needed - length(hist.adj.starts)))
    } else if(length(hist.av) > prev.yrs.needed){
      hist.av = head(hist.av, prev.yrs.needed)
      hist.adj.starts = head(hist.adj.starts, prev.yrs.needed)
    }
    draft.wgt = GetInterceptWgt(nfl.yrs)
    intercept.value = weighted.mean(c(draft.intercept, all.avg.av[[position]]), 
                                    c(draft.wgt, 1 - draft.wgt))
    if(!av.per.szn){
      hist.av = hist.av / hist.adj.starts * 16
    }
    hist.adj.starts = c(12, hist.adj.starts)
    numerator = c(intercept.value, hist.av)
    wgts = hist.wgts * hist.adj.starts
    proj.av.szn = weighted.mean(numerator, wgts, na.rm = TRUE)
    return(proj.av.szn)
  }
  DraftStPct = function(draft.pick, position){
    return(predict(rookie.stpct.models[[position]], 
                   data.frame(Draft.Pick = draft.pick)))
  }
  KickerStPct = function(){
    return(0)
  }
  ProjectStPct = function(position, hist.stpct, hist.games, draft.pick, nfl.yrs, 
                          total.starts = TRUE){
    if(position == "KP"){
      return(KickerStPct())
    }
    undrafted.rookie = nfl.yrs == 0 & draft.pick == 275
    if(undrafted.rookie){
      return(0.05)
    }
    draft.value = DraftStPct(draft.pick, position)
    with.draft.model = stpct.models.draft[[position]]
    draft.intercept = as.numeric(
      (coef(with.draft.model)[1] + coef(with.draft.model)[2] * draft.value) / 
        (1 - sum(tail(coef(with.draft.model), -2))))
    hist.wgts = c(stpct.int.wgt[[position]], stpct.wgts[[position]])
    prev.yrs.needed = length(hist.wgts) - 1
    if(length(hist.stpct) < prev.yrs.needed){
      hist.stpct = c(hist.stpct, rep(NA, prev.yrs.needed - length(hist.stpct)))
      hist.games = c(hist.games, rep(NA, prev.yrs.needed - length(hist.games)))
    } else if(length(hist.stpct) > prev.yrs.needed){
      hist.stpct = head(hist.stpct, prev.yrs.needed)
      hist.games = head(hist.games, prev.yrs.needed)
    }
    draft.wgt = GetInterceptWgt(nfl.yrs)
    intercept.value = weighted.mean(c(draft.intercept, avg.stpct[[position]]), 
                                    c(draft.wgt, 1 - draft.wgt))
    if(total.starts){
      hist.stpct = hist.stpct / hist.games
    }
    hist.games = c(12, hist.games)
    numerator = c(intercept.value, hist.stpct)
    wgts = hist.wgts * hist.games
    proj.stpct = weighted.mean(numerator, wgts, na.rm = TRUE)
    if(nfl.yrs == 0){
      proj.stpct = proj.stpct / 2
    }
    return(proj.stpct)
  }
  DraftGames = function(draft.pick, position){
    return(predict(rookie.games.models[[position]], 
                   data.frame(Draft.Pick = draft.pick)))
  }
  ProjectGames = function(position, hist.games, draft.pick, nfl.yrs){
    draft.value = DraftGames(draft.pick, position)
    with.draft.model = games.models.draft[[position]]
    draft.intercept = as.numeric(
      (coef(with.draft.model)[1] + coef(with.draft.model)[2] * draft.value) / 
                                   (1 - sum(tail(coef(with.draft.model), -2))))
    hist.wgts = c(games.int.wgt[[position]], games.played.wgts[[position]])
    prev.yrs.needed = length(hist.wgts) - 1
    if(length(hist.games) < prev.yrs.needed){
      hist.games = c(hist.games, rep(NA, prev.yrs.needed - length(hist.games)))
    } else if(length(hist.games) > prev.yrs.needed){
      hist.games = head(hist.games, prev.yrs.needed)
    }
    draft.wgt = GetInterceptWgt(nfl.yrs)
    intercept.value = weighted.mean(c(draft.intercept, 
                                      avg.games.played[[position]]), 
                                    c(draft.wgt, 1 - draft.wgt))
    numerator = c(intercept.value, hist.games)
    wgts = hist.wgts
    proj.games = weighted.mean(numerator, wgts, na.rm = TRUE)
    return(proj.games)
  }
  PredictRealStPct = function(position, proj.stpct, proj.av.szn, proj.games, 
                              ceiling = TRUE, floor = TRUE){
    if(position == "KP"){
      return(KickerStPct())
    }
    proj.av1 = proj.games * (1 + 5 * proj.stpct) / 6 * proj.av.szn / 16
    model = nonlin.stpct.models[[position]]
    proj.real.stpct = as.numeric(predict(model, data.frame(
      Pred.StPct = proj.stpct, Pred.Av.Rate = proj.av.szn, Pred.AV1 = proj.av1)))
    # if(ceiling){
    #   proj.real.stpct = min(1, proj.real.stpct)
    # }
    # if(floor){
    #   proj.real.stpct = max(0, proj.real.stpct)
    # }
    # logistic formula so must be between 0 and 1
    return(proj.real.stpct)
  }
  SeeProjRoster = function(team){
    View(nfl.proj.rosters[[team]])
  }
  position = "QB"
  hist.games = c(16, 16, 16, 16)
  draft.pick = 24
  nfl.yrs = 13
  GetDraftRound = function(pick, teams = 32){
    return((pick - 1) %/% teams + 1)
  }
  ReadHtmlTableKalin = function(html){
    table.start = gregexpr("<table", html)[[1]]
    table.end = gregexpr("</table", html)[[1]]
    table.html = substr(html, table.start, table.end)
    
    header.start = regexpr("<thead", table.html)
    header.end = regexpr("</thead", table.html)
    header.html = substr(table.html, header.start, header.end)
    header.data.starts = gregexpr("<th ", header.html)[[1]]
    header.data.ends = gregexpr("</th", header.html)[[1]]
    header.names = character(length = length(header.data.starts))
    for(j in 1:length(header.data.starts)){
      header.data.html = substr(header.html, header.data.starts[j] + 1, 
                                header.data.ends[j])
      open.tag.end = max(gregexpr(" >", header.data.html)[[1]])
      close.tag.start = max(gregexpr("<", header.data.html)[[1]])
      header.names[j] = substr(header.data.html, open.tag.end + 2, 
                               close.tag.start - 1)
    }
    for(esc.char in html.escape.chars){
      has.esc.char = regexpr(esc.char, header.names)
      for(j in which(has.esc.char != -1)){
        header.names[j] = substr(header.names[j], 1, has.esc.char[j] - 1)
      }
    }
    row.starts = sort(c(gregexpr("<tr ", table.html)[[1]], 
                        gregexpr("<tr>", table.html)[[1]]))
    row.ends = gregexpr("</tr", table.html)[[1]]
    if(length(row.starts) != length(row.ends)){
      if(length(row.starts) - 1 == length(row.ends)){
        if(row.starts[1] == -1){
          row.starts = row.starts[-c(1)]
        } else {
          stop("Row closings does not equal row openings")
        }
        
      }
    }
    row.starts = row.starts[-c(1, length(row.starts))]
    row.ends = row.ends[-c(1, length(row.ends))] # minus header and footer
    num.rows = length(row.starts) 
    roster.table = as.data.frame(matrix(NA, ncol = length(header.names), 
                                        nrow = num.rows))
    names(roster.table) = header.names
    for(row in 1:num.rows){
      row.html = substr(table.html, row.starts[row], row.ends[row])
      row.data.start = gregexpr("<td", row.html)[[1]]
      row.data.end = gregexpr("</td", row.html)[[1]]
      has.th = grepl("</th", row.html)
      if(has.th){
        row.data.start = 
          sort(c(row.data.start, gregexpr("<th", row.html)[[1]]))
        row.data.end = sort(c(row.data.end, gregexpr("</th", row.html)[[1]]))
      }
      if(length(row.data.start) != length(row.data.end)){
        stop("Data openeings does not equal data closings")
      } else if (length(row.data.start) != length(header.names)){
        stop("Number of cells in row does not equal number of cols")
      }
      for(j in 1:length(header.names)){
        row.data.html = substr(row.html, row.data.start[j], row.data.end[j])
        open.tag.end = max(gregexpr(" >", row.data.html)[[1]])
        close.tag.start = max(gregexpr("<", row.data.html)[[1]])
        data.text = substr(row.data.html, open.tag.end + 2, 
                           close.tag.start - 1)
        while(grepl("</", data.text)){
          # open.tag.end = gregexpr(">", data.text)[[1]]
          # close.tag.start = max(gregexpr("</", data.text)[[1]])
          # open.tag.end = max(open.tag.end[which(open.tag.end < 
          #                                         close.tag.start)])
          # data.text = substr(data.text, open.tag.end + 1, 
          #                    close.tag.start - 1)
          data.text = RemoveHtml(data.text)
        }
        roster.table[row, j] = data.text
      }
    }
    for(col in 1:ncol(roster.table)){
      tryCatch({
        roster.table[, col] = as.numeric(roster.table[, col])
      }, warning = function(w){
        # do nothing
      })
    }
    return(roster.table)
  }
  {
    position.classif = vector("list", length = length(position.groups))
    names(position.classif) = position.groups
    position.classif$QB = "QB"
    position.classif$RB = c("RB", "FB", "HB")
    position.classif$WR = c("WR", "FL", "SE")
    position.classif$TE = "TE"
    position.classif$OL = c("LT", "LG", "C", "RG", "RT", "T", "G", "RT ", "OL", 
                            "OG", "OT", "OL", "LS") 
    # long snapper or left safey?
    position.classif$F7 = c("DE", "DT", "RDT", "LDT", "NT", "DL", 
                            "MLB", "LB", "OLB", "LOLB", "ROLB", "LILB", "RILB", 
                            "ILB")
    position.classif$DB = c("CB", "S", "FS", "SS", "RCB", "LCB", "DB")
    position.classif$KP = c("K", "P")
    for(i in 1:length(position.classif)){
      position.classif[[i]] = c(position.classif[[i]], 
                                tolower(position.classif[[i]]))
    }
  }
  GetPosition = function(position){
    real.position = NA
    if(grepl("/", position)){
      position = strsplit(position, "/")[[1]][1]
    }
    if(grepl(",", position)){
      position = strsplit(position, ",")[[1]][1]
    }
    for(pos in position.groups){
      if(position %in% position.classif[[pos]]){
        real.position = pos
      }
    }
    return(real.position)
  }
} # packages and functions
season = 2018
{
  team.av.data = data.frame("Team" = rep(nfl.teams, length(seasons)), 
                            "Season" = rep(seasons, each = nNflTeams), 
                            "ELO.Rate" = NA, stringsAsFactors = FALSE)
  {
    pfr.tms = substr(nfl.teams, 1, 3)
    pfr.tms[grep(" ", pfr.tms)] = c("SFO", "TAM", "SDG", "KAN", "NYG", "NYJ", 
                                    "GNB", "NWE", "RAM", "NOR")
    pfr.tms = gsub("ARI", "CRD", pfr.tms)
    pfr.tms = gsub("BAL", "RAV", pfr.tms)
    pfr.tms = gsub("HOU", "HTX", pfr.tms)
    pfr.tms = gsub("IND", "CLT", pfr.tms)
    pfr.tms = gsub("OAK", "RAI", pfr.tms)
    # pfr.tms = gsub("STL", "RAM", pfr.tms)
    pfr.tms = gsub("TEN", "OTI", pfr.tms)
  } # pfr teams
  seasons = (season - 1):2002
  nfl.hist.rosters = vector("list", length = nNflTeams)
  names(nfl.hist.rosters) = nfl.teams
  for(i in 1:nNflTeams){
    nfl.hist.rosters[[i]] = vector("list", length = length(seasons))
    names(nfl.hist.rosters[[i]]) = seasons
  }
  html.escape.chars = c("&nbsp")
  for(year in seasons){
    year.index = which(team.av.data$Season == year)
    col.name = paste("Kal", year, sep = ".")
    team.av.data[year.index, "ELO.Rate"] = nfl.hist.ratings[, col.name]
    for(i in 1:nNflTeams){
      pfr.url = paste("http://www.pro-football-reference.com/teams/",
                      tolower(pfr.tms[i]), "/", year, "_roster.htm", sep = "")
      pfr.html = rawToChar(GET(pfr.url)$content)
      # pfr.data = readHTMLTable(pfr.html)
      # roster.table = pfr.data
      # roster.table = data.frame()
      table.start = gregexpr("<table", pfr.html)[[1]][2]
      table.end = gregexpr("</table", pfr.html)[[1]][2]
      table.html = substr(pfr.html, table.start, table.end)
      
      header.start = regexpr("<thead", table.html)
      header.end = regexpr("</thead", table.html)
      header.html = substr(table.html, header.start, header.end)
      header.data.starts = gregexpr("<th ", header.html)[[1]]
      header.data.ends = gregexpr("</th", header.html)[[1]]
      header.names = character(length = length(header.data.starts))
      for(j in 1:length(header.data.starts)){
        header.data.html = substr(header.html, header.data.starts[j] + 1, 
                                  header.data.ends[j])
        open.tag.end = max(gregexpr(" >", header.data.html)[[1]])
        close.tag.start = max(gregexpr("<", header.data.html)[[1]])
        header.names[j] = substr(header.data.html, open.tag.end + 2, 
                                 close.tag.start - 1)
      }
      for(esc.char in html.escape.chars){
        has.esc.char = regexpr(esc.char, header.names)
        for(j in which(has.esc.char != -1)){
          header.names[j] = substr(header.names[j], 1, has.esc.char[j] - 1)
        }
      }
      row.starts = sort(c(gregexpr("<tr ", table.html)[[1]], 
                          gregexpr("<tr>", table.html)[[1]]))
      row.ends = gregexpr("</tr", table.html)[[1]]
      if(length(row.starts) != length(row.ends)){
        if(length(row.starts) - 1 == length(row.ends)){
          if(row.starts[1] == -1){
            row.starts = row.starts[-c(1)]
          } else {
            stop("Row closings does not equal row openings")
          }
          
        }
      }
      row.starts = row.starts[-c(1, length(row.starts))]
      row.ends = row.ends[-c(1, length(row.ends))] # minus header and footer
      num.rows = length(row.starts) 
      roster.table = as.data.frame(matrix(NA, ncol = length(header.names), 
                                          nrow = num.rows))
      names(roster.table) = header.names
      for(row in 1:num.rows){
        row.html = substr(table.html, row.starts[row], row.ends[row])
        row.data.start = gregexpr("<td", row.html)[[1]]
        row.data.end = gregexpr("</td", row.html)[[1]]
        has.th = grepl("</th", row.html)
        if(has.th){
          row.data.start = 
            sort(c(row.data.start, gregexpr("<th", row.html)[[1]]))
          row.data.end = sort(c(row.data.end, gregexpr("</th", row.html)[[1]]))
        }
        if(length(row.data.start) != length(row.data.end)){
          stop("Data openeings does not equal data closings")
        } else if (length(row.data.start) != length(header.names)){
          stop("Number of cells in row does not equal number of cols")
        }
        for(j in 1:length(header.names)){
          row.data.html = substr(row.html, row.data.start[j], row.data.end[j])
          open.tag.end = max(gregexpr(" >", row.data.html)[[1]])
          close.tag.start = max(gregexpr("<", row.data.html)[[1]])
          data.text = substr(row.data.html, open.tag.end + 2, 
                             close.tag.start - 1)
          while(grepl("</", data.text)){
            # open.tag.end = gregexpr(">", data.text)[[1]]
            # close.tag.start = max(gregexpr("</", data.text)[[1]])
            # open.tag.end = max(open.tag.end[which(open.tag.end < 
            #                                         close.tag.start)])
            # data.text = substr(data.text, open.tag.end + 1, 
            #                    close.tag.start - 1)
            data.text = RemoveHtml(data.text)
          }
          roster.table[row, j] = data.text
        }
      }
      for(col in 1:ncol(roster.table)){
        tryCatch({
          roster.table[, col] = as.numeric(roster.table[, col])
        }, warning = function(w){
          # do nothing
        })
      }
      roster.table$Player = gsub("\\*", "", roster.table$Player)
      roster.table$Player = gsub("\\+", "", roster.table$Player)
      nfl.hist.rosters[[i]][[as.character(year)]] = roster.table
    }
  } # getting raw data from PFR
  {
    position.types = c("Pass", "Rush", "Rec", "OLine", "DLine", "LB", "Fr7", 
                       "Sec", "Blank", "Off", "Def", "SpTm", "Total")
    position.classif = vector("list", length = length(position.types))
    names(position.classif) = position.types
    position.classif$Pass = "QB"
    position.classif$Rush = c("RB", "FB")
    position.classif$Rec = c("WR", "TE", "FL", "SE")
    position.classif$OLine = c("LT", "LG", "C", "RG", "RT", "T", "G", "RT ", "OL")
    position.classif$DLine = c("DE", "DT", "RDT", "LDT", "NT", "DL")
    position.classif$LB = c("MLB", "LB", "OLB", "LOLB", "ROLB", "LILB", "RILB")
    position.classif$Sec = c("CB", "S", "FS", "SS", "RCB", "LCB", "DB")
    position.classif$Off = "K"
    position.classif$Def = "P"
    position.classif$Blank = ""
    offense.types = c("Pass", "Rush", "Rec", "OLine")
    front7.types = c("DLine", "LB")
    defense.types = c("Fr7", "Sec")
    all.units = c("Off", "Def", "Blank")
    for(i in offense.types){
      position.classif$Off = c(position.classif$Off, 
                               as.character(position.classif[[i]]))
    }
    for(i in front7.types){
      position.classif$Fr7 = c(position.classif$Fr7, 
                               as.character(position.classif[[i]]))
    }
    for(i in defense.types){
      position.classif$Def = c(position.classif$Def, 
                               as.character(position.classif[[i]]))
    }
    position.classif$SpTm = c("K", "P")
    for(i in all.units){
      position.classif$Total = c(position.classif$Total, 
                                 as.character(position.classif[[i]]))
    }
    for(i in 1:length(position.classif)){
      position.classif[[i]] = c(position.classif[[i]], 
                                tolower(position.classif[[i]]))
    }
  } # pfr positions
  team.av.data$Total.AV = NA
  team.av.data[, paste(position.types, "AV", sep = ".")] = NA
  missing.positions = NULL
  for(i in 1:nrow(team.av.data)){
    team = team.av.data[i, "Team"]
    year = team.av.data[i, "Season"]
    roster.table = nfl.hist.rosters[[team]][[as.character(year)]]
    # team.av.data[i, "Total.AV"] = sum(roster.table$AV, na.rm = TRUE)
    for(j in position.types){
      index = which(roster.table$Pos %in% position.classif[[j]])
      col.name = paste(j, "AV", sep = ".")
      team.av.data[i, col.name] = sum(roster.table[index, "AV"], na.rm = TRUE)
      if(j == "Total"){
        missing.index = setdiff(1:nrow(roster.table), index)
        missing.positions = append(missing.positions, 
                                   roster.table[missing.index, "Pos"])
      }
    }
  } # filling in position/unit AV
  # unique(missing.positions)
  off.ratio = team.av.data$Off.AV / team.av.data$Def.AV
  def.blank = round(team.av.data$Blank.AV * off.ratio / (off.ratio + 1))
  team.av.data$Def.AV = team.av.data$Def.AV + def.blank
  team.av.data$Off.AV = team.av.data$Off.AV + team.av.data$Blank.AV - def.blank
  # same off/def --> 50/50 split from blanks
  # off = 2* def --> blanks 2/3 def, 1/3 off
  
} # team
{
  get.player.av.data = FALSE
  {
    # all.positions = 
    #   position.classif$Total[1:(match("", position.classif$Total) - 1)]
    pfr.positions = c("QB", "RB", "WR", "TE", "OL", "DL", "LB", "DB", "K", "P")
    if(get.player.av.data){
      player.av.data = data.frame()
      important.cols = c("Player", "Year", "Age", "Draft", "Tm", "G", "GS", "AV", 
                         "Pos")
      for(year in seasons){
        for(pos in pfr.positions){
          repeat.loop = TRUE
          page.number = 1
          while(repeat.loop){
            offset.number = (page.number - 1) * 100
            pfr.url = paste("https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=", 
                            year, "&year_max=", year, "&season_start=1&season_end=-1&pos%5B%5D=", 
                            tolower(pos), "&draft_year_min=1936&draft_year_max=2018&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c1stat=g&c1comp=gt&c1val=2&c5val=1.0&order_by=av", 
                            "&offset=", offset.number, sep = "")
            pfr.html = rawToChar(GET(pfr.url)$content)
            tryCatch({
              pfr.table = readHTMLTable(pfr.html)[[1]]
              table.readable = TRUE
            }, error = function(e){
              table.readable <<- FALSE
            })
            if(!table.readable){
              break()
            }
            header.rows = which(pfr.table$Rk == "Rk")
            if(length(header.rows) > 0){
              pfr.table = pfr.table[-c(header.rows), ]
            }
            pfr.table = ConvertFactorData(pfr.table)
            pfr.table$Pos = pos
            pfr.table = pfr.table[, important.cols]
            player.av.data = rbind(player.av.data, pfr.table)
            repeat.loop = nrow(pfr.table) == 100
            page.number = page.number + 1
          }
        }
      }
      write.xlsx(player.av.data, 
                 "/Users/malexk999/Desktop/Miscellaneous/Football/NFL Ratings/AV data.xlsx", 
                 row.names = FALSE)
      rownames(player.av.data) = 1:nrow(player.av.data)
    } else {
      player.av.data = as.data.frame(
        read_excel("/Users/malexk999/Desktop/Miscellaneous/Football/NFL Ratings/AV data.xlsx"))
      player.av.data[which(is.na(player.av.data$Draft)), "Draft"] = ""
    }
  } # getting the raw data from PFR (this takes a while)
  {
    grouped.pos = names(position.classif)[c(1:6, 8)]
    gp.pfr.names = c("QB", "RB", "WR", "OL", "DL", "LB", "DB")
    
    player.year = paste(player.av.data$Player, player.av.data$Year)
    duplicate.players = NULL
    for(i in 1:length(player.year)){
      if(length(which(player.year == player.year[i])) > 1){
        duplicate.players = append(duplicate.players, player.year[i])
      }
    }
    duplicate.players = unique(duplicate.players)
    # diff.players.same.name = NULL
    # switched.teams = NULL
    # fixed.duplicate.players = NULL
    other.pfr.tms = c("LAC", "ARI", "LAR", "IND", "OAK", "TEN", "BAL", 
                      "HOU", "STL")
    other.kalin.tms = c(la.chargers, ari.cardinals, la.rams, ind.colts, 
                        oak.raiders, ten.titans, bal.ravens, hou.texans, la.rams)
    bad.index = NULL
    suffix.letters = c("", LETTERS)
    
    # brandon williams, kyle fuller, darrell williams, chris jones 
    for(i in 1:length(duplicate.players)){
      words = strsplit(duplicate.players[i], " ")[[1]]
      year = tail(words, n=1)
      player = paste(setdiff(words, year), collapse = " ")
      pfr.index = which(player.av.data$Player == player & 
                      player.av.data$Year == year)
      pfr.data = player.av.data[pfr.index, ]
      age.draft.tm = paste(pfr.data$Age, pfr.data$Draft, pfr.data$Tm)
      each.adt = unique(age.draft.tm)
      num.players = length(each.adt)
      adt.index = match(age.draft.tm, each.adt)
      new.pfr.index = numeric(length = num.players)
      for(j in 1:num.players){
        this.pfr.index = pfr.index[which(adt.index == j)]
        if(length(this.pfr.index) > 1){
          pfr.team = pfr.data[match(j, adt.index), "Tm"]
          kalin.team = nfl.teams[match(pfr.team, pfr.tms)]
          if(is.na(kalin.team)){
            if(pfr.team %in% other.pfr.tms){
              kalin.team = other.kalin.tms[match(pfr.team, other.pfr.tms)]
            } 
          }
          if(!is.na(kalin.team)){
            roster.table = nfl.hist.rosters[[kalin.team]][[as.character(year)]]
            roster.index = match(player, roster.table$Player)
            if(is.na(roster.index)){
              player.position = ""
            } else {
              player.position = toupper(roster.table[roster.index, "Pos"])
            }
            
          } else {
            player.position = ""
          }
          if(player.position == "") {
            this.yr.pos = pfr.data$Pos
            all.index = which(player.av.data$Player == player & 
                                player.av.data$Draft == pfr.data[1, "Draft"])
            all.pos = player.av.data[all.index, "Pos"]
            pos.freq = numeric(length = length(this.yr.pos))
            for(k in 1:length(this.yr.pos)){
              pos.freq[k] = sum(all.pos == this.yr.pos[k])
            }
            player.position = this.yr.pos[match(max(pos.freq), pos.freq)]
            # other.pfr.tms = append(other.pfr.tms, pfr.team)
          } else {
            roster.table = nfl.hist.rosters[[kalin.team]][[as.character(year)]]
            roster.index = match(player, roster.table$Player)
            player.position = toupper(roster.table[roster.index, "Pos"])
          }
          real.pfr.index = this.pfr.index[which(
            player.av.data[this.pfr.index, "Pos"] == player.position)]
          if(length(real.pfr.index) == 0){
            for(k in 1:length(grouped.pos)){
              if(player.position %in% position.classif[[grouped.pos[k]]]){
                player.position = gp.pfr.names[k]
              }
            }
            real.pfr.index = this.pfr.index[which(
              player.av.data[this.pfr.index, "Pos"] == player.position)]
          }
          new.pfr.index[j] = real.pfr.index
          fake.pfr.index = setdiff(this.pfr.index, real.pfr.index)
          if(length(fake.pfr.index) > 0){
            # player.av.data = player.av.data[-c(fake.pfr.index), ]
            bad.index = append(bad.index, fake.pfr.index)
          }
        } else {
          new.pfr.index[j] = this.pfr.index
        }
        
      }
      
      pfr.data = player.av.data[new.pfr.index, ]
      career.av = numeric(length = num.players)
      for(j in 1:num.players){
        his.index = which(player.av.data$Player == player & 
                            player.av.data$Draft == pfr.data[j, "Draft"])
        career.av[j] = sum(player.av.data[his.index, "AV"])
      }
      av.ranks = num.players - rank(career.av, ties.method = "last") + 1
      last.char = substr(player, nchar(player), nchar(player))
      if(last.char %in% as.character(0:9)){
        # player = gsub(paste(" ", last.char, sep = ""), "", player)
        suffix = suffix.letters[av.ranks]
        # suffix = gsub(" ", "", suffix)
      } else {
        suffix = paste(" ", av.ranks, sep = "")
        suffix = gsub(" 1", "", suffix)
      }
      for(j in 1:num.players){
        his.index = new.pfr.index[j]
        player.av.data[his.index, "Player"] = 
          paste(player, suffix[j], sep = "")
      }
      
      
    }
    {
      # is.same.player = IsSingleValue(pfr.data$Age) &
      #   IsSingleValue(pfr.data$Draft) & IsSingleValue(pfr.data$Tm)
      # if(is.same.player){
      #   pfr.team = pfr.data[1, "Tm"]
      #   kalin.team = nfl.teams[match(pfr.team, pfr.tms)]
      #   if(is.na(kalin.team)){
      #     if(pfr.team %in% other.pfr.tms){
      #       kalin.team = other.kalin.tms[match(pfr.team, other.pfr.tms)]
      #     } 
      #   }
      #   if(is.na(kalin.team)) {
      #     switched.teams = append(switched.teams, duplicate.players[i])
      #     this.yr.pos = pfr.data$Pos
      #     all.index = which(player.av.data$Player == player & 
      #                         player.av.data$Draft == pfr.data[1, "Draft"])
      #     all.pos = player.av.data[all.index, "Pos"]
      #     pos.freq = numeric(length = length(this.yr.pos))
      #     for(j in 1:length(this.yr.pos)){
      #       pos.freq[j] = sum(all.pos == this.yr.pos[j])
      #     }
      #     player.position = all.pos[match(max(pos.freq), pos.freq)]
      #     # other.pfr.tms = append(other.pfr.tms, pfr.team)
      #   } else {
      #     roster.table = nfl.hist.rosters[[kalin.team]][[as.character(year)]]
      #     roster.index = match(player, roster.table$Player)
      #     player.position = toupper(roster.table[roster.index, "Pos"])
      #   }
      #   real.pfr.index = pfr.index[which(pfr.data$Pos == player.position)]
      #   fake.pfr.index = setdiff(pfr.index, real.pfr.index)
      #   if(length(fake.pfr.index) > 0){
      #     player.av.data = player.av.data[-c(fake.pfr.index), ]
      #     if(length(real.pfr.index) == 1){
      #       fixed.duplicate.players = append(fixed.duplicate.players, 
      #                                        duplicate.players[i])
      #     }
      #   }
      # } 
      # if(!is.same.player) {
      #   # diff.players.same.name = 
      #   #   append(diff.players.same.name, duplicate.players[i])
      #   # extra.players = length(pfr.index) - 1
      #   total.players = length(pfr.index)
      #   career.av = numeric(length = total.players)
      #   for(j in 1:total.players){
      #     his.index = which(player.av.data$Player == player & 
      #                         player.av.data$Draft == pfr.data[j, "Draft"])
      #     career.av[j] = sum(player.av.data[his.index, "AV"])
      #   }
      #   av.ranks = total.players - rank(career.av, ties.method = "last") + 1
      #   suffix = paste(" ", av.ranks, sep = "")
      #   suffix = gsub(" 1", "", suffix)
      #   for(j in 1:total.players){
      #     his.index = which(player.av.data$Player == player & 
      #                         player.av.data$Draft == pfr.data[j, "Draft"])
      #     player.av.data[his.index, "Player"] = 
      #       paste(player, suffix[j], sep = "")
      #   }
      #   fixed.duplicate.players = append(fixed.duplicate.players, 
      #                                    duplicate.players[i])
      # }
      # fixed.index = which(duplicate.players %in% fixed.duplicate.players)
      # if(length(fixed.index) > 0){
      #   duplicate.players = duplicate.players[-c(fixed.index)]
      # }
      # player.year = paste(player.av.data$Player, player.av.data$Year)
      # duplicate.players = NULL
      # for(i in 1:length(player.year)){
      #   if(length(which(player.year == player.year[i])) > 1){
      #     duplicate.players = append(duplicate.players, player.year[i])
      #   }
      # }
      # duplicate.players = unique(duplicate.players)
      # print(duplicate.players)
    } # old code
    
    
    if(length(bad.index) > 0){
      player.av.data = player.av.data[-c(bad.index), ]
    }
    
    rownames(player.av.data) = 1:nrow(player.av.data)
  } # duplicate players (WORKS!)
  {
    player.av.data$Adj.St = 
      round((player.av.data$G + 5 * player.av.data$GS) / 6, 2)
    player.av.data$Av.Szn = player.av.data$AV / player.av.data$Adj.St * 16
    player.av.data$Draft.Pick = NA
    draft.info = strsplit(player.av.data$Draft, "-")
    for(i in 1:nrow(player.av.data)){
      if(length(draft.info[[i]] == 2)){
        round = as.numeric(draft.info[[i]][[1]])
        draft.pick = as.numeric(draft.info[[i]][2])
        if(round > 1 & draft.pick < 32){ # supplemental draft
          draft.pick = NA
        }
      } else if(length(draft.info[[i]]) == 0){
        draft.pick = NA
      } else {
        player.av.data[i, "Draft"]
        next()
      }
      player.av.data[i, "Draft.Pick"] = draft.pick
    }
    # player.av.data$Draft.Pick = as.numeric(player.av.data$Draft.Pick)
    undrafted.value = 275
    player.av.data[which(is.na(player.av.data$Draft.Pick) | 
                           player.av.data$Draft.Pick > undrafted.value), 
                   "Draft.Pick"] = undrafted.value
  } # projessing the raw data
  {
    {
      prev.yrs = 4
      min.adj.start = 7
      min.yr = min(seasons) + prev.yrs
      other.cols = 3:(prev.yrs + 3)
      position.groups = c("QB", "RB", "WR", "TE", "OL", "F7", "DB", "KP")
      pos.av.data = vector("list", length = length(position.groups))
      names(pos.av.data) = position.groups
      stat.names = c("Index", "AV", "Adj.St", "Av.Szn")
      multi.index = NULL
      for(pos in position.groups){
        pos.av.data[[pos]] = vector("list", length = length(stat.names))
        names(pos.av.data[[pos]]) = stat.names
        if(pos == "F7"){
          pos.names = c("DL", "LB")
        } else if(pos == "KP"){
          pos.names = c("K", "P")
        } else {
          pos.names = pos
        }
        pfr.index = which(player.av.data$Pos %in% pos.names & 
                            player.av.data$Year >= min.yr)
        pos.av.data[[pos]]$Index = 
          data.frame("Player" = player.av.data[pfr.index, "Player"], 
                     "Year" = player.av.data[pfr.index, "Year"], 
                     stringsAsFactors = FALSE)
        pos.av.data[[pos]]$Index[, other.cols] = NA
        names(pos.av.data[[pos]]$Index)[other.cols] =
          paste("Yr", 0:prev.yrs, sep = ".")
        for(i in 1:nrow(pos.av.data[[pos]]$Index)){
          player = pos.av.data[[pos]]$Index[i, "Player"]
          year0 = pos.av.data[[pos]]$Index[i, "Year"]
          for(j in other.cols){
            year = year0 - j + 3
            index = which(player.av.data$Player == player & 
                            player.av.data$Year == year)
            if(length(index) == 1){
              pos.av.data[[pos]]$Index[i, j] = index
            } else if(length(index) > 1){
              multi.index = append(player, year)
            }
            
          }
        }
        for(stat in stat.names[2:length(stat.names)]){
          pos.av.data[[pos]][[stat]] = pos.av.data[[pos]]$Index
          for(i in 1:nrow(pos.av.data[[pos]]$Index)){
            for(j in other.cols){
              index = pos.av.data[[pos]]$Index[i, j]
              pos.av.data[[pos]][[stat]][i, j] = player.av.data[index, stat]
            }
          }
        }
        pos.av.data[[pos]]$Draft.Pick = 
          data.frame("Player" = pos.av.data[[pos]]$Index$Player, 
                     "Draft.Pick" = player.av.data[pos.av.data[[pos]]$Index$Yr.0, "Draft.Pick"])
        pos.av.data[[pos]]$Age = 
          data.frame("Player" = pos.av.data[[pos]]$Index$Player, 
                     "Age" = player.av.data[pos.av.data[[pos]]$Index$Yr.0, "Age"])
      }
      {
        for(i in 1:nrow(pos.av.data$KP$Index)){
          for(col in 3:ncol(pos.av.data$KP$Index)){
            pos.av.data$KP$Adj.St[i, col] = 
              round(pos.av.data$KP$Adj.St[i, col] * 6)
            pos.av.data$KP$Av.Szn[i, col] = 
              pos.av.data$KP$AV[i, col] / pos.av.data$KP$Adj.St[i, col] * 16
          }
        }
      } # fixing kicker/punter starts
      
    } # organizing the data
    {
      linear.proj.models = vector("list", length = length(position.groups))
      names(linear.proj.models) = position.groups
      rookie.av.models = linear.proj.models
      rookie.avg.pick = linear.proj.models
      rookie.avg.av = linear.proj.models
      models.with.draft = linear.proj.models
      prev.yrs.wgts = linear.proj.models
      all.avg.av = linear.proj.models
      # intercept.multiplier = linear.proj.models
      intercept.wgt = linear.proj.models
      for(pos in position.groups){
        qualified.data = pos.av.data[[pos]]
        for(i in 1:nrow(qualified.data$Index)){
          for(j in other.cols){
            is.qual = isTRUE(qualified.data$Adj.St[i, j] >= min.adj.start)
            # if(pos == "KP"){
            #   is.qual = isTRUE(qualified.data$Adj.St[i, j] * 6 >= min.adj.start)
            # }
            if(!is.qual){
              for(stat in stat.names){
                qualified.data[[stat]][i, j] = NA
              }
            }
          }
        }
        qual.index = which(!is.na(qualified.data$Index$Yr.0))
        for(stat in c(stat.names, "Draft.Pick")){
          qualified.data[[stat]] = qualified.data[[stat]][qual.index, ]
          rownames(qualified.data[[stat]]) = 1:nrow(qualified.data[[stat]])
        }
        qualified.data$Av.Szn$Draft.Pick = 
         qualified.data$Draft.Pick$Draft.Pick
        # View(qualified.data$Av.Szn)
        rookie.index = NULL
        for(i in 1:nrow(qualified.data$Av.Szn)){
          if(mean(is.na(qualified.data$Av.Szn
                        [i, paste("Yr", 1:prev.yrs, sep = ".")])) == 1){
            rookie.index = append(rookie.index, i)
          }
        }
        rookie.data = qualified.data$Av.Szn[rookie.index, c(
          "Player", "Year", "Yr.0", "Draft.Pick")]
        outliers = which(rookie.data$Yr.0 < 0)
        if(length(outliers) > 0){
          rookie.index = rookie.index[-c(outliers)]
          rookie.data = qualified.data$Av.Szn[rookie.index, c(
            "Player", "Year", "Yr.0", "Draft.Pick")]
        }
        rookie.volume = qualified.data$Adj.St[rookie.index, "Yr.0"]
        # plot(rookie.data$Draft.Pick, rookie.data$Yr.0)
        # cor(rookie.data$Draft.Pick, rookie.data$Yr.0)
        tryCatch({
          rookie.draft.model = nls(Yr.0 ~ (Draft.Pick + h) ^ a + b, 
                                   data = rookie.data, algorithm = 'port',
                                   weights = rookie.volume, 
                                   start = c(a = 0.5, b = 0, h = 1), 
                                   lower = c(a = -Inf, b = -Inf, h = 0), 
                                   trace = FALSE)
        }, error = function(e){
          rookie.draft.model <<- 
            lm(Yr.0 ~ Draft.Pick, data = rookie.data, weights = rookie.volume)
          if(coef(rookie.draft.model)[2] > 0){
            rookie.draft.model <<- 
              lm(Yr.0 ~ 1, data = rookie.data, weights = rookie.volume)
          }
        })
        rookie.av.models[[pos]] = rookie.draft.model
        rookie.avg.pick[[pos]] = mean(rookie.data$Draft.Pick)
        rookie.avg.av[[pos]] = weighted.mean(rookie.data$Yr.0, rookie.volume)
        # predict(rookie.draft.model, data.frame(Draft.Pick = 1))
        # predict(rookie.draft.model, data.frame(Draft.Pick = 275))
        # YAAAY it worked!!! #yeet
        # only for RB, OL, 
        
        qualified.data$Av.Szn$Draft.Val = 
          GetDraftValue(qualified.data$Av.Szn$Draft.Pick, pos)
        x.cols = paste("Yr", 1:4, sep = ".")
        linear.proj.models[[pos]] = 
          CreateProjectionModel(x.cols, "Yr.0", qualified.data$Av.Szn)
        x.cols = c("Draft.Val", x.cols)
        models.with.draft[[pos]] = 
          CreateProjectionModel(x.cols, "Yr.0", qualified.data$Av.Szn)
        hist.coef = coef(linear.proj.models[[pos]])[-c(1)]
        coef.ratio = 
          numeric(length = (length(hist.coef) * (length(hist.coef) - 1)) / 2)
        # i = 1, k = j
        # i = 2, k = j + 1
        # i = 3, k = j + 2
        for(i in 1:length(hist.coef)){
          for(j in 2:length(hist.coef)){
            if(j > i){
              coef.ratio[i + j - 2] = (hist.coef[i] / hist.coef[j]) ^ (1 / (j -i))
            }
          }
        }
        common.ratio = mean(coef.ratio)
        relative.wgts = common.ratio ^ ((length(hist.coef) - 1):0)
        prev.yrs.wgts[[pos]] = relative.wgts / sum(relative.wgts) * sum(hist.coef)
        all.avg.av[[pos]] = weighted.mean(pos.av.data[[pos]]$Av.Szn$Yr.0, 
                                          pos.av.data[[pos]]$Adj.St$Yr.0)
        intercept.wgt[[pos]] = 
          as.numeric(coef(linear.proj.models[[pos]])[1] / all.avg.av[[pos]])
      }
    } # researching the model
    {
      hist.av.proj = data.frame()
      hist.volume = data.frame()
      important.cols = c("Player", "Year", "Pos", "Yr.0", "Yr.1", "Yr.2", "Yr.3", 
                         "Yr.4", "Draft.Pick", "Age")
      for(pos in position.groups){
        av.data = pos.av.data[[pos]]$Av.Szn
        volume.data = pos.av.data[[pos]]$Adj.St
        av.data$Pos = pos
        av.data$Draft.Pick = pos.av.data[[pos]]$Draft.Pick$Draft.Pick
        av.data$Age = pos.av.data[[pos]]$Age$Age
        av.data = av.data[, important.cols]
        hist.av.proj = rbind(hist.av.proj, av.data)
        hist.volume = rbind(hist.volume, volume.data)
      }
      hist.av.proj$Proj = NA
      hist.cols = paste("Yr", 1:4, sep = ".")
      for(i in 1:nrow(hist.av.proj)){
        hist.av = as.numeric(hist.av.proj[i, hist.cols])
        hist.adj.starts = as.numeric(hist.volume[i, hist.cols])
        nfl.yrs = max(hist.av.proj[i, "Age"] - 22, sum(!is.na(hist.av)))
        hist.av.proj[i, "Proj"] = ProjectAV(
          hist.av.proj[i, "Pos"], hist.av, hist.adj.starts, 
          hist.av.proj[i, "Draft.Pick"], nfl.yrs, av.per.szn = TRUE)
      }
      hist.av.proj$Resid = hist.av.proj$Yr.0 - hist.av.proj$Proj
      for(pos in position.groups){
        index = which(hist.av.proj$Pos == pos)
        plot(hist.av.proj[index, "Proj"], hist.av.proj[index, "Yr.0"], 
             main = paste(pos, "AV proj"), xlab = "Projected", ylab = "Actual")
        abline(0, 1, col="red")
      }
    } # implementing the model
  } # projection research
  {
    {
      player.av.data$Start.Pct = player.av.data$GS / player.av.data$G
      min.games = 10
      prev.yrs = 4
      min.yr = min(seasons) + prev.yrs
      other.cols = 3:(prev.yrs + 3)
      pos.vol.data = vector("list", length = length(position.groups))
      names(pos.vol.data) = position.groups
      stat.names = c("Index", "G", "GS", "Start.Pct")
      for(pos in position.groups){
        pos.vol.data[[pos]] = vector("list", length = length(stat.names))
        names(pos.vol.data[[pos]]) = stat.names
        if(pos == "F7"){
          pos.names = c("DL", "LB")
        } else if(pos == "KP"){
          pos.names = c("K", "P")
        } else {
          pos.names = pos
        }
        pfr.index = which(player.av.data$Pos %in% pos.names & 
                            player.av.data$Year >= min.yr)
        pos.vol.data[[pos]]$Index = pos.av.data[[pos]]$Index
        pos.vol.data[[pos]]$Index[, other.cols] = NA
        names(pos.vol.data[[pos]]$Index)[other.cols] =
          paste("Yr", 0:prev.yrs, sep = ".")
        for(i in 1:nrow(pos.vol.data[[pos]]$Index)){
          player = pos.vol.data[[pos]]$Index[i, "Player"]
          year0 = pos.vol.data[[pos]]$Index[i, "Year"]
          for(j in other.cols){
            year = year0 - j + 3
            index = which(player.av.data$Player == player & 
                            player.av.data$Year == year)
            if(length(index) == 1){
              pos.vol.data[[pos]]$Index[i, j] = index
            } else if(length(index) > 1){
              multi.index = append(player, year)
            }
            
          }
        }
        for(stat in stat.names[2:length(stat.names)]){
          pos.vol.data[[pos]][[stat]] = pos.vol.data[[pos]]$Index
          for(i in 1:nrow(pos.vol.data[[pos]]$Index)){
            for(j in other.cols){
              index = pos.vol.data[[pos]]$Index[i, j]
              pos.vol.data[[pos]][[stat]][i, j] = player.av.data[index, stat]
            }
          }
        }
        pos.vol.data[[pos]]$Draft.Pick = pos.av.data[[pos]]$Draft.Pick
        pos.vol.data[[pos]]$Age = pos.av.data[[pos]]$Age
      }
    } # organizing the data
    {
      stpct.pos = head(position.groups, -1)
      start.pct.models = vector("list", length = length(stpct.pos))
      names(start.pct.models) = stpct.pos
      rookie.stpct.models = start.pct.models
      rookie.avg.stpct = start.pct.models
      stpct.models.draft = start.pct.models
      stpct.wgts = start.pct.models
      avg.stpct = start.pct.models
      stpct.int.wgt = start.pct.models
      vol.to.proj = c("Start.Pct")
      for(pos in stpct.pos){
        qualified.data = pos.vol.data[[pos]]
        for(i in 1:nrow(qualified.data$Index)){
          for(j in other.cols){
            is.qual = isTRUE(qualified.data$G[i, j] >= min.games)
            # if(pos == "KP"){
            #   is.qual = isTRUE(qualified.data$Adj.St[i, j] * 6 >= min.adj.start)
            # }
            if(!is.qual){
              for(stat in stat.names){
                qualified.data[[stat]][i, j] = NA
              }
            }
          }
        }
        qual.index = which(!is.na(qualified.data$Index$Yr.0))
        for(stat in c(stat.names, "Draft.Pick", "Age")){
          qualified.data[[stat]] = qualified.data[[stat]][qual.index, ]
          rownames(qualified.data[[stat]]) = 1:nrow(qualified.data[[stat]])
        }
        # rookie.data = vector("list", length = length(vol.to.proj))
        # names(rookie.data) = vol.to.proj
        qualified.data$Start.Pct$Draft.Pick = 
          qualified.data$Draft.Pick$Draft.Pick
        qualified.data$Start.Pct$Age = qualified.data$Age$Age
        rookie.index = NULL
        for(i in 1:nrow(qualified.data$Start.Pct)){
          if(mean(is.na(qualified.data$Start.Pct
                        [i, paste("Yr", 1:prev.yrs, sep = ".")])) == 1){
            rookie.index = append(rookie.index, i)
          }
        }
        rookie.data = qualified.data$Start.Pct[rookie.index, c(
          "Player", "Year", "Yr.0", "Draft.Pick")]
        rookie.volume = qualified.data$G[rookie.index, "Yr.0"]
        tryCatch({
          logit.formula = Yr.0 ~ 1 / (1 + exp(-(m * Draft.Pick + b)))
          # Yr.0 ~ (Draft.Pick + h) ^ a + b
          rookie.draft.model = nls(logit.formula, 
                                   data = rookie.data,
                                   weights = rookie.volume, 
                                   start = c(m = -0.01, b = 0.5), 
                                   trace = FALSE)
        }, error = function(e){
          rookie.draft.model <<- 
            lm(Yr.0 ~ Draft.Pick, data = rookie.data, weights = rookie.volume)
          if(coef(rookie.draft.model)[2] > 0){
            rookie.draft.model <<- 
              lm(Yr.0 ~ 1, data = rookie.data, weights = rookie.volume)
          }
        })
        rookie.stpct.models[[pos]] = rookie.draft.model
        rookie.avg.stpct[[pos]] = weighted.mean(rookie.data$Yr.0, rookie.volume)
        
        qualified.data$Start.Pct$Draft.Val = 
          DraftStPct(qualified.data$Start.Pct$Draft.Pick, pos)
        x.cols = paste("Yr", 1:4, sep = ".")
        start.pct.models[[pos]] = 
          CreateProjectionModel(x.cols, "Yr.0", qualified.data$Start.Pct)
        x.cols = c("Draft.Val", x.cols)
        stpct.models.draft[[pos]] = 
          CreateProjectionModel(x.cols, "Yr.0", qualified.data$Start.Pct)
        hist.coef = coef(start.pct.models[[pos]])[-c(1)]
        coef.ratio = 
          numeric(length = (length(hist.coef) * (length(hist.coef) - 1)) / 2)
        for(i in 1:length(hist.coef)){
          for(j in 2:length(hist.coef)){
            if(j > i){
              coef.ratio[i + j - 2] = (hist.coef[i] / hist.coef[j]) ^ (1 / (j -i))
            }
          }
        }
        common.ratio = mean(coef.ratio)
        relative.wgts = common.ratio ^ ((length(hist.coef) - 1):0)
        stpct.wgts[[pos]] = relative.wgts / sum(relative.wgts) * sum(hist.coef)
        avg.stpct[[pos]] = weighted.mean(pos.vol.data[[pos]]$Start.Pct$Yr.0, 
                                         pos.vol.data[[pos]]$G$Yr.0)
        stpct.int.wgt[[pos]] = 
          as.numeric(coef(start.pct.models[[pos]])[1]) / avg.stpct[[pos]]
      }
    } # predicting start pct
    {
      games.played.models = vector("list", length = length(position.groups))
      names(games.played.models) = position.groups
      rookie.games.models = games.played.models
      rookie.avg.games = games.played.models
      games.models.draft = games.played.models
      games.played.wgts = games.played.models
      avg.games.played = games.played.models
      games.int.wgt = games.played.models
      prev.yr.cols = paste("Yr", 1:prev.yrs, sep = ".")
      for(pos in position.groups){
        games.played.data = pos.vol.data[[pos]]$G
        games.played.data$Age = pos.vol.data[[pos]]$Age$Age
        for(i in 1:nrow(games.played.data)){
          min.nfl.yrs = as.numeric(
            quantile(c(0, games.played.data[i, "Age"] - 22, 
            max(0, which(!is.na(games.played.data[i, prev.yr.cols])))), 0.5))
          
          na.data = which(is.na(games.played.data[i, prev.yr.cols]))
          for(j in na.data){
            if(j <= min.nfl.yrs){
              games.played.data[i, j + 3] = 0
            }
          }
        }
        games.played.data$Draft.Pick = pos.vol.data[[pos]]$Draft.Pick$Draft.Pick
        rookie.index = NULL
        for(i in 1:nrow(games.played.data)){
          if(mean(is.na(games.played.data[i, prev.yr.cols])) == 1){
            rookie.index = append(rookie.index, i)
          }
        }
        rookie.data = games.played.data[rookie.index, c(
          "Player", "Year", "Yr.0", "Draft.Pick")]
        tryCatch({
          rookie.draft.model = nls(Yr.0 ~ 16 / (1 + exp(-(m * Draft.Pick + b))), 
                                   data = rookie.data, 
                                   start = c(m = -0.01, b = 0), trace = FALSE)
        }, error = function(e){
          rookie.draft.model <<- 
            lm(Yr.0 ~ Draft.Pick, data = rookie.data)
          if(coef(rookie.draft.model)[2] > 0){
            rookie.draft.model <<- 
              lm(Yr.0 ~ 1, data = rookie.data)
          }
        })
        rookie.games.models[[pos]] = rookie.draft.model
        rookie.avg.games[[pos]] = mean(rookie.data$Yr.0)
        
        games.played.data$Draft.Val = 
          DraftGames(games.played.data$Draft.Pick, pos)
        x.cols = paste("Yr", 1:4, sep = ".")
        games.played.models[[pos]] = 
          CreateProjectionModel(x.cols, "Yr.0", games.played.data)
        x.cols = c("Draft.Val", x.cols)
        games.models.draft[[pos]] = 
          CreateProjectionModel(x.cols, "Yr.0", games.played.data)
        hist.coef = coef(games.played.models[[pos]])[-c(1)]
        coef.ratio = 
          numeric(length = (length(hist.coef) * (length(hist.coef) - 1)) / 2)
        for(i in 1:length(hist.coef)){
          for(j in 2:length(hist.coef)){
            if(j > i){
              coef.ratio[i + j - 2] = (hist.coef[i] / hist.coef[j]) ^ (1 / (j -i))
            }
          }
        }
        common.ratio = mean(coef.ratio)
        relative.wgts = common.ratio ^ ((length(hist.coef) - 1):0)
        games.played.wgts[[pos]] = 
          relative.wgts / sum(relative.wgts) * sum(hist.coef)
        avg.games.played[[pos]] = mean(pos.vol.data[[pos]]$G$Yr.0)
        games.int.wgt[[pos]] = 
          as.numeric(coef(games.played.models[[pos]])[1]) / avg.games.played[[pos]]
      }
      
    } # predicting games played
    # players who (at least sometimes) start should play 16 games
    # players who never start (probably) won't play in all 16 games
    # above does not apply to QBs
  } # projecting playing time
  {
    prev.yrs = 4
    hist.av.proj = data.frame()
    min.yr = min(seasons) + prev.yrs
    av.proj.data = vector("list", length = length(position.groups))
    names(av.proj.data) = position.groups
    stat.names = c("Index", "AV", "Av.Szn", "G", "Start.Pct")
    other.cols = 3:(prev.yrs + 3)
    prev.yr.cols = paste("Yr", 1:prev.yrs, sep = ".")
    all.yr.cols = paste("Yr", 0:prev.yrs, sep = ".")
    for(pos in position.groups){
      {
        av.proj.data[[pos]] = vector('list', length = length(stat.names))
        names(av.proj.data[[pos]]) = stat.names
        if(pos == "F7"){
          pos.names = c("DL", "LB")
        } else if(pos == "KP"){
          pos.names = c("K", "P")
        } else {
          pos.names = pos
        }
        pfr.index = which(player.av.data$Pos %in% pos.names & 
                            player.av.data$Year >= min.yr)
        av.proj.data[[pos]]$Index = 
          data.frame("Player" = player.av.data[pfr.index, "Player"], 
                     "Year" = player.av.data[pfr.index, "Year"], 
                     stringsAsFactors = FALSE)
        av.proj.data[[pos]]$Index[, all.yr.cols] = NA
        for(i in 1:nrow(av.proj.data[[pos]]$Index)){
          player = pos.av.data[[pos]]$Index[i, "Player"]
          year0 = pos.av.data[[pos]]$Index[i, "Year"]
          for(j in other.cols){
            year = year0 - j + 3
            index = which(player.av.data$Player == player & 
                            player.av.data$Year == year)
            if(length(index) == 1){
              av.proj.data[[pos]]$Index[i, j] = index
            }
          }
          
        }
        for(stat in stat.names[2:length(stat.names)]){
          av.proj.data[[pos]][[stat]] = av.proj.data[[pos]]$Index
          for(i in 1:nrow(av.proj.data[[pos]]$Index)){
            for(j in other.cols){
              index = av.proj.data[[pos]]$Index[i, j]
              av.proj.data[[pos]][[stat]][i, j] = player.av.data[index, stat]
            }
          }
        }
        av.proj.data[[pos]]$Draft.Pick = 
          data.frame("Player" = av.proj.data[[pos]]$Index$Player, "Draft.Pick" = 
                       player.av.data[av.proj.data[[pos]]$Index$Yr.0, "Draft.Pick"])
        # drunk coding starts here
        av.proj.data[[pos]]$Age = 
          data.frame("Player" = av.proj.data[[pos]]$Index$Player, 
                     "Age" = player.av.data[av.proj.data[[pos]]$Index$Yr.0, "Age"])
        av.proj.data[[pos]]$Start.Pct$Proj.StPct = NA
        av.proj.data[[pos]]$G$Proj.Games = NA
        av.proj.data[[pos]]$Av.Szn$Proj.AvSzn = NA
        av.proj.data[[pos]]$AV$Proj.AV = NA
        for(i in 1:nrow(av.proj.data[[pos]]$Index)){
          hist.av = as.numeric(av.proj.data[[pos]]$Av.Szn[i, prev.yr.cols])
          hist.games = as.numeric(av.proj.data[[pos]]$G[i, prev.yr.cols])
          hist.stpct = as.numeric(av.proj.data[[pos]]$Start.Pct[i, prev.yr.cols])
          hist.adj.starts = (hist.games + 5 * hist.stpct * hist.games) / 6
          draft.pick = av.proj.data[[pos]]$Draft.Pick[i, "Draft.Pick"]
          nfl.yrs = max(av.proj.data[[pos]]$Age[i, "Age"] - 22, 
                        sum(!is.na(hist.av)))
          proj.av.szn = ProjectAV(pos, hist.av, hist.adj.starts, 
                                  draft.pick, nfl.yrs, av.per.szn = TRUE)
          proj.games = ProjectGames(pos, hist.games, draft.pick, nfl.yrs)
          proj.stpct = ProjectStPct(pos, hist.stpct, hist.games, draft.pick, 
                                    nfl.yrs, total.starts = FALSE)
          
          av.proj.data[[pos]]$Av.Szn[i, "Proj.AvSzn"] = proj.av.szn
          av.proj.data[[pos]]$G[i, "Proj.Games"] = proj.games
          av.proj.data[[pos]]$Start.Pct[i, "Proj.StPct"] = proj.stpct
          proj.av = (5 * proj.games * proj.stpct + proj.games) / 6 * 
            proj.av.szn / 16
          # if(pos == "KP"){
          #   proj.av = proj.av * 6
          # }
          av.proj.data[[pos]]$AV[i, "Proj.AV"] = proj.av
        }
        x = av.proj.data[[pos]]$AV # come up with a better variable name
        x$Draft.Pick = av.proj.data[[pos]]$Draft.Pick$Draft.Pick
        x$Age = av.proj.data[[pos]]$Age$Age
        x$Pos = pos
        x = x[, c("Player", "Year", "Pos", all.yr.cols, "Draft.Pick", "Age", 
                  "Proj.AV")]
        hist.av.proj = rbind(hist.av.proj, x)
        # error for KP
      } # organizing the data
    }
    View(hist.av.proj)
    pos = "RB"
    min.proj = 12.8
    max.proj = 13.2
    stat = "G"
    expected.average = (min.proj + max.proj) / 2
    df = av.proj.data[[pos]][[stat]]
    index = 
      which(df$Proj.Games >= min.proj & df$Proj.Games <= max.proj)
    proj.average = mean(df[index, "Proj.Games"])
    actual.average = mean(df[index, "Yr.0"])
  } # testing all 3 models together
} # individual
{
  {
    prev.yrs = 4
    hist.av.data = as.data.frame(
      read_excel("/Users/malexk999/Desktop/Miscellaneous/Football/NFL Ratings/AV data.xlsx"))
    hist.av.data[which(is.na(hist.av.data$Draft)), "Draft"] = ""
    hist.av.data[which(hist.av.data$Pos %in% c("DL", "LB")), "Pos"] = 'F7'
    hist.av.data[which(hist.av.data$Pos %in% c("K", "P")), "Pos"] = 'KP'
    {
      player.year = paste(hist.av.data$Player, hist.av.data$Year)
      duplicate.players = NULL
      for(i in 1:length(player.year)){
        if(length(which(player.year == player.year[i])) > 1){
          duplicate.players = append(duplicate.players, player.year[i])
        }
      }
      duplicate.players = unique(duplicate.players)
      bad.index = NULL
      for(i in 1:length(duplicate.players)){
        words = strsplit(duplicate.players[i], " ")[[1]]
        year = tail(words, n=1)
        player = paste(setdiff(words, year), collapse = " ")
        pfr.index = which(hist.av.data$Player == player & 
                            hist.av.data$Year == year)
        pfr.data = hist.av.data[pfr.index, ]
        age.draft.tm = paste(pfr.data$Age, pfr.data$Draft, pfr.data$Tm)
        each.adt = unique(age.draft.tm)
        num.players = length(each.adt)
        adt.index = match(age.draft.tm, each.adt)
        new.pfr.index = numeric(length = num.players)
        for(j in 1:num.players){
          this.pfr.index = pfr.index[which(adt.index == j)]
          if(length(this.pfr.index) > 1){
            pfr.team = pfr.data[match(j, adt.index), "Tm"]
            kalin.team = nfl.teams[match(pfr.team, pfr.tms)]
            if(is.na(kalin.team)){
              if(pfr.team %in% other.pfr.tms){
                kalin.team = other.kalin.tms[match(pfr.team, other.pfr.tms)]
              } 
            }
            if(!is.na(kalin.team)){
              roster.table = nfl.hist.rosters[[kalin.team]][[as.character(year)]]
              roster.index = match(player, roster.table$Player)
              if(is.na(roster.index)){
                player.position = ""
              } else {
                player.position = toupper(roster.table[roster.index, "Pos"])
              }
              
            } else {
              player.position = ""
            }
            if(player.position == "") {
              this.yr.pos = pfr.data$Pos
              all.index = which(hist.av.data$Player == player & 
                                  hist.av.data$Draft == pfr.data[1, "Draft"])
              all.pos = hist.av.data[all.index, "Pos"]
              pos.freq = numeric(length = length(this.yr.pos))
              for(k in 1:length(this.yr.pos)){
                pos.freq[k] = sum(all.pos == this.yr.pos[k])
              }
              player.position = this.yr.pos[match(max(pos.freq), pos.freq)]
              # other.pfr.tms = append(other.pfr.tms, pfr.team)
            } else {
              roster.table = nfl.hist.rosters[[kalin.team]][[as.character(year)]]
              roster.index = match(player, roster.table$Player)
              player.position = toupper(roster.table[roster.index, "Pos"])
            }
            true.position = GetPosition(player.position)
            if(!is.na(true.position)){
              player.position = true.position
            }
            real.pfr.index = this.pfr.index[which(
              hist.av.data[this.pfr.index, "Pos"] == player.position)]
            if(length(real.pfr.index) == 0){
              for(k in 1:length(grouped.pos)){
                if(player.position %in% position.classif[[grouped.pos[k]]]){
                  player.position = gp.pfr.names[k]
                }
              }
              real.pfr.index = this.pfr.index[which(
                hist.av.data[this.pfr.index, "Pos"] == player.position)]
            }
            new.pfr.index[j] = real.pfr.index
            fake.pfr.index = setdiff(this.pfr.index, real.pfr.index)
            if(length(fake.pfr.index) > 0){
              # hist.av.data = hist.av.data[-c(fake.pfr.index), ]
              bad.index = append(bad.index, fake.pfr.index)
            }
          } else {
            new.pfr.index[j] = this.pfr.index
          }
          
        }
        
        pfr.data = hist.av.data[new.pfr.index, ]
        # career.av = numeric(length = num.players)
        # for(j in 1:num.players){
        #   his.index = which(hist.av.data$Player == player & 
        #                       hist.av.data$Draft == pfr.data[j, "Draft"])
        #   career.av[j] = sum(hist.av.data[his.index, "AV"])
        # }
        # av.ranks = num.players - rank(career.av, ties.method = "last") + 1
        # last.char = substr(player, nchar(player), nchar(player))
        # if(last.char %in% as.character(0:9)){
        #   # player = gsub(paste(" ", last.char, sep = ""), "", player)
        #   suffix = suffix.letters[av.ranks]
        #   # suffix = gsub(" ", "", suffix)
        # } else {
        #   suffix = paste(" ", av.ranks, sep = "")
        #   suffix = gsub(" 1", "", suffix)
        # }
        # for(j in 1:num.players){
        #   his.index = new.pfr.index[j]
        #   hist.av.data[his.index, "Player"] = 
        #     paste(player, suffix[j], sep = "")
        # }
        
        
      }
      if(length(bad.index) > 0){
        hist.av.data = hist.av.data[-c(bad.index), ]
      }
    } # duplicates
    hist.av.data$Draft.Pick = NA
    for(i in 1:nrow(hist.av.data)){
      draft.info = strsplit(hist.av.data[i, "Draft"], "-")[[1]]
      if(length(draft.info) == 2){
        round = as.numeric(draft.info[[1]])
        draft.pick = as.numeric(draft.info[2])
        if(round > 1 & draft.pick < 32){ # supplemental draft
          draft.pick = 275
        }
      } else if(length(draft.info) == 0){
        draft.pick = 275
      } else {
        next()
      }
      hist.av.data[i, "Draft.Pick"] = min(draft.pick, 275)
    }
    hist.av.data$Birth.Yr = hist.av.data$Year - hist.av.data$Age
    all.hist.rosters = data.frame()
    for(year in 2017:2006){
      for(i in 1:nNflTeams){
        pfr.data = nfl.hist.rosters[[i]][[as.character(year)]]
        pfr.data[, c("Draft.Pick", "Pred.Games", "Pred.StPct", "Real.StPct", 
                     "Pred.Av.Rate", "Pos.Type")] = NA
        pfr.data[which(is.na(pfr.data$GS)), "GS"] = 0
        pfr.data$Real.StPct = pfr.data$GS / pfr.data$G
        pfr.data[which(pfr.data$Yrs == "Rook"), "Yrs"] = 0
        pfr.data$Yrs = as.numeric(pfr.data$Yrs)
        bad.index = NULL
        for(j in 1:nrow(pfr.data)){
          draft.info = pfr.data[j, "Drafted (tm/rnd/yr)"]
          if(draft.info == ""){
            draft.pick = 275
          } else {
            words = strsplit(draft.info, " / ")[[1]]
            draft.round = as.numeric(substr(words[2], 1, 1))
            draft.pick = words[3]
            draft.pick = as.numeric(substr(draft.pick, 1, nchar(draft.pick) - 7))
            if(draft.round > 1 & draft.pick < 32){ # supplemental draft
              draft.pick = 275
            }
          }
          pfr.data[j, "Draft.Pick"] = draft.pick
          {
            name = pfr.data[j, "Player"]
            birth.yr = year - pfr.data[j, "Age"]
            pfr.index = which(hist.av.data$Player == name & 
                                hist.av.data$Draft.Pick == draft.pick & 
                                hist.av.data$Birth.Yr == birth.yr)
            player.data = hist.av.data[pfr.index, ]
            player.seasons = player.data$Year
            if(!isTRUE(all.equal(year - 1:prev.yrs, player.seasons))){
              new.index = rep(NA, prev.yrs)
              for(k in 1:prev.yrs){
                new.index[k] = pfr.index[match(year - k, player.seasons)]
              }
              pfr.index = new.index
              player.data = hist.av.data[pfr.index, ]
            }
            hist.games = player.data$G
            hist.starts = player.data$GS
            hist.adj.starts = (hist.games + hist.starts * 5) / 6
            hist.stpct = hist.starts / hist.games
            hist.tot.av = player.data$AV
            hist.av.szn = hist.tot.av / hist.adj.starts * 16
            position.type = GetPosition(pfr.data[j, "Pos"])
            if(is.na(position.type)){
              av.index = which(hist.av.data$Player == name & 
                                 hist.av.data$Year == year & 
                                 hist.av.data$Draft.Pick == draft.pick & 
                                 hist.av.data$Age == pfr.data[j, "Age"])
              if(length(av.index) == 1){
                position.type = hist.av.data[av.index, "Pos"]
              } else {
                bad.index = append(bad.index, j)
                next()
              }
            }
            nfl.yrs = pfr.data[j, "Yrs"]
            proj.av.szn = ProjectAV(position.type, hist.av.szn, hist.adj.starts, 
                                    draft.pick, nfl.yrs, av.per.szn = TRUE)
            proj.games = ProjectGames(position.type, hist.games, draft.pick, nfl.yrs)
            proj.stpct = ProjectStPct(position.type, hist.stpct, hist.games, 
                                      draft.pick, nfl.yrs, total.starts = FALSE)
            pfr.data[j, "Pred.Av.Rate"] = proj.av.szn
            pfr.data[j, "Pred.Games"] = proj.games
            pfr.data[j, "Pred.StPct"] = proj.stpct
            pfr.data[j, "Pos.Type"] = position.type
          } # the magic
          # rookies projected too many games played & starts (especially OL)
          # exponentiate start % until it adds up to 1
          # also use AV as a part of start % (maybe like avrate^2)
        }
        if(length(bad.index) > 0){
          pfr.data = pfr.data[-c(bad.index), ]
        }
        pfr.data$Team = nfl.teams[i]
        pfr.data$Year = year
        pfr.data = pfr.data[, c("Player", "Team", "Year", "Pos", "G", "GS", "Yrs", 
                                "AV", "Draft.Pick", "Pred.Games", "Pred.StPct", 
                                "Real.StPct", "Pred.Av.Rate", "Pos.Type")]
        all.hist.rosters = rbind(all.hist.rosters, pfr.data)
      }
    }
    all.hist.rosters$Pred.AV1 = all.hist.rosters$Pred.Games * 
      (1 + 5 * all.hist.rosters$Pred.StPct) / 6 * 
      all.hist.rosters$Pred.Av.Rate / 16
    nonlin.stpct.models = vector("list", length = length(position.groups))
    names(nonlin.stpct.models) = position.groups
    start.values = nonlin.stpct.models
    {
      start.values$QB = c(a = 0.4, b = 1, c = 0.03, d = 1, e = 0.25)
      # start.values$QB = c(a = 67, b = 10^-4, c = -10, d = -0.035, e = -100)
      start.values$RB = c(a = 0.9, b = 1.4, c = 0.07, d = 0.7, e = 0.01)
      start.values$WR = c(a = 1, b = 1.35, c = -0.01, d = 5, e = -0)
      start.values$TE = c(a = 1, b = 1.2, c = 0.04, d = -1.5, e = 0)
      start.values$OL = c(a = 0.25, b = 1, c = 0.07, d = 1, e = 0.17)
      # start.values$OL = c(a = 0.6, b = 0.5, c = 120, d = 0.04, e = -120)
      start.values$F7 = c(a = 0.71, b = 1, c = 0.05, d = 1, e = -0)
      # start.values$F7 = c(a = -25, b = -10^-3*4, c = 35, d = 0.1, e = -20)
      start.values$DB = c(a = 0.57, b = 1, c = 0.09, d = 1, e = -0.1)
      # start.values$DB = c(a = 0.5, b = 1, c = 25, d = 0.13, e = -35)
    } # old start values
    start.values$QB = c(a = 1, b = 0.1, c = 0.4, d = -2.7)
    start.values$RB = c(a = 3.1, b = -0.04, c = 0.23, d = -2.1)
    start.values$WR = c(a = 1.1, b = 0.02, c = 0.6, d = -2.8)
    start.values$TE = c(a = 2.6, b = -0.16, c = 0.5, d = -1.6)
    start.values$OL = c(a = -3.8, b = -0.5, c = 1.4, d = 1.34)
    start.values$F7 = c(a = -2.4, b = -0.01, c = 1.4, d = -4)
    start.values$DB = c(a = 0.1, b = 0.2, c = 1, d = -4.5)
    model.formula = Real.StPct ~ 1 / (1 + exp(-(
      a * Pred.StPct + b * Pred.Av.Rate + c * Pred.AV1 + d)))
    for(pos in position.groups){
      if(pos != "KP"){
        pos.roster.data = 
          all.hist.rosters[which(all.hist.rosters$Pos.Type == pos), ]
        tryCatch({
          nonlin.stpct.models[[pos]] = 
            nls(model.formula, 
                data = pos.roster.data, algorithm = "port", 
                start = start.values[[pos]], 
                weights = pos.roster.data$G, trace = FALSE)
          nonlin.stpct.models
        }, error = function(e){
          nonlin.stpct.models[[pos]] <<-
            lm(Real.StPct ~ Pred.StPct + Pred.Av.Rate + Pred.AV1, 
               data = pos.roster.data, weights = pos.roster.data$G)
        })
        
      }
    } # getting the models
    # nonlinear worked initially for QB, RB, TE
    # got all to work!
    predict(nonlin.stpct.models$DB, data.frame(Pred.StPct = 0.6, 
                                               Pred.Av.Rate = 6.5))
    all.hist.rosters$Pred.StPct2 = NA
    for(i in 1:nrow(all.hist.rosters)){
      position = all.hist.rosters[i, "Pos.Type"]
      proj.stpct = all.hist.rosters[i, "Pred.StPct"]
      proj.av.szn = all.hist.rosters[i, "Pred.Av.Rate"]
      proj.games = all.hist.rosters[i, "Pred.Games"]
      all.hist.rosters[i, "Pred.StPct2"] = 
        PredictRealStPct(position, proj.stpct, proj.av.szn, proj.games)
    }
    # all.hist.rosters$Model.StPct = predict(stpct.model, data.frame(
    #   Pred.StPct = all.hist.rosters$Pred.StPct, 
    #   Pred.Av.Rate = all.hist.rosters$Pred.Av.Rate))
  } # researching start pct data
  
  {
    prev.yrs = 4
    hist.av.data = as.data.frame(
      read_excel("/Users/malexk999/Desktop/Miscellaneous/Football/NFL Ratings/AV data.xlsx"))
    hist.av.data = hist.av.data[which(hist.av.data$Year >= season - prev.yrs), ]
    hist.av.data[which(is.na(hist.av.data$Draft)), "Draft"] = ""
    hist.av.data[which(hist.av.data$Pos %in% c("DL", "LB")), "Pos"] = 'F7'
    hist.av.data[which(hist.av.data$Pos %in% c("K", "P")), "Pos"] = 'KP'
    {
      player.year = paste(hist.av.data$Player, hist.av.data$Year)
      duplicate.players = NULL
      for(i in 1:length(player.year)){
        if(length(which(player.year == player.year[i])) > 1){
          duplicate.players = append(duplicate.players, player.year[i])
        }
      }
      duplicate.players = unique(duplicate.players)
      bad.index = NULL
      for(i in 1:length(duplicate.players)){
        words = strsplit(duplicate.players[i], " ")[[1]]
        year = tail(words, n=1)
        player = paste(setdiff(words, year), collapse = " ")
        pfr.index = which(hist.av.data$Player == player & 
                            hist.av.data$Year == year)
        pfr.data = hist.av.data[pfr.index, ]
        age.draft.tm = paste(pfr.data$Age, pfr.data$Draft, pfr.data$Tm)
        each.adt = unique(age.draft.tm)
        num.players = length(each.adt)
        adt.index = match(age.draft.tm, each.adt)
        new.pfr.index = numeric(length = num.players)
        for(j in 1:num.players){
          this.pfr.index = pfr.index[which(adt.index == j)]
          if(length(this.pfr.index) > 1){
            pfr.team = pfr.data[match(j, adt.index), "Tm"]
            kalin.team = nfl.teams[match(pfr.team, pfr.tms)]
            if(is.na(kalin.team)){
              if(pfr.team %in% other.pfr.tms){
                kalin.team = other.kalin.tms[match(pfr.team, other.pfr.tms)]
              } 
            }
            if(!is.na(kalin.team)){
              roster.table = nfl.hist.rosters[[kalin.team]][[as.character(year)]]
              roster.index = match(player, roster.table$Player)
              if(is.na(roster.index)){
                player.position = ""
              } else {
                player.position = toupper(roster.table[roster.index, "Pos"])
              }
              
            } else {
              player.position = ""
            }
            if(player.position == "") {
              this.yr.pos = pfr.data$Pos
              all.index = which(hist.av.data$Player == player & 
                                  hist.av.data$Draft == pfr.data[1, "Draft"])
              all.pos = hist.av.data[all.index, "Pos"]
              pos.freq = numeric(length = length(this.yr.pos))
              for(k in 1:length(this.yr.pos)){
                pos.freq[k] = sum(all.pos == this.yr.pos[k])
              }
              player.position = this.yr.pos[match(max(pos.freq), pos.freq)]
              # other.pfr.tms = append(other.pfr.tms, pfr.team)
            } else {
              roster.table = nfl.hist.rosters[[kalin.team]][[as.character(year)]]
              roster.index = match(player, roster.table$Player)
              player.position = toupper(roster.table[roster.index, "Pos"])
            }
            true.position = GetPosition(player.position)
            if(!is.na(true.position)){
              player.position = true.position
            }
            real.pfr.index = this.pfr.index[which(
              hist.av.data[this.pfr.index, "Pos"] == player.position)]
            if(length(real.pfr.index) == 0){
              for(k in 1:length(grouped.pos)){
                if(player.position %in% position.classif[[grouped.pos[k]]]){
                  player.position = gp.pfr.names[k]
                }
              }
              real.pfr.index = this.pfr.index[which(
                hist.av.data[this.pfr.index, "Pos"] == player.position)]
            }
            new.pfr.index[j] = real.pfr.index
            fake.pfr.index = setdiff(this.pfr.index, real.pfr.index)
            if(length(fake.pfr.index) > 0){
              # hist.av.data = hist.av.data[-c(fake.pfr.index), ]
              bad.index = append(bad.index, fake.pfr.index)
            }
          } else {
            new.pfr.index[j] = this.pfr.index
          }
          
        }
        
        pfr.data = hist.av.data[new.pfr.index, ]
        # career.av = numeric(length = num.players)
        # for(j in 1:num.players){
        #   his.index = which(hist.av.data$Player == player & 
        #                       hist.av.data$Draft == pfr.data[j, "Draft"])
        #   career.av[j] = sum(hist.av.data[his.index, "AV"])
        # }
        # av.ranks = num.players - rank(career.av, ties.method = "last") + 1
        # last.char = substr(player, nchar(player), nchar(player))
        # if(last.char %in% as.character(0:9)){
        #   # player = gsub(paste(" ", last.char, sep = ""), "", player)
        #   suffix = suffix.letters[av.ranks]
        #   # suffix = gsub(" ", "", suffix)
        # } else {
        #   suffix = paste(" ", av.ranks, sep = "")
        #   suffix = gsub(" 1", "", suffix)
        # }
        # for(j in 1:num.players){
        #   his.index = new.pfr.index[j]
        #   hist.av.data[his.index, "Player"] = 
        #     paste(player, suffix[j], sep = "")
        # }
        
        
      }
      if(length(bad.index) > 0){
        hist.av.data = hist.av.data[-c(bad.index), ]
      }
    } # duplicates
    hist.av.data$Draft.Pick = NA
    for(i in 1:nrow(hist.av.data)){
      draft.info = strsplit(hist.av.data[i, "Draft"], "-")[[1]]
      if(length(draft.info) == 2){
        round = as.numeric(draft.info[[1]])
        draft.pick = as.numeric(draft.info[2])
        if(round > 1 & draft.pick < 32){ # supplemental draft
          draft.pick = 275
        }
      } else if(length(draft.info) == 0){
        draft.pick = 275
      } else {
        next()
      }
      hist.av.data[i, "Draft.Pick"] = min(draft.pick, 275)
    }
    hist.av.data$Birth.Yr = hist.av.data$Year - hist.av.data$Age
  } # raw data
  {
    total.games.played = (diff(range(hist.av.data$Year)) + 1) * 32 * 16
    starters.per.tm = c(QB = 1, RB = 1.5, WR = 2.1, TE = 1.4, OL = 5, 
                        F7 = 6.8, DB = 4.2, KP = 0)
    names(starters.per.tm) = position.groups
    min.starters = starters.per.tm
    max.starters = starters.per.tm
    avg.starters = starters.per.tm
    for(pos in position.groups){
      avg.starters[[pos]] = 
        round(sum(hist.av.data[which(hist.av.data$Pos == pos), "GS"]) / 
                total.games.played, 2)
    }
    max.starters = c(QB = 1, RB = 2, WR = 3, TE = 2, OL = 5, F7 = 7, DB = 5, KP = 0)
    min.starters = c(QB = 1, RB = 1, WR = 2, TE = 1, OL = 5, F7 = 6, DB = 4, KP = 0)
    starting.group.names = c("QB", "Flex", "OL", "Def", "KP")
    starting.groups = vector("list", length = length(starting.group.names))
    names(starting.groups) = starting.group.names
    starting.groups$QB = "QB"
    starting.groups$Flex = c("RB", "WR", "TE")
    starting.groups$OL = "OL"
    starting.groups$Def = c("F7", "DB")
    starting.groups$KP = "KP"
  } # starters at each position
  all.av.proj = data.frame()
  team.av.proj = data.frame("Team" = nfl.teams, "Tot.AV" = NA, "Off.AV" = NA, 
                            "Def.AV" = NA, "Roster.Size" = NA)
  for(pos in position.groups){
    team.av.proj[, paste(pos, "AV", sep = ".")] = NA
  }
  nfl.proj.rosters = vector("list", length = nNflTeams)
  names(nfl.proj.rosters) = nfl.teams
  starters.per.game = nfl.proj.rosters
  off.pos = c("QB", "RB", "WR", "TE", "OL")
  def.pos = c("F7", "DB")
  # i = 14
  start.error.max = 0.04
  for(i in 1:nNflTeams){
    pfr.url = paste("https://www.pro-football-reference.com/teams/", 
                    tolower(pfr.tms[i]), "/", season, "_roster.htm", sep = "")
    pfr.html = rawToChar(GET(pfr.url)$content)
    pfr.data = ReadHtmlTableKalin(pfr.html)
    pfr.data[, c("Draft.Pick", "Games", "Starts1", "Starts2","StPct1", "StPct2", 
                 "AV", "Av.Rate", "Adj.St1", "Adj.St2", "Pos.Type")] = NA
    pfr.data[which(pfr.data$Yrs == "Rook"), "Yrs"] = 0
    pfr.data$Yrs = as.numeric(pfr.data$Yrs)
    bad.index = NULL
    for(j in 1:nrow(pfr.data)){
      draft.info = pfr.data[j, "Drafted (tm/rnd/yr)"]
      if(draft.info == ""){
        draft.pick = 275
      } else {
        words = strsplit(draft.info, " / ")[[1]]
        draft.round = as.numeric(substr(words[2], 1, 1))
        draft.pick = words[3]
        draft.pick = as.numeric(substr(draft.pick, 1, nchar(draft.pick) - 7))
        if(draft.round > 1 & draft.pick < 32){ # supplemental draft
          draft.pick = 275
        }
      }
      pfr.data[j, "Draft.Pick"] = draft.pick
      {
        name = pfr.data[j, "Player"]
        birth.yr = season - pfr.data[j, "Age"]
        pfr.index = which(hist.av.data$Player == name & 
                            hist.av.data$Draft.Pick == draft.pick & 
                            hist.av.data$Birth.Yr == birth.yr)
        player.data = hist.av.data[pfr.index, ]
        player.seasons = player.data$Year
        if(!isTRUE(all.equal(season - 1:prev.yrs, player.seasons))){
          new.index = rep(NA, prev.yrs)
          for(k in 1:prev.yrs){
            new.index[k] = pfr.index[match(season - k, player.seasons)]
          }
          pfr.index = new.index
          player.data = hist.av.data[pfr.index, ]
        }
        hist.games = player.data$G
        hist.starts = player.data$GS
        hist.adj.starts = (hist.games + hist.starts * 5) / 6
        pfr.pos = pfr.data[j, "Pos"]
        position.type = GetPosition(pfr.pos)
        if(is.na(position.type)){
          position.type = player.data[1, "Pos"]
        }
        if(is.na(position.type)){
          bad.index = append(bad.index, j)
          next()
        }
        if(position.type == "KP"){
          hist.adj.starts = hist.adj.starts * 6
        }
        hist.stpct = hist.starts / hist.games
        hist.tot.av = player.data$AV
        hist.av.szn = hist.tot.av / hist.adj.starts * 16
        nfl.yrs = pfr.data[j, "Yrs"]
        proj.av.szn = ProjectAV(position.type, hist.av.szn, hist.adj.starts, 
                                draft.pick, nfl.yrs, av.per.szn = TRUE)
        proj.games = ProjectGames(position.type, hist.games, draft.pick, nfl.yrs)
        proj.stpct = ProjectStPct(position.type, hist.stpct, hist.games, 
                                  draft.pick, nfl.yrs, total.starts = FALSE)
        real.proj.stpct = PredictRealStPct(position.type, proj.stpct, 
                                           proj.av.szn, proj.games)
        pfr.data[j, "Av.Rate"] = proj.av.szn
        pfr.data[j, "Games"] = proj.games
        pfr.data[j, "Starts1"] = proj.games * real.proj.stpct
        pfr.data[j, "StPct1"] = real.proj.stpct
        pfr.data[j, "Pos.Type"] = position.type
        pfr.data[j, "Adj.St1"] = 
          (proj.games + 5 * (proj.games * real.proj.stpct)) / 6
      } # the magic
      # rookies projected too many games played & starts (especially OL)
      # exponentiate start % until it adds up to 1
      # also use AV as a part of start % (maybe like avrate^2)
    } # projections for each player
    if(length(bad.index) > 0){
      pfr.data = pfr.data[-c(bad.index), ]
    }
    starters.per.game[[i]] = vector("numeric", length = length(position.groups))
    names(starters.per.game[[i]]) = position.groups
    for(pos.group in head(starting.group.names, -1)){
      total.starts = 0
      for(pos in starting.groups[[pos.group]]){
        total.starts = total.starts + starters.per.tm[[pos]]
      }
      total.starts = total.starts * 16
      index = which(pfr.data$Pos.Type %in% starting.groups[[pos.group]])
      # current.tot.starts = sum(pfr.data[index, "Starts1"])
      current.stpct = pfr.data[index, "StPct1"]
      proj.games = pfr.data[index, "Games"]
      tot.games = sum(proj.games)
      target.avg.stpct = total.starts / tot.games
      curr.proj.starts = sum(proj.games * current.stpct)
      games.wgt.exp = 1
      new.stpct = current.stpct
      while(abs(1 - curr.proj.starts / total.starts) > start.error.max){
        # games.wgt.exp = games.wgt.exp * (curr.proj.starts / total.starts)
        current.logit.vals = logit(current.stpct)
        avg.logit.val = weighted.mean(current.logit.vals, 
                                      proj.games ^ games.wgt.exp)
        # avg.logit.val = logit(sum(current.stpct * proj.games) / tot.games)
        logit.adj = logit(target.avg.stpct) - avg.logit.val
        logit.adj = 1 - curr.proj.starts / total.starts
        # if(abs(logit.adj) < 0.1){
          # logit.adj = logit.adj / abs(logit.adj) * abs(0)
          # next()
        # }
        new.logit.vals = current.logit.vals + logit.adj
        new.stpct = inv.logit(new.logit.vals)
        # pfr.data[index, "StPct2"] = pfr.data[index, "StPct1"] * 
        #   total.starts / current.tot.starts
        curr.proj.starts = sum(proj.games * new.stpct)
        current.stpct = new.stpct
      }
      
      pfr.data[index, "StPct2"] = new.stpct
      pfr.data[index, "Starts2"] =
        proj.games * pfr.data[index, "StPct2"]
      for(pos in starting.groups[[pos.group]]){
        pos.index = which(pfr.data$Pos.Type == pos)
        new.tot.starts = sum(pfr.data[pos.index, "Starts2"])
        starters.per.game[[i]][[pos]] = new.tot.starts / 16
      }
    }
    for(pos in c("K", "P")){
      index = which(pfr.data$Pos == pos)
      current.tot.starts = sum(pfr.data[index, "Games"])
      pfr.data[index, "Games"] = pfr.data[index, "Games"] * 16 / 
        current.tot.starts
      pfr.data[index, "StPct2"] = 0
      pfr.data[index, "Starts2"] = 0
    }
    pfr.data$Adj.St2 = (pfr.data$Games + 5 * pfr.data$Starts2) / 6
    kp.index = which(pfr.data$Pos.Type == "KP")
    pfr.data[kp.index, "Adj.St2"] = pfr.data[kp.index, "Games"]
    pfr.data$Proj.AV = pfr.data$Av.Rate * pfr.data$Adj.St2 / 16
    team.av = round(sum(pfr.data$Proj.AV), 2)
    # print(paste(nfl.teams[i], team.av, "AV"))
    team.av.proj[i, "Tot.AV"] = team.av
    off.av = sum(pfr.data[which(pfr.data$Pos.Type %in% off.pos), "Proj.AV"])
    def.av = sum(pfr.data[which(pfr.data$Pos.Type %in% def.pos), "Proj.AV"])
    off.av = off.av + sum(pfr.data[which(pfr.data$Pos == "K"), "Proj.AV"])
    def.av = def.av + sum(pfr.data[which(pfr.data$Pos == "P"), "Proj.AV"])
    for(pos in position.groups){
      index = which(pfr.data$Pos.Type == pos)
      start.adj = sum(pfr.data[index, "Starts2"]) / 
        sum(pfr.data[index, "Adj.St2"])
      if(pos == "KP"){
        start.adj = 1
      }
      team.av.proj[i, paste(pos, "AV", sep = ".")] = 
        sum(pfr.data[index, "Proj.AV"])  * start.adj
        
    }
    pfr.data = pfr.data[, c("Player", "Pos", "Yrs", "Draft.Pick", "Games", 
                            "Starts2", "StPct2", "Av.Rate", "Adj.St2", 
                            "Pos.Type", "Proj.AV")]
    pfr.data = arrange(pfr.data, desc(pfr.data$Proj.AV))
    nfl.proj.rosters[[i]] = pfr.data
    team.av.proj[i, "Roster.Size"] = nrow(pfr.data)
    team.av.proj[i, "Off.AV"] = off.av
    team.av.proj[i, "Def.AV"] = def.av
    pfr.data$Team = nfl.teams[i]
    pfr.data = pfr.data[, c("Player", "Team", "Pos", "Yrs", "Draft.Pick", 
                            "Games", "Starts2", "StPct2", "Av.Rate", 
                            "Adj.St2", "Pos.Type", "Proj.AV")]
    all.av.proj = rbind(all.av.proj, pfr.data)
  }
  View(team.av.proj)
  {
    team.elo.proj = team.av.proj[, 1:4]
    nfl.avg.ppg = 22
    avg.off.av = mean(team.av.proj$Off.AV)
    avg.def.av = mean(team.av.proj$Def.AV)
    {
      # for offense its just 100m
      # AV = 100 * (1 + 2M - M^2)/(2m)
      # m = PPG / NFL.PPG
      # av = 100 * (1/(2m) + 1 - m/2)
      # av/100 = 1/(2m) + 1 - m/2
      # av/100 - 1 = 1/(2m) - m/2
      # 1 - av/100 = m/2 - 1/(2m)
      # 1 - av/100 = 1/(2m)(m^2 - 1)
      
      # m = ppg / nfl.avg
      # m = -av/100 + sqrt((av/100) ^ 2 - 2(av/100) + 2) + 1
      # symbolabs to the rescue
      GetPtsAllowed = function(def.av, scale100 = TRUE){
        if(scale100){
          def.av = def.av / 100
        }
        return(-def.av + sqrt((def.av)^2 - 2*(def.av) + 2) + 1)
      }
    } # algebra
    team.elo.proj$PF = team.elo.proj$Off.AV / avg.off.av * nfl.avg.ppg
    team.elo.proj$PA = GetPtsAllowed(team.elo.proj$Def.AV / avg.off.av, 
                                     scale100 = FALSE) * nfl.avg.ppg
    nfl.pyth.exp = 2.37
    team.elo.proj$Pyth = team.elo.proj$PF ^ nfl.pyth.exp / 
      (team.elo.proj$PF ^ nfl.pyth.exp + team.elo.proj$PA ^ nfl.pyth.exp)
    team.elo.proj$ELO = 500 + eloDiff(team.elo.proj$Pyth)
    min.sd = sd(nfl.pre$Rating) * 1.1
    team.elo.proj$ELO = round((team.elo.proj$ELO - 500) / 
                                sd(team.elo.proj$ELO) * min.sd + 500, 2)
  } # team pts scored and allowed per game
} # team projections
# next up: project playing time (start %, games played)

# starts not meaningful for punters/kickers, use games played instead

# https://www.pro-football-reference.com/play-index/psl_finder.cgi?
# request=1 &match=single &year_min=2017 &year_max=2017 &season_start=1 
# &season_end=-1 &pos%5B%5D=ilb &draft_year_min=1936 &draft_year_max=2018 
# &draft_slot_min=1 &draft_slot_max=500 &draft_pick_in_round=pick_overall 
# &conference=any &draft_pos%5B%5D=qb &draft_pos%5B%5D=rb &draft_pos%5B%5D=wr 
# &draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c5val=1.0&order_by=av

"https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=2017&year_max=2017&season_start=1&season_end=-1&pos%5B%5D=ol&draft_year_min=1936&draft_year_max=2018&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c1stat=g&c1comp=gt&c1val=2&c5val=1.0&order_by=av"

# players are sometimes listed under 2 positions (nate solder LT/te)
# inspect team rosters to determine primary position