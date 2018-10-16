cfp.urls = c("http://www.espn.com/college-football/rankings/_/poll/21/week/15/year/2017/seasontype/2", 
             "http://www.espn.com/college-football/rankings/_/poll/21/week/15/year/2016/seasontype/2", 
             "http://www.espn.com/college-football/rankings/_/poll/21/week/15/year/2015/seasontype/2", 
             "http://www.espn.com/college-football/rankings/_/poll/21/week/16/year/2014/seasontype/2")
years = 2017:2014
ESPN_Abbreviations$Full.Name = paste(ESPN_Abbreviations$ESPN.Name, 
                                     ESPN_Abbreviations$ESPN.Abr, sep = "")
{
  conf.winners = vector("list", length = 10)
  names(conf.winners) = fbs.confs[-c(8)]
  conf.winners$ACC = c(clemson, clemson, clemson, florida.st)
  conf.winners$`Big 12` = c(oklahoma, oklahoma, oklahoma, NA) # 2014 baylor, tcu
  conf.winners$`Big Ten` = c(ohio.st, penn.st, michigan.st, ohio.st)
  conf.winners$SEC = c(georgia, alabama, alabama, alabama)
  conf.winners$`Pac-12` = c(usc, washington, stanford, oregon)
  conf.winners$American = c(ucf, temple, houston, memphis)
  conf.winners$`Conference USA` = c(florida.atlantic, western.kentucky, 
                                    western.kentucky, marshall)
  conf.winners$MAC = c(toledo, western.michigan, bowling.green, northern.illinois)
  conf.winners$`Mountain West` = c(boise.st, san.diego.st, san.diego.st, boise.st)
  conf.winners$`Sun Belt` = c(NA, NA, arkansas.st, georgia.southern)
  for(i in 1:10){
    names(conf.winners[[i]]) = years
  }
  power5.confs = fbs.confs[1:5]
} # conf winners
# wl record, conf winner, rating, pwr 5 conf

cfp.data = data.frame()
for(i in 1:length(cfp.urls)){
  cfp.html = rawToChar(GET(cfp.urls[i])$content)
  cfp.table = readHTMLTable(cfp.html)[[1]]
  cfp.table = ConvertFactorData(cfp.table)
  index = match(cfp.table$Team, ESPN_Abbreviations$Full.Name)
  cfp.table$Team = ESPN_Abbreviations[index, "Team"]
  cfp.table$Win = NA
  cfp.table$Loss = NA
  cfp.table$Power5 = NA
  cfp.table$Win.Conf = NA
  for(j in 1:nrow(cfp.table)){
    words = strsplit(cfp.table[j, "Rec"], "-")[[1]]
    cfp.table[j, "Win"] = as.numeric(words[1])
    cfp.table[j, "Loss"] = as.numeric(words[2])
    team = cfp.table[j, "Team"]
    index = match(team, ncaaf.teams)
    conference = NCAAF_Teams_and_Confs[index, "Conference"]
    cfp.table[j, "Power5"] = as.numeric(conference %in% power5.confs || 
                                          team == "Notre Dame")
    if(conference %in% names(conf.winners)){
      cfp.table[j, "Win.Conf"] = 
        as.numeric(isTRUE(as.logical(conf.winners[[conference]][i] == team)))
    } else {
      cfp.table[j, "Win.Conf"] = 0
    }
    
  }
  col.name = paste("Kal", years[i], sep = ".")
  index = match(cfp.table$Team, ratings.since.2000$Team)
  cfp.table$Rating = ratings.since.2000[index, col.name] + 1000
  cfp.table$Made.CFP = as.numeric(cfp.table$RK <= 4)
  cfp.table$Season = years[i]
  cfp.table = cfp.table[, c("Team", "Season", "Win", "Loss", "Power5", "Win.Conf", 
                            "Rating", "Made.CFP")]
  
  # rownames(cfp.table) = 1:25 + 25 * (i - 1)
  cfp.data = rbind(cfp.data, cfp.table)
}
cfp.data[which(cfp.data$Team %in% c(baylor, tcu) & cfp.data$Season == 2014), 
         "Win.Conf"] = 0.5
{
  # cfp.model = glm(Made.CFP ~ Win + Loss + Power5 + Win.Conf + Rating, 
  #                 family = binomial(link = 'logit'), data = cfp.data)
  cfp.model = glm(Made.CFP ~ Loss + Win.Conf + Rating, 
                  family = binomial(link = 'logit'), data = cfp.data)
  summary(cfp.model)
  cfp.data$Model = fitted(cfp.model)
} # regression (yeah fuck this)
# -20 (logit pts) per loss
# not power 5 worth 2 losses
# conf winner worth 0.5 losses
# 250 elo pts worth 1 loss (more in simulations)
CalcCfpPts = function(losses, conf.winner, elo.rating, power5 = TRUE, 
                      simulation = TRUE){
  loss.coef = 20
  if(simulation){
    elo.rating = elo.rating * 2/3
  }
  cfp.pts = -losses + 0.5 * conf.winner + elo.rating / 250 + 2 * power5
  return(cfp.pts * loss.coef)
}
cfp.data$CFP.Pts = CalcCfpPts(cfp.data$Loss, cfp.data$Win.Conf, cfp.data$Rating, 
                              cfp.data$Power5, simulation = FALSE)





