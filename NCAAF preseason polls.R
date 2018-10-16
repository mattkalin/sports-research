{
AP_Preseason_polls <- 
  read_excel("~/Desktop/Summer 2017/Preseason polls.xlsx", 
                                 sheet = "AP clean", na = "NA")
Coaches_Preseason_polls <- 
  read_excel("~/Desktop/Summer 2017/Preseason polls.xlsx", 
             sheet = "Coach clean", na = "NA")
Sagarin_Data <- 
  read_excel("~/Desktop/Summer 2017/Preseason polls.xlsx", 
             sheet = "Sagarin clean", na = "NA")
Recruiting_Data <- read_excel("~/Desktop/Summer 2017/Preseason polls.xlsx", 
                             sheet = "Rivals raw")

Poll.data=data.frame("Year"=AP_Preseason_polls$Year, 
                     "Team"=AP_Preseason_polls$Name, 
                     "AP"=AP_Preseason_polls$PTS, "Coaches"=NA, 
                     "Ttl Rank"=NA, "Avg Pts"=NA)
for(i in 1:nrow(AP_Preseason_polls)){
  name=as.character(AP_Preseason_polls[i, "Name"])
  year=as.numeric(AP_Preseason_polls[i, "Year"])
  coach.index=which(Coaches_Preseason_polls$Name==name & 
                      Coaches_Preseason_polls$Year==year)
  if(length(coach.index)==1){
  coach.pts=Coaches_Preseason_polls[coach.index, "PTS"]
  Poll.data[i, "Coaches"]=coach.pts
  }
} # fills in coaches' poll pts
Poll.data=Poll.data[which(is.na(Poll.data$Coaches)==FALSE),]
} # data import
{
polls_voters=data.frame("Year"=2002:2016, "AP"=NA, "Coaches"=NA)
for(i in 2002:2016){
  index=i-2001
  year=i
  this.year=which(AP_Preseason_polls$Year==year)
  polls_voters[index, "AP"]=sum(AP_Preseason_polls[this.year, "FPV"])
  polls_voters[index, "Coaches"]=sum(Coaches_Preseason_polls[this.year, "FPV"])
  {
    ap.first.place=which(AP_Preseason_polls$RK==1)
    ap_first_place=AP_Preseason_polls[ap.first.place,]
    coach.first.place=which(Coaches_Preseason_polls$RK==1)
    coach_first_place=Coaches_Preseason_polls[coach.first.place,]
  } # first place teams
}
  Poll.data$AP.Avg=NA
  Poll.data$Coach.Avg=NA
  {
# AP_Preseason_polls$Avg.Pts=NA
# Coaches_Preseason_polls$Avg.Pts=NA
# for(i in 1:nrow(AP_Preseason_polls)){
#   year=as.integer(AP_Preseason_polls[i, "Year"])
#   year.index=year-2001
#   ap.voters=polls_voters[year.index, "AP"]
#   coach.voters=polls_voters[year.index, "Coaches"]
#   AP_Preseason_polls[i, "Avg.Pts"]=AP_Preseason_polls[i, "PTS"]/ap.voters
#   Coaches_Preseason_polls[i, "Avg.Pts"]=Coaches_Preseason_polls[i, "PTS"]/
#     coach.voters
# }
} # unused
  for(i in 1:nrow(Poll.data)){
    year=as.integer(Poll.data[i, "Year"])
    year.index=year-2001
    ap.voters=polls_voters[year.index, "AP"]
    coach.voters=polls_voters[year.index, "Coaches"]
    total.voters=ap.voters+coach.voters
    total.pts=Poll.data[i, "AP"]+Poll.data[i, "Coaches"]
    Poll.data[i, "Avg.Pts"]=total.pts/total.voters
    Poll.data[i, "AP.Avg"]=Poll.data[i, "AP"]/ap.voters
    Poll.data[i, "Coach.Avg"]=Poll.data[i, "Coaches"]/coach.voters
  }
  
  for(i in 1:nrow(Poll.data)){
    team=as.character(Poll.data[i, "Team"])
    year=as.integer(Poll.data[i, "Year"])
    this.year=Poll.data[which(Poll.data$Year==year),]
    this.year.index=match(team, this.year$Team)
    ranking=nrow(this.year)+1-1*rank(this.year$Avg.Pts)[this.year.index]
    Poll.data[i, "Ttl.Rank"]=ranking
  }

} # average points 
{
Sagarin_Data[which(Sagarin_Data$Team=="Southern California"),"Team"]="USC"
Sagarin_Data[which(Sagarin_Data$Team=="Southern Cal"),"Team"]="USC"
Sagarin_Data[which(Sagarin_Data$Team=="Mississippi"),"Team"]="Ole Miss"
Sagarin_Data[which(Sagarin_Data$Team=="Miami-Florida"),"Team"]="Miami"
Sagarin_Data[which(Sagarin_Data$Team=="Hawaii"),"Team"]="Hawai'i"
Sagarin_Data[which(Sagarin_Data$Team=="No. Carolina St."),"Team"]="NC State"
Sagarin_Data$Team=sub("St.", "State", Sagarin_Data$Team, fixed = TRUE)
Recruiting_Data[which(Recruiting_Data$School=="Mississippi"),
                "School"]="Ole Miss"
Recruiting_Data[which(Recruiting_Data$School=="Miami (FL)"),
                "School"]="Miami"
Recruiting_Data[which(Recruiting_Data$School=="Brigham Young"),
                "School"]="BYU"
Recruiting_Data[which(Recruiting_Data$School=="Hawaii"),
                "School"]="Hawai'i"
Recruiting_Data[which(Recruiting_Data$School=="North Carolina State"),
                "School"]="NC State"
# AP_Preseason_polls$Sagarin=NA
# Coaches_Preseason_polls$Sagarin=NA
# AP_Preseason_polls$Sag.1=NA
# Coaches_Preseason_polls$Sag.1=NA
# AP_Preseason_polls$Rec.0=NA
# Coaches_Preseason_polls$Rec.0=NA
Poll.data$Sagarin=NA
Poll.data$Sag.1=NA
Poll.data$Recrt=NA
Poll.data$Rec.1=NA
# spaces.at.end=AP_Preseason_polls[which(substr(AP_Preseason_polls$Name,
#   nchar(AP_Preseason_polls$Name), nchar(AP_Preseason_polls$Name))==" "), "Name"]
} # sagarin and recruiting team name fix
{
for(i in 1:nrow(Poll.data)){
  name=as.character(Poll.data[i, "Team"])
  year=as.numeric(Poll.data[i, "Year"])
  sag.index=which(Sagarin_Data$Team==name & Sagarin_Data$Year==year)
  if(length(sag.index)==1){
  sag.rating=Sagarin_Data[sag.index, "Predictor"]
  Poll.data[i, "Sagarin"]=sag.rating
  }
  sag.index.1=which(Sagarin_Data$Team==name & Sagarin_Data$Year==year-1)
  if(length(sag.index.1)==1){
    last.year.sag=Sagarin_Data[sag.index.1, "Predictor"]
    Poll.data[i, "Sag.1"]=last.year.sag
  }
  rec.index=which(Recruiting_Data$School==name & Recruiting_Data$Year==year)
  if(length(rec.index)==1){
    rec.rating=Recruiting_Data[rec.index, "Points"]
    Poll.data[i, "Recrt"]=rec.rating
  }
  rec1.index=which(Recruiting_Data$School==name & 
                   Recruiting_Data$Year==year-1)
  if(length(rec1.index)==1){
    rec1.rating=Recruiting_Data[rec1.index, "Points"]
    Poll.data[i, "Rec.1"]=rec1.rating
  }
}
  # AP_Preseason_polls=AP_Preseason_polls[-c(369),] 
  # Coaches_Preseason_polls=Coaches_Preseason_polls[-c(371),]
  # Poll.data=Poll.data[-c(356),] # marshall outlier (recruiting)
# x=AP_Preseason_polls[which(is.na(AP_Preseason_polls$Sagarin)),]
  # x=Poll.data[which(is.na(Poll.data$Rec.1)),]
  # Poll.data=Poll.data[which(is.na(Poll.data$Rec.1)==FALSE),]
  Poll.data$Sag.1=NULL
  Poll.data$Recrt=NULL
  Poll.data$Rec.1=NULL
  Poll.data$AP.Avg=NULL
  Poll.data$Coach.Avg=NULL
  
  rownames(Poll.data)=1:nrow(Poll.data)
  
} # fill in sagarin ratings and recruiting points
{
  {
    # ap.avg.pts=ap.avg.pts[which(AP_Preseason_polls$Year!=2016)]
    # coach.pts=coach.pts[which(AP_Preseason_polls$Year!=2016)]
    # sag.rating=sag.rating[which(AP_Preseason_polls$Year!=2016)]
    # sag.last=sag.last[which(AP_Preseason_polls$Year!=2016)]
    polls.training=Poll.data[which(Poll.data$Year!=2016),]
    polls.model=lm(Sagarin~Avg.Pts, data = polls.training)
    # ap.avg.pts=AP_Preseason_polls$Avg.Pts
    # coach.pts=Coaches_Preseason_polls$Avg.Pts
    # sag.rating=AP_Preseason_polls$Sagarin
    # sag.last=AP_Preseason_polls$Sag.1
    # preds.16=predict(polls.model, newdata = data.frame(both.avg.pts=(
      # ap.avg.pts[1:25]+coach.pts[1:25])/2, sag.last=sag.last[1:25]))
    preds.16=predict(polls.model, data.frame(
      Avg.Pts=Poll.data[1:25, "Avg.Pts"]))
    polls.test=Poll.data[1:25,]
    # polls.test$Avg.Pts=(ap.avg.pts[1:25]+coach.pts[1:25])/2
    # polls.test$PTS=NULL
    # polls.test$FPV=NULL
    polls.test$Model=preds.16
    polls.test$Resid=polls.test$Sagarin-preds.16
  } # testing for 2016
# ap.avg.pts=AP_Preseason_polls$Avg.Pts
# coach.pts=Coaches_Preseason_polls$Avg.Pts
# sag.rating=Poll.data$Sagarin
# sag.last=AP_Preseason_polls$Sag.1
# both.avg.pts=(ap.avg.pts+coach.pts)/2
# ap.model=lm(sag.rating~ap.avg.pts)
# coaches.model=lm(sag.rating~coach.pts)
# both.model=lm(sag.rating~ap.avg.pts+coach.pts)
# avg.model=lm(sag.rating~both.avg.pts)

polls.model=lm(Sagarin~Avg.Pts, data = Poll.data) # best
# polls.model=lm(Sagarin~AP.Avg+Coach.Avg, data = Poll.data)

# recruiting and last year's rating don't make much of a difference
} # models 
Poll.data$Model=fitted(polls.model)
Poll.data$Resid=resid(polls.model)




