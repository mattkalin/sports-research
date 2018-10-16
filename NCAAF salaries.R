{
NCAAF_coaches_salaries <- read_excel(
  "~/Desktop/Summer 2017/NCAAF coaches salaries.xlsx", sheet = "Salary", 
  col_types = c("numeric", "text", "text", "text", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric"))
NCAAF_Sagarin <- read_excel("~/Desktop/Summer 2017/NCAAF coaches salaries.xlsx",
                            sheet = "Sagarin")
NCAAF_coaches_salaries$All.coaches=NCAAF_coaches_salaries$`TOTAL PAY`+
  NCAAF_coaches_salaries$`ASST PAY TOTAL`
} # data import
{
x=which(is.na(NCAAF_coaches_salaries$`ASST PAY TOTAL`)==FALSE)
x.schools=NCAAF_coaches_salaries[x, "SCHOOL"]
x.conf=NCAAF_coaches_salaries[x, "CONF"]
x.all.coaches=NCAAF_coaches_salaries$All.coaches
x.all.coaches=x.all.coaches[x]
x.head.coach=NCAAF_coaches_salaries$`TOTAL PAY`
x.head.coach=x.head.coach[x]
NCAAF_data=data.frame("Team"=x.schools, "CONF"=x.conf, "HC Salary"=
     x.head.coach,"Coach Payroll"=x.all.coaches, "Pay Rk"=NA, "Record"=NA, 
    "Wins Rating"=NA, "Rating Rk"=NA)
NCAAF_data[44, "Coach.Payroll"]=NCAAF_data[44, "Coach.Payroll"]+1
NCAAF_data$Pay.Rk=length(x)-rank(x.all.coaches, ties.method="last")+1
} # new dataframe
{
NCAAF_Sagarin$sched.elo=-400*log10(1/pnorm(NCAAF_Sagarin$SCHEDL, 55, 16)-1)
ncaaf.win.pct=NCAAF_Sagarin$W/(NCAAF_Sagarin$W+NCAAF_Sagarin$L)
NCAAF_Sagarin$wl.elo=-400*log10(1/ncaaf.win.pct-1)
NCAAF_Sagarin$Adj.win.elo=NCAAF_Sagarin$sched.elo+NCAAF_Sagarin$wl.elo+500
sag.predictor=NA
for(i in 1:length(x)){
  team=as.character(NCAAF_data[i, "SCHOOL"])
  sagarin.index=which(NCAAF_Sagarin$Team==team)
  NCAAF_data[i, "Wins.Rating"]=round(
    NCAAF_Sagarin[sagarin.index, "Adj.win.elo"], 1)
  wins=NCAAF_Sagarin[sagarin.index, "W"]
  losses=NCAAF_Sagarin[sagarin.index, "L"]
  wl.record=paste(wins, losses, sep = "-")
  NCAAF_data[i, "Record"]=wl.record
  sag.predictor[i]=as.numeric(NCAAF_Sagarin[sagarin.index, "PREDICTOR"])
}
NCAAF_data$Rating.Rk=length(x)-rank(NCAAF_data$Wins.Rating)+1
} # power ratings
{
plot((NCAAF_data$Coach.Payroll)/1000000, (NCAAF_data$Wins.Rating), xlab = 
       "Head and Assistant Coach Payroll ($Millions)", ylab = 
       "Schedule-Adjusted Win-Loss Record ELO Rating", 
     main = "Team Wins Strength vs. Coach Payroll")
# power.win.pct=1/(10^((500-NCAAF_data$Wins.Rating)/400)+1)
# plot(log(NCAAF_data$Coach.Payroll), power.win.pct)

# points(NCAAF_data[67, "Coach.Payroll"], NCAAF_data[67, "Wins.Rating"], lwd=2)
# points(NCAAF_data[2, "Coach.Payroll"], NCAAF_data[2, "Wins.Rating"], lwd=2)
# points(NCAAF_data[11, "Coach.Payroll"], NCAAF_data[11, "Wins.Rating"], lwd=2)
# points(NCAAF_data[1, "Coach.Payroll"], NCAAF_data[1, "Wins.Rating"], lwd=2)
# points(NCAAF_data[3, "Coach.Payroll"], NCAAF_data[3, "Wins.Rating"], lwd=2)

text(NCAAF_data[67, "Coach.Payroll"]/1000000, NCAAF_data[67, "Wins.Rating"], 
     labels = "Western Michigan", pos = 1, cex=0.8)
text(NCAAF_data[2, "Coach.Payroll"]/1000000, NCAAF_data[2, "Wins.Rating"], 
     labels = "Alabama", pos = 1, cex=0.8)
text(NCAAF_data[11, "Coach.Payroll"]/1000000, NCAAF_data[11, "Wins.Rating"], 
     labels = "Clemson", pos = 1, cex=0.8)
text(NCAAF_data[1, "Coach.Payroll"]/1000000, NCAAF_data[1, "Wins.Rating"], 
     labels = "Michigan", pos = 1, cex=0.8)
text(NCAAF_data[3, "Coach.Payroll"]/1000000, NCAAF_data[3, "Wins.Rating"], 
     labels = "Ohio State", pos = 1, cex=0.8)
text(NCAAF_data[56, "Coach.Payroll"]/1000000, NCAAF_data[56, "Wins.Rating"], 
     labels = "Fresno State", pos = 1, cex=0.8)

sorted.payroll=sort.default(NCAAF_data$Coach.Payroll)
y=NA
for(i in 1:length(NCAAF_data$SCHOOL)){
  finder.payroll=sorted.payroll[i]
  og.index=which(NCAAF_data$Coach.Payroll==finder.payroll)
  y[i]=og.index
}
sorted.ratings=as.numeric(NCAAF_data[y, "Wins.Rating"])

sorted.payroll.mils=sorted.payroll/1000000
ncaaf.payroll.model=lm(sorted.ratings~(sorted.payroll.mils))
payroll.preds=predict(ncaaf.payroll.model, newdata=list(
  x=seq(from=0,to=max(NCAAF_data$Coach.Payroll),length.out = 1000)))
lines((sorted.payroll/1000000), payroll.preds, col="red")

} # graph
{
confs=c("SEC", "Big Ten", "Big 12", "Pac-12", "ACC", "AAC","Ind.","Mt. West",
        "C-USA", "Sun Belt", "MAC")
NCAAF_Conferences=data.frame("Conference"=confs, "Teams"=NA, 
    "Avg Payroll"=NA, "Payroll Rk"=NA, "Avg Rating"=NA, "Rating Rk"=NA)

for(i in 1:length(confs)){
  conf=confs[i]
  index=which(NCAAF_data$CONF==conf)
  NCAAF_Conferences[i, "Teams"]=length(index)
  NCAAF_Conferences[i, "Avg.Payroll"]=round(
    mean(NCAAF_data[index, "Coach.Payroll"]))
  NCAAF_Conferences[i, "Avg.Rating"]=round(
    mean(NCAAF_data[index, "Wins.Rating"]))
}
NCAAF_Conferences$Payroll.Rk=12-rank(NCAAF_Conferences$Avg.Payroll)
NCAAF_Conferences$Rating.Rk=12-rank(NCAAF_Conferences$Avg.Rating)
NCAAF_data$Power.Rating=NULL
NCAAF_Conferences=arrange(
  NCAAF_Conferences,desc(NCAAF_Conferences$Avg.Payroll))
} # conferences