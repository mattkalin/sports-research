### Getting the Data
nfl.boxscores <- read.csv("~/Desktop/Moneyball/nfl.boxscores.post.2005.csv", 
                          stringsAsFactors=FALSE)
#View(nfl.boxscores)

### Seasons Vectors

seasons <- 2005:2015
n.seasons <- length(seasons)

### Avg. Yards per Rush and Catch for each Year


avg.yprush <- rep(NA, times = n.seasons)
avg.ypcatch <- rep(NA, times = n.seasons)

avg.tdprush <- rep(NA, times = n.seasons)
avg.tdpcatch <- rep(NA, times = n.seasons)

for(i in 1:n.seasons){
  year <- seasons[i]
  year.index <- which(nfl.boxscores[,"Year"] == year)
  temp <- nfl.boxscores[year.index,]
  rush.att <- sum(temp[,"RushAtt"])
  rush.yds <- sum(temp[,"RushYds"])
  avg.yprush[i] <- rush.yds / rush.att
  receptions <- sum(temp[,"Receptions"])
  rec.yards <- sum(temp[,"RecYds"])
  avg.ypcatch[i] <- rec.yards / receptions
  rush.td <- sum(temp[,"RushTD"]) / rush.att
  rec.td <- sum(temp[,"RecTD"]) / receptions
  avg.tdprush[i] <- rush.td
  avg.tdpcatch[i] <- rec.td
}

avg.yprush  
avg.ypcatch

avg.tdprush
avg.tdpcatch


### Avg. Yards per Rush/Reception/Pass per Year for 2010 
##(Working around “NA” in 2010)

n <- 6
index.2010 <- which(nfl.boxscores[,"Year"] == 2010)
vector <- nfl.boxscores[index.2010,]
vector <- vector[-c(575),]
rush.yds <- sum(vector[,"RushYds"])
rush.att <- sum(vector[,"RushAtt"])
this.is.it <- vector[,"Receptions"]
receptions <- sum(this.is.it)
rec.yards <- sum(vector[,"RecYds"])

rush.only <- rush.yds / rush.att
avg.yprush[n] <- rush.only
avg.yprush

catch.only <- rec.yards / receptions
avg.ypcatch[n] <- catch.only
avg.ypcatch

rush.td <- sum(vector[,"RushTD"]) / rush.att
rec.td <- sum(vector[,"RecTD"]) / receptions
avg.tdprush[n] <- rush.td
avg.tdpcatch[n] <- rec.td

avg.tdprush
avg.tdpcatch


### Forming the Yards-Equality Ratio

ratio.receive <- avg.ypcatch / avg.yprush
ratio.receive

#ratio.pass <- avg.yppass / avg.yprush
#ratio.pass

### Forming the TD-Equality Ratio

ratio.td <- avg.tdpcatch / avg.tdprush
ratio.td

### Add a column for touches

nfl.boxscores[,"Touches"] <- nfl.boxscores[,"Receptions"]
+ nfl.boxscores[,"RushAtt"] + nfl.boxscores[,"PassAtt"]
#View(nfl.boxscores)

### Remove non-RB,WR,TEs

index <- which(nfl.boxscores[,"FantPos"] == "RB" | 
                 nfl.boxscores[,"FantPos"] == "WR" | 
                 nfl.boxscores[,"FantPos"] == "TE")

reduced.nfl <- nfl.boxscores[index, ]

### Seasons Vectors (Again)

seasons <- 2005:2015
n.seasons <- length(seasons)

### Add a column for ADJUSTED touches
nflrows <- nrow(reduced.nfl)
reduced.nfl[,"AdjTouches"] <- rep(NA , times = nflrows)

for(i in 1:n.seasons){
  year <- seasons[i]
  index.year <- which(reduced.nfl[, "Year"] == year) 
  temp <- reduced.nfl[index.year, ]
  temp[,"AdjTouches"] <- ratio.receive[i]*temp[,"Receptions"] + temp[,"RushAtt"]
  adjtouches <- temp[,"AdjTouches"]
  reduced.nfl[index.year,"AdjTouches"] <- adjtouches
  temp[,"AdjTD"] <- ratio.td[i]*temp[,"RushTD"] + temp[,"RecTD"]
  adjtds <- temp[,"AdjTD"]
  reduced.nfl[index.year,"AdjTD"] <- adjtds
}
#View(reduced.nfl)
### Calculate Efficiency Rating

## formula with adjusted td
reduced.nfl[,"FER"] <- ((reduced.nfl[,"RecYds"]) + 
                          (reduced.nfl[,"RushYds"]) +
                          60 * (reduced.nfl[,"AdjTD"])) /
                          (reduced.nfl[,"AdjTouches"])


#View(reduced.nfl)


### Set parameters
limit <- 150
final.index <- which(reduced.nfl[,"AdjTouches"] >= limit)
final.nfl <- reduced.nfl[final.index, ]

## Max and Min (Overall)

final.nfl[which.max(final.nfl[,"FER"]),]
final.nfl[which.min(final.nfl[,"FER"]),]

## Separate into Positions (Runner and Passer)
# RB
rb.index <- which(final.nfl[,"FantPos"] == "RB")
final.rb <- final.nfl[rb.index, ]
# Receivers
catch.index <- which(final.nfl[,"FantPos"] == "WR" | 
                       final.nfl[,"FantPos"] == "TE" )
final.catch <- final.nfl[catch.index, ]
# WR
wr.index <- which(final.nfl[,"FantPos"] == "WR")
final.wr <- final.nfl[wr.index, ]
# TE
te.index <- which(final.nfl[,"FantPos"] == "TE")
final.te <- final.nfl[te.index, ]

###### Standardization

final.nfl[,"zFER"] <- rep(NA, times = nrow(final.nfl))

for(i in 1:n.seasons){
  year <- seasons[i]
  z.index <- which(final.nfl[,"Year"] == year)
  z.temp <- final.nfl[z.index,]
  z.mu <- mean(z.temp[,"FER"])
  z.sd <- sd(z.temp[,"FER"])
  z.fer <- (z.temp[,"FER"] - z.mu) / z.sd
  final.nfl[z.index,"zFER"] <- z.fer
}

## Max and Min (Overall zFER)

final.nfl[which.max(final.nfl[,"zFER"]),]
final.nfl[which.min(final.nfl[,"zFER"]),]

## Separate into Positions (Runner and Passer)
# RB
rb.index <- which(final.nfl[,"FantPos"] == "RB")
final.rb <- final.nfl[rb.index, ]
# Receivers
catch.index <- which(final.nfl[,"FantPos"] == "WR" | 
                       final.nfl[,"FantPos"] == "TE" )
final.catch <- final.nfl[catch.index, ]
# WR
wr.index <- which(final.nfl[,"FantPos"] == "WR")
final.wr <- final.nfl[wr.index, ]
# TE
te.index <- which(final.nfl[,"FantPos"] == "TE")
final.te <- final.nfl[te.index, ]

#View(final.rb)
#View(final.catch)
#View(final.wr)
#View(final.te)

# histograms and density lines
hist(final.nfl[,"zFER"], main = "Standardized FER: RB, WR, TE", 
     xlab="zFER", col="lightgreen", ylim=c(0, .6), probability = TRUE)
lines(density(final.nfl[,"zFER"]), col="red", lwd=2)
hist(final.rb[,"zFER"], main = "Standardized FER: RB", 
     xlab="zFER", col="lightgreen", ylim=c(0, .6), probability = TRUE)
lines(density(final.rb[,"zFER"]), col="red", lwd=2)
hist(final.wr[,"zFER"], main = "Standardized FER: WR", 
     xlab="zFER", col="lightgreen", ylim=c(0, .6), probability = TRUE)
lines(density(final.wr[,"zFER"]), col="red", lwd=2)
hist(final.te[,"zFER"], main = "Standardized FER: TE", 
     xlab="zFER", col="lightgreen", ylim=c(0, .6), probability = TRUE)
lines(density(final.te[,"zFER"]), col="red", lwd=2)
hist(final.catch[,"zFER"], main = "Standardized FER: WR and TE", 
     xlab="zFER", col="lightgreen", ylim=c(0, .6), probability = TRUE)
lines(density(final.catch[,"zFER"]), col="red", lwd=2)


## Max and Min (Separate)
# RB
final.rb[which.max(final.rb[,"zFER"]),]
final.rb[which.min(final.rb[,"zFER"]),]
# Receivers
final.catch[which.max(final.catch[,"zFER"]),]
final.catch[which.min(final.catch[,"zFER"]),]
# WR
final.wr[which.max(final.wr[,"zFER"]),]
final.wr[which.min(final.wr[,"zFER"]),]
# TE
final.te[which.max(final.te[,"zFER"]),]
final.te[which.min(final.te[,"zFER"]),]

## Scatterplot
plot(jitter(final.nfl[,"Year"]), final.nfl[,"zFER"], 
     xlab = "Season", ylab = "zFER", 
     main = "zTRB of All Players from 2005-2006 to 2015-2016, 
     min 150 Adjusted Touches", 
     col = "blue", cex = 0.6)

## Best Players each Season in zFER
seasons <- 2005:2015
n.seasons <- length(seasons)

max.players <- data.frame(Year = rep(NA, times = 11), 
               Name = rep(NA, times = 11), FER = rep(NA, times = 11), 
               zFER = rep(NA, times = 11))

for(i in 1:n.seasons){
  year <- seasons[i]
  year.index <- which(final.nfl[,"Year"] == year)
  temp <- final.nfl[year.index,]
  max.data <- temp[which.max(temp[,"zFER"]),]
  new.max <- max.data[c(1,2,23,24)]
  max.players[i,] <- new.max
}

#View(max.players)

## Look at Specific Players
player <- "LeSean McCoy"
player.index <- which(final.nfl[,"Player"] == player)
player.temp <- final.nfl[player.index, ]
#View(player.temp)

plot(player.temp[,"Year"], player.temp[,"zFER"], 
     xlab = "Season", ylab = "zFER", 
     main = "zTRB of Specific Player, min 150 Adjusted Touches", 
     col = "red" , cex = 0.8, pch = 16)     


# Empirical rule
mean(abs(final.nfl[,"zFER"])<=1)
mean(abs(final.nfl[,"zFER"])<=2)
mean(abs(final.nfl[,"zFER"])<=3)
mean(abs(final.rb[,"zFER"])<=1)
mean(abs(final.rb[,"zFER"])<=2)
mean(abs(final.rb[,"zFER"])<=3)
mean(abs(final.wr[,"zFER"])<=1)
mean(abs(final.wr[,"zFER"])<=2)
mean(abs(final.wr[,"zFER"])<=3)
mean(abs(final.te[,"zFER"])<=1)
mean(abs(final.te[,"zFER"])<=2)
mean(abs(final.te[,"zFER"])<=3)



### Eliminate duplicates
number <- which(final.nfl[,"Player"] == "Adrian Peterson" &
                  final.nfl[,"Tm"] == "CHI")
final.nfl[number,"Player"] <- "Adrian Peterson 2"

number <- which(final.nfl[,"Player"] == "Steve Smith" &
                  final.nfl[,"Tm"] == "NYG")
final.nfl[number,"Player"] <- "Steve Smith 2"

number <- which(final.nfl[,"Player"] == "Mike Williams" &
                  final.nfl[,"Tm"] == "SEA")
final.nfl[number,"Player"] <- "Mike Williams 2"


## Multi-Year Data

players <- final.nfl[,"Player"]
unik.players <- unique(players)
n.unik <- length(unik.players)
multi.year <- data.frame(Player = unik.players)
multi.year[,"2005"] <- rep(NA, times = n.unik)
multi.year[,"2006"] <- rep(NA, times = n.unik)
multi.year[,"2007"] <- rep(NA, times = n.unik)
multi.year[,"2008"] <- rep(NA, times = n.unik)
multi.year[,"2009"] <- rep(NA, times = n.unik)
multi.year[,"2010"] <- rep(NA, times = n.unik)
multi.year[,"2011"] <- rep(NA, times = n.unik)
multi.year[,"2012"] <- rep(NA, times = n.unik)
multi.year[,"2013"] <- rep(NA, times = n.unik)
multi.year[,"2014"] <- rep(NA, times = n.unik)
multi.year[,"2015"] <- rep(NA, times = n.unik)
#View(multi.year)

seasons <- 2005:2015
n.seasons <- length(seasons)



for(i in 1:n.seasons) {
  year <- seasons[i]
  year.index <- which(final.nfl[,"Year"] == year)
  temp <- final.nfl[year.index, ]
  temp.index <- which(temp[,"AdjTouches"] >= limit)
  super.temp <- temp[temp.index, ]
  unik.x <- unique(super.temp)
  unik.y <- unik.x[,"Player"]
  n.unik.y <- length(unik.y)
  for(j in 1:n.unik.y){
    player <- unik.y[j]
    player.index <- which(super.temp[,"Player"] == player)
    multi.index <- which(multi.year[,"Player"] == player)
    more.temp <- super.temp[player.index, ]
    ultra.temp <- more.temp[c(2,23)]
    if(ultra.temp[,"Player"] == player){
      multi.year[multi.index, i + 1] <- ultra.temp[,"FER"]
    }
  }
}


# previous years
# this year, 1 year ago, 2 years ago, etc.
prev.years = data.frame(Year=final.nfl[,"Year"])
prev.years[,"Player"]=final.nfl[,"Player"]
prev.years[,"This yr"]=final.nfl[,"FER"]
one.yr.ago=rep(NA, times=length(prev.years[,"Player"])) #all works thru here
for (i in 2:n.seasons) {
  year=seasons[n.seasons-i+2]
  year.index=which(final.nfl[,"Year"] == year-1)
  temp <- final.nfl[year.index, ] #returns all players from prev season
  temp.index <- which(temp[,"AdjTouches"] >= limit) #returns 1:length of temp
  super.temp=temp[temp.index,] #maybe it's working??
  for (j in 1:length(prev.years[,"Player"])) {
    player=temp
    player.index <- which(super.temp[,"Player"] == player[2])
    multi.index <- which(prev.years[,"Player"] == player[2])
    more.temp <- temp[player.index, ]
    ultra.temp <- more.temp[c(2,23)]
    one.yr.ago[j]=ultra.temp[j, "FER"]
  }
}
prev.years[,"1 yr ago"]=one.yr.ago
#View(prev.years)

improvement=data.frame(Player = multi.year[,"Player"])
improvement[,"2015"]=multi.year[,"2015"]-multi.year[,"2014"]
improvement[,"2014"]=multi.year[,"2014"]-multi.year[,"2013"]
improvement[,"2013"]=multi.year[,"2013"]-multi.year[,"2012"]
improvement[,"2012"]=multi.year[,"2012"]-multi.year[,"2011"]
improvement[,"2011"]=multi.year[,"2011"]-multi.year[,"2010"]
improvement[,"2010"]=multi.year[,"2010"]-multi.year[,"2009"]
improvement[,"2009"]=multi.year[,"2009"]-multi.year[,"2008"]
improvement[,"2008"]=multi.year[,"2008"]-multi.year[,"2007"]
improvement[,"2007"]=multi.year[,"2007"]-multi.year[,"2006"]
improvement[,"2006"]=multi.year[,"2006"]-multi.year[,"2005"]
#View(improvement)

######### 2014 Analysis (FER)

prediction.year <- 2014
predictor.index <- which(multi.year[,"2014"] != 0 & (multi.year[,"2013"] != 0 |
                                                     multi.year[,"2012"] != 0))
predictor.temp <- multi.year[predictor.index, ]
#View(predictor.temp)

players <- predictor.temp[,"Player"]
n.predict <- length(players)
predictor.temp[,"Pre-2014 Mean"] <- rep(NA, times = n.predict)
prob <- c(0.01, 0.03, 0.06, 0.08, 0.11, 0.13, 0.16, 0.19, 0.23)

for(i in 1:n.predict){
  player <- players[i]
  player.index <- which(predictor.temp[,"Player"] == player)
  vector <- predictor.temp[player.index,2:10]
  real.vector <- as.numeric(as.vector(vector[1,]))
  predict.mu <- weighted.mean(real.vector, prob, na.rm = TRUE)
  ### EDIT WEIGHTS!!!
  predictor.temp[player.index,"Pre-2014 Mean"] <- predict.mu
}
#View(predictor.temp)
plot(predictor.temp[,"Pre-2014 Mean"], predictor.temp[,"2014"], 
     xlab = "FER Average Before 2014", ylab = "2014 FER", col = "purple")
cor(predictor.temp[,"Pre-2014 Mean"], predictor.temp[,"2014"])

fit.pre_15 <- lm(predictor.temp[,"2014"] ~ predictor.temp[,"Pre-2014 Mean"])
a.fit <- fit.pre_15$coefficients[1]
b.fit <- fit.pre_15$coefficients[2]
abline(a = a.fit, b = b.fit, col = "red")
a.fit
b.fit


# predicting 2015
players <- predictor.temp[,"Player"]
n.predict <- length(players)
predictor.temp[,"Pre-2015 Mean"] <- rep(NA, times = n.predict)
prob <- c(0.01, 0.02, 0.03, 0.05, 0.07, 0.10, 0.13, 0.16, 0.19, 0.24)

for(i in 1:n.predict){
  player <- players[i]
  player.index <- which(predictor.temp[,"Player"] == player)
  vector <- predictor.temp[player.index,2:11]
  real.vector <- as.numeric(as.vector(vector[1,]))
  predict.mu <- weighted.mean(real.vector, prob, na.rm = TRUE)
  ### EDIT WEIGHTS!!!
  predictor.temp[player.index,"Pre-2015 Mean"] <- predict.mu
}

#forecasting 2015
predictor.temp[,"2015 forecast"]=predictor.temp[,"Pre-2015 Mean"]*b.fit+a.fit
plot(predictor.temp[,"2015 forecast"], predictor.temp[,"2015"], 
     xlab = "2015 forecast", ylab = "2015 FER", col = "purple")
cor(predictor.temp[,"2015 forecast"], predictor.temp[,"2015"])
predictor.temp[,"2015 residual"]=predictor.temp[,"2015"]-
  predictor.temp[,"2015 forecast"]
sd(predictor.temp[,"2015 residual"], na.rm = TRUE)


#cleaning the projected data
n.players=length(espy.projections[,"PLAYER..TEAM.POS"])
proj.players = sub('\\s*,.*','', espy.projections[,"PLAYER..TEAM.POS"])
espn.proj.reduced = data.frame("Player"=proj.players)
toString(espn.proj.reduced[,"Player"])
espn.proj.reduced[,"Player"]=sapply(espn.proj.reduced[,"Player"], as.character)
for (i in 1:n.players) { # this should get rid of injury *s
  short.name=toString(espn.proj.reduced[i,"Player"])
  short.name.length=nchar(short.name)
  if(substr(short.name, short.name.length, short.name.length)=="*"){
    espn.proj.reduced[i,"Player"]=substr(short.name, 1, short.name.length-1)
  }
}
proj.positions = rep(NA, times=n.players)
name.length = nchar(espy.projections[1,"PLAYER..TEAM.POS"])
for (i in 1:n.players) {
  long.name=espy.projections[i,"PLAYER..TEAM.POS"]
  name.length = nchar(long.name)
  if(substr(long.name, name.length-7, name.length-5)=="ing"){
    proj.positions[i]=substr(long.name, name.length-14, name.length-13)
  } else if(substr(long.name, name.length-7, name.length-5)=="ent"){
    proj.positions[i]=substr(long.name, name.length-12, name.length-11)
  } else if (substr(long.name, name.length-1, name.length)=="eo"){
    proj.positions[i]=substr(long.name, name.length-15, name.length-14)
  } else {
    proj.positions[i]=substr(long.name, name.length-1, name.length)
  }
  if(proj.positions[i]== " P" | proj.positions[i]== " Q" | 
     proj.positions[i]== " D" | proj.positions[i]== " O"){
    if(substr(long.name, name.length-7, name.length-5)=="ing"){
      proj.positions[i]=substr(long.name, name.length-17, name.length-16)
    } else if(substr(long.name, name.length-7, name.length-5)=="ent"){
      proj.positions[i]=substr(long.name, name.length-15, name.length-14)
    } else if (substr(long.name, name.length-1, name.length)=="eo"){
      proj.positions[i]=substr(long.name, name.length-18, name.length-17)
    } else {
      proj.positions[i]=substr(long.name, name.length-4, name.length-3)
    }
  }
}
espn.proj.reduced[,"Pos"]=proj.positions
espn.proj.reduced[,"RushAtt"]=espy.projections[,"RUSH"]
espn.proj.reduced[,"Receptions"]=espy.projections[,"REC"]
espn.proj.reduced[,"AdjTouches"]=espn.proj.reduced[,"RushAtt"]+
  11/4*espn.proj.reduced[,"Receptions"]


############
## Multi-Year Data (FER)
############
players <- final.nfl[,"Player"]
unik.players <- unique(players)
n.unik <- length(unik.players)
multi.year <- data.frame(Player = unik.players)
multi.year[,"FantPos"] <- rep(NA, times = n.unik)
multi.year[,"2005"] <- rep(NA, times = n.unik)
multi.year[,"2006"] <- rep(NA, times = n.unik)
multi.year[,"2007"] <- rep(NA, times = n.unik)
multi.year[,"2008"] <- rep(NA, times = n.unik)
multi.year[,"2009"] <- rep(NA, times = n.unik)
multi.year[,"2010"] <- rep(NA, times = n.unik)
multi.year[,"2011"] <- rep(NA, times = n.unik)
multi.year[,"2012"] <- rep(NA, times = n.unik)
multi.year[,"2013"] <- rep(NA, times = n.unik)
multi.year[,"2014"] <- rep(NA, times = n.unik)
multi.year[,"2015"] <- rep(NA, times = n.unik)
#View(multi.year)

seasons <- 2005:2015
n.seasons <- length(seasons)



for(i in 1:n.seasons) {
  year <- seasons[i]
  year.index <- which(final.nfl[,"Year"] == year)
  temp <- final.nfl[year.index, ]
  temp.index <- which(temp[,"AdjTouches"] >= limit)
  super.temp <- temp[temp.index, ]
  unik.x <- unique(super.temp)
  unik.y <- unik.x[,"Player"]
  n.unik.y <- length(unik.y)
  for(j in 1:n.unik.y){
    player <- unik.y[j]
    player.index <- which(super.temp[,"Player"] == player)
    multi.index <- which(multi.year[,"Player"] == player)
    more.temp <- super.temp[player.index, ]
    ultra.temp <- more.temp[c(2,4,23)]
    if(ultra.temp[,"Player"] == player){
      multi.year[multi.index, i + 2] <- ultra.temp[,"FER"]
      multi.year[multi.index, "FantPos"] <- ultra.temp[,"FantPos"]
    }
  }
}
#View(multi.year)



######### 2014 Analysis (FER)

prediction.year <- 2015
predictor.index <- which(multi.year[,"2015"] != 0 | multi.year[,"2014"] != 0 |
                        (multi.year[,"2013"] != 0 | multi.year[,"2012"] != 0))
                        predictor.temp <- multi.year[predictor.index, ]
#View(predictor.temp)

predictor.temp[,"Pre-Mean14"] <- rep(NA, times = nrow(predictor.temp))
predictor.temp[,"Pre-Mean15"] <- rep(NA, times = nrow(predictor.temp))
prob <- c(0.01, 0.03, 0.06, 0.08, 0.11, 0.13, 0.16, 0.19, 0.23)

positions <- c("RB", "WR", "TE")
n.positions <- length(positions)

for(j in 1:n.positions) {
  spot <- positions[j]
  spot.index <- which(predictor.temp[,"FantPos"] == spot)
  spot.temp <- predictor.temp[spot.index,]
  players <- spot.temp[,"Player"]
  n.predict <- length(players)
  for(i in 1:n.predict){
    player <- players[i]
    player.index <- which(predictor.temp[,"Player"] == player)
    vector <- predictor.temp[player.index,3:11]
    real.vector <- as.numeric(as.vector(vector[1,]))
    predict.mu <- weighted.mean(real.vector, prob, na.rm = TRUE)
    ### EDIT WEIGHTS!!!
    predictor.temp[player.index,"Pre-Mean14"] <- predict.mu
  }
}

#View(predictor.temp)


### Separating Positions
rb.index <- which(predictor.temp[,"FantPos"] == "RB")
rb.temp <- predictor.temp[rb.index,]

wr.index <- which(predictor.temp[,"FantPos"] == "WR")
wr.temp <- predictor.temp[wr.index,]

te.index <- which(predictor.temp[,"FantPos"] == "TE")
te.temp <- predictor.temp[te.index,]
#View(te.temp)
plot(rb.temp[,"Pre-Mean14"], rb.temp[,"2014"], 
     xlab = "FER Average Before 2014", ylab = "2014 FER", 
     main = "2014 FER vs. FER Before 2014", col = "blue")
points(wr.temp[,"Pre-Mean14"], wr.temp[,"2014"], col = "green")
points(te.temp[,"Pre-Mean14"], te.temp[,"2014"], col = "pink")

cor(rb.temp[,"Pre-Mean14"], rb.temp[,"2014"])
cor(wr.temp[,"Pre-Mean14"], wr.temp[,"2014"])
cor(te.temp[,"Pre-Mean14"], te.temp[,"2014"])

## Equations

#RB
rb.fit.pre_15 <- lm(rb.temp[,"2014"] ~ rb.temp[,"Pre-Mean14"])
rb.a.fit <- rb.fit.pre_15$coefficients[1]
rb.b.fit <- rb.fit.pre_15$coefficients[2]
abline(a = rb.a.fit, b = rb.b.fit, col = "dark blue")
rb.a.fit
rb.b.fit

#WR
wr.fit.pre_15 <- lm(wr.temp[,"2014"] ~ wr.temp[,"Pre-Mean14"])
wr.a.fit <- wr.fit.pre_15$coefficients[1]
wr.b.fit <- wr.fit.pre_15$coefficients[2]
abline(a = wr.a.fit, b = wr.b.fit, col = "dark green")
wr.a.fit
wr.b.fit

#TE
te.fit.pre_15 <- lm(te.temp[,"2014"] ~ te.temp[,"Pre-Mean14"])
te.a.fit <- fit.pre_15$coefficients[1]
te.b.fit <- fit.pre_15$coefficients[2]
abline(a = te.a.fit, b = te.b.fit, col = "red")
te.a.fit
te.b.fit

# 2015 Prediction
#View(predictor.temp)
prediction.year <- 2015
predictor.index2 <- which(predictor.temp[,"2015"] != 0 |
                            predictor.temp[,"2014"] != 0)
predictor.temp2 <- predictor.temp[predictor.index2, ]
#View(predictor.temp2)

### Separating Positions
rb.index <- which(predictor.temp2[,"FantPos"] == "RB")
rb.temp <- predictor.temp2[rb.index,]

wr.index <- which(predictor.temp2[,"FantPos"] == "WR")
wr.temp <- predictor.temp2[wr.index,]

te.index <- which(predictor.temp2[,"FantPos"] == "TE")
te.temp <- predictor.temp2[te.index,]
#RB
rb.temp[,"Pred2015"] <- rep(NA, times = nrow(rb.temp))

players <- rb.temp[,"Player"]
n.predict <- length(players)
prob2 <- c(0.01, 0.02, 0.03, 0.05, 0.07, 0.10, 0.13, 0.16, 0.19, 0.24)

for(i in 1:n.predict){
  player <- players[i]
  player.index <- which(rb.temp[,"Player"] == player)
  vector <- rb.temp[player.index,3:12]
  real.vector <- as.numeric(as.vector(vector[1,]))
  predict.mu <- weighted.mean(real.vector, prob2, na.rm = TRUE)
  rb.temp[player.index,"Pre-Mean15"] <- predict.mu
}

rb.temp["Pred2015"]<- rb.temp["Pre-Mean15"]*rb.b.fit+rb.a.fit

#WR
wr.temp[,"Pred2015"] <- rep(NA, times = nrow(wr.temp))

players <- wr.temp[,"Player"]
n.predict <- length(players)
prob2 <- c(0.01, 0.02, 0.03, 0.05, 0.07, 0.10, 0.13, 0.16, 0.19, 0.24)

for(i in 1:n.predict){
  player <- players[i]
  player.index <- which(wr.temp[,"Player"] == player)
  vector <- wr.temp[player.index,3:12]
  real.vector <- as.numeric(as.vector(vector[1,]))
  predict.mu <- weighted.mean(real.vector, prob2, na.rm = TRUE)
  wr.temp[player.index,"Pre-Mean15"] <- predict.mu
}

wr.temp["Pred2015"]<- wr.temp["Pre-Mean15"]*wr.b.fit+wr.a.fit

#TE
te.temp[,"Pred2015"] <- rep(NA, times = nrow(te.temp))

players <- te.temp[,"Player"]
n.predict <- length(players)
prob2 <- c(0.01, 0.02, 0.03, 0.05, 0.07, 0.10, 0.13, 0.16, 0.19, 0.24)

for(i in 1:n.predict){
  player <- players[i]
  player.index <- which(te.temp[,"Player"] == player)
  vector <- te.temp[player.index,3:12]
  real.vector <- as.numeric(as.vector(vector[1,]))
  predict.mu <- weighted.mean(real.vector, prob2, na.rm = TRUE)
  te.temp[player.index,"Pre-Mean15"] <- predict.mu
}

te.temp["Pred2015"]<- te.temp["Pre-Mean15"]*te.b.fit+te.a.fit

## Residual

#RB
rb.temp[,"Residual"] <- rep(NA, times = nrow(rb.temp))
rb.temp[,"Residual"] <- rb.temp[,"2015"] - rb.temp[,"Pred2015"]

rb.rmse <- sd(rb.temp[,"Residual"], na.rm = TRUE)
rb.rmse

#WR
wr.temp[,"Residual"] <- rep(NA, times = nrow(wr.temp))
wr.temp[,"Residual"] <- wr.temp[,"2015"] - wr.temp[,"Pred2015"]

wr.rmse <- sd(wr.temp[,"Residual"], na.rm = TRUE)
wr.rmse

#TE
te.temp[,"Residual"] <- rep(NA, times = nrow(te.temp))
te.temp[,"Residual"] <- te.temp[,"2015"] - te.temp[,"Pred2015"]

te.rmse <- sd(te.temp[,"Residual"], na.rm = TRUE)
te.rmse

### Make a Pre-Mean16 and final predictions
##RB
rb.temp[,"Pre-Mean16"] <- rep(NA, times = nrow(rb.temp))
rb.temp[,"Pred2016"] <- rep(NA, times = nrow(rb.temp))

players <- rb.temp[,"Player"]
n.predict <- length(players)
prob3 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12, 0.15, 0.19, 0.23)

for(i in 1:n.predict){
  player <- players[i]
  player.index <- which(rb.temp[,"Player"] == player)
  vector <- rb.temp[player.index,3:13]
  real.vector <- as.numeric(as.vector(vector[1,]))
  predict.mu <- weighted.mean(real.vector, prob3, na.rm = TRUE)
  rb.temp[player.index,"Pre-Mean16"] <- predict.mu
}

rb.temp["Pred2016"]<- rb.temp["Pre-Mean16"]*rb.b.fit+rb.a.fit

##WR
wr.temp[,"Pre-Mean16"] <- rep(NA, times = nrow(wr.temp))
wr.temp[,"Pred2016"] <- rep(NA, times = nrow(wr.temp))

players <- wr.temp[,"Player"]
n.predict <- length(players)
prob3 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12, 0.15, 0.19, 0.23)

for(i in 1:n.predict){
  player <- players[i]
  player.index <- which(wr.temp[,"Player"] == player)
  vector <- wr.temp[player.index,3:13]
  real.vector <- as.numeric(as.vector(vector[1,]))
  predict.mu <- weighted.mean(real.vector, prob3, na.rm = TRUE)
  wr.temp[player.index,"Pre-Mean16"] <- predict.mu
}

wr.temp["Pred2016"]<- wr.temp["Pre-Mean16"]*wr.b.fit+wr.a.fit 

wr.temp[,"Player"] <- sapply(wr.temp[,"Player"], as.character)
number <- which(wr.temp[,"Player"] == "Odell Beckham")
wr.temp[number,"Player"] <- "Odell Beckham Jr."


##TE
te.temp[,"Pre-Mean16"] <- rep(NA, times = nrow(te.temp))
te.temp[,"Pred2016"] <- rep(NA, times = nrow(te.temp))

players <- te.temp[,"Player"]
n.predict <- length(players)
prob3 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12, 0.15, 0.19, 0.23)

for(i in 1:n.predict){
  player <- players[i]
  player.index <- which(te.temp[,"Player"] == player)
  vector <- te.temp[player.index,3:13]
  real.vector <- as.numeric(as.vector(vector[1,]))
  predict.mu <- weighted.mean(real.vector, prob3, na.rm = TRUE)
  te.temp[player.index,"Pre-Mean16"] <- predict.mu
}

te.temp["Pred2016"]<- te.temp["Pre-Mean16"]*te.b.fit+te.a.fit





# projecting fantasy points
n.espn.players=length(espn.proj.reduced[,"Player"])
n.rb.players=length(rb.temp[,"Player"])
n.wr.players=length(wr.temp[,"Player"])
n.te.players=length(te.temp[,"Player"])
proj.fer.16=rep(NA, times=n.espn.players)
espn.proj.reduced[,"2016 FER proj"]=proj.fer.16
for (i in 1:n.espn.players) {
  name.espn=espn.proj.reduced[i, "Player"]
  if(espn.proj.reduced[i,"Pos"]=="RB") {
  for (j in 1:n.rb.players) {
    name.rb=rb.temp[j, "Player"]
    if(name.rb==name.espn){
      espn.proj.reduced[i, "2016 FER proj"]=rb.temp[j, "Pred2016"]
    }
  }
  } else if (espn.proj.reduced[i, "Pos"]=="WR"){
    for (j in 1:n.wr.players) {
      name.wr=wr.temp[j, "Player"]
      if(name.wr==name.espn){
        espn.proj.reduced[i, "2016 FER proj"]=wr.temp[j, "Pred2016"]
      }
    }
  } else if (espn.proj.reduced[i, "Pos"]=="TE"){
    for (j in 1:n.te.players) {
      name.te=te.temp[j, "Player"]
      if(name.te==name.espn){
        espn.proj.reduced[i, "2016 FER proj"]=te.temp[j, "Pred2016"]
      }
    }
  }
}

#espn.proj.reduced[,"2016 proj pts"]=espn.proj.reduced[,"AdjTouches"]*
  espn.proj.reduced[,"2016 FER proj"]/10
  
  
