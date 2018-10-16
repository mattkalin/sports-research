### Getting the Data
nfl.boxscores <- read.csv("~/Desktop/Moneyball/nfl_boxscores.csv", stringsAsFactors=FALSE)
###############
### xFER
###############

### Seasons Vectors

seasons <- 2005:2015
n.seasons <- length(seasons)

### Avg. Yards per Rush and Catch per Year


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

### Avg. Yards per Rush/Reception/Pass per Year (Year 6 SUCKS!!!)

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

### Forming the TD-Equality Ratio

ratio.td <- avg.tdpcatch / avg.tdprush
ratio.td

### Add a column for touches

nfl.boxscores[,"Touches"] <- nfl.boxscores[,"Receptions"] + nfl.boxscores[,"RushAtt"]

### Remove non-RB,WR,TEs

index <- which(nfl.boxscores[,"FantPos"] == "RB" | 
                 nfl.boxscores[,"FantPos"] == "WR" | 
                 nfl.boxscores[,"FantPos"] == "TE")

reduced.nfl <- nfl.boxscores[index, ]

### Seasons Vectors (Again)

seasons <- 2005:2015
n.seasons <- length(seasons)

### Add a column for ADJUSTED touches and ADJUSTED TDs
nflrows <- nrow(reduced.nfl)
reduced.nfl[,"AdjTouches"] <- rep(NA , times = nflrows)
reduced.nfl[,"AdjTD"] <- rep(NA , times = nflrows)

for(i in 1:n.seasons){
  year <- seasons[i]
  index.year <- which(reduced.nfl[, "Year"] == year) 
  temp <- reduced.nfl[index.year, ]
  #Touches
  temp[,"AdjTouches"] <- ratio.receive[i]*temp[,"Receptions"] + temp[,"RushAtt"]
  adjtouches <- temp[,"AdjTouches"]
  reduced.nfl[index.year,"AdjTouches"] <- adjtouches
  #temp[,"AdjTD"] <- ratio.td[i]*temp[,"RushTD"] + temp[,"RecTD"]
  temp[,"AdjTD"] <- temp[,"RushTD"] + temp[,"RecTD"]
  adjtds <- temp[,"AdjTD"]
  reduced.nfl[index.year,"AdjTD"] <- adjtds
}
### Calculate Fantasy Efficiency Rating

reduced.nfl[,"FER"] <- ((reduced.nfl[,"RecYds"]) + 
                          (reduced.nfl[,"RushYds"]) +
                          60 * (reduced.nfl[,"AdjTD"])) /
  (reduced.nfl[,"AdjTouches"])


### Set parameters
limit <- 150
final.index <- which(reduced.nfl[,"AdjTouches"] >= limit)
final.nfl <- reduced.nfl[final.index, ]

### Finding Duplicates
year <- 2010
first.index <- which(final.nfl[,"Year"] == year)
first.temp <- final.nfl[first.index, ]
h.index <- duplicated(first.temp[,"Player"])
h.temp <- first.temp[h.index, ]

### Eliminate duplicates
number <- which(final.nfl[,"Player"] == "Adrian Peterson" & final.nfl[,"Tm"] == "CHI")
final.nfl[number,"Player"] <- "Adrian Peterson 2"

number <- which(final.nfl[,"Player"] == "Steve Smith" & final.nfl[,"Tm"] == "NYG")
final.nfl[number,"Player"] <- "Steve Smith 2"

number <- which(final.nfl[,"Player"] == "Mike Williams" & final.nfl[,"Tm"] == "SEA")
final.nfl[number,"Player"] <- "Mike Williams 2"

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

final.nfl[which.max(final.nfl[,"FER"]),]
final.nfl[which.min(final.nfl[,"FER"]),]


## Max and Min (Separate)
# RB
final.rb[which.max(final.rb[,"FER"]),]
final.rb[which.min(final.rb[,"FER"]),]
# Receivers
final.catch[which.max(final.catch[,"FER"]),]
final.catch[which.min(final.catch[,"FER"]),]
# WR
final.wr[which.max(final.wr[,"FER"]),]
final.wr[which.min(final.wr[,"FER"]),]
# TE
final.te[which.max(final.te[,"FER"]),]
final.te[which.min(final.te[,"FER"]),]

###### Standardization

## Standardize per position

positions <- c("RB", "WR", "TE")
n.positions <- length(positions)
final.nfl[,"zFER"] <- rep(NA, times = nrow(final.nfl))

for(i in 1:n.positions){
  spot <- positions[i]
  z.index <- which(final.nfl[,"FantPos"] == spot)
  z.temp <- final.nfl[z.index,]
  z.mu <- mean(z.temp[,"FER"])
  z.sd <- sd(z.temp[,"FER"])
  z.fer <- (z.temp[,"FER"] - z.mu) / z.sd
  final.nfl[z.index,"zFER"] <- z.fer
}

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

## Histograms

#hist(final.qb[,"zFER"])
hist(final.rb[,"zFER"], main = "Standardized FER: RBs", xlab = "zFER", col = "lightgreen", ylim = c(0,0.6), probability = TRUE)
lines(density(final.rb[,"zFER"]), col = "red", lwd = 2)
hist(final.catch[,"zFER"], main = "Standardized FER: WR and TE", xlab = "zFER", col = "lightblue", ylim = c(0,0.6), probability = TRUE)
lines(density(final.catch[,"zFER"]), col = "red", lwd = 2)
hist(final.wr[,"zFER"], main = "Standardized FER: WRs", xlab = "zFER", col = "tan", ylim = c(0,0.6), probability = TRUE)
lines(density(final.wr[,"zFER"]), col = "red", lwd = 2)
hist(final.te[,"zFER"], main = "Standardized FER: TEs", xlab = "zFER", col = "purple", ylim = c(0,0.6), probability = TRUE)
lines(density(final.te[,"zFER"]), col = "red", lwd = 2)
hist(final.nfl[,"zFER"], main = "Standardized FER: RB, WR, and TE", xlab = "zFER", col = "grey", ylim = c(0,0.6), probability = TRUE)
lines(density(final.nfl[,"zFER"]), col = "red", lwd = 2)

## Min and Max (zFER)

final.nfl[which.max(final.nfl[,"zFER"]),]
final.nfl[which.min(final.nfl[,"zFER"]),]
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
plot(jitter(final.nfl[,"Year"]), final.nfl[,"zFER"], xlab = "Season", ylab = "zFER", 
     main = "zFER of All Players from 2005-2006 to 2015-2016, min 150 Adjusted Touches", 
     col = "blue", cex = 0.6)

## Best Players each Season in zFER
seasons <- 2005:2015
n.seasons <- length(seasons)

max.players <- data.frame(Year = rep(NA, times = 11), Player = rep(NA, times = 11), 
                          FER = rep(NA, times = 11), zFER = rep(NA, times = 11))

for(i in 1:n.seasons){
  year <- seasons[i]
  year.index <- which(final.nfl[,"Year"] == year)
  temp <- final.nfl[year.index,]
  max.data <- temp[which.max(temp[,"zFER"]),]
  new.max <- max.data[c(1,2,23,24)]
  max.players[i,] <- new.max
}

## Look at Specific Players
player <- "DeAndre Hopkins"
player.index <- which(final.nfl[,"Player"] == player)
player.temp <- final.nfl[player.index, ]

plot(player.temp[,"Year"], player.temp[,"zFER"], xlab = "Season", ylab = "zFER", 
     main = "zFER of Specific Player, min 150 Adjusted Touches", 
     col = "red" , cex = 0.8, pch = 16)



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



######### 2014 Analysis (FER)

prediction.year <- 2015
predictor.index <- which(multi.year[,"2015"] != 0 | (multi.year[,"2014"] != 0 | 
                                                       multi.year[,"2013"] != 0 | multi.year[,"2012"] != 0))
predictor.temp <- multi.year[predictor.index, ]

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



### Separating Positions
rb.index <- which(predictor.temp[,"FantPos"] == "RB")
rb.temp <- predictor.temp[rb.index,]

wr.index <- which(predictor.temp[,"FantPos"] == "WR")
wr.temp <- predictor.temp[wr.index,]

te.index <- which(predictor.temp[,"FantPos"] == "TE")
te.temp <- predictor.temp[te.index,]

plot(rb.temp[,"Pre-Mean14"], rb.temp[,"2014"], 
     xlab = "FER Average Before 2014", ylab = "2014 FER", main = "2014 FER vs. FER Before 2014", col = "blue")
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
te.a.fit <- te.fit.pre_15$coefficients[1]
te.b.fit <- te.fit.pre_15$coefficients[2]
abline(a = te.a.fit, b = te.b.fit, col = "red")
te.a.fit
te.b.fit

# 2015 Prediction
prediction.year <- 2015
predictor.index2 <- which(predictor.temp[,"2015"] != 0 | predictor.temp[,"2014"] != 0)
predictor.temp2 <- predictor.temp[predictor.index2, ]

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
unadj.rb.rmse <- rb.rmse

#WR
wr.temp[,"Residual"] <- rep(NA, times = nrow(wr.temp))
wr.temp[,"Residual"] <- wr.temp[,"2015"] - wr.temp[,"Pred2015"]

wr.rmse <- sd(wr.temp[,"Residual"], na.rm = TRUE)
wr.rmse
unadj.wr.rmse <- wr.rmse

#TE
te.temp[,"Residual"] <- rep(NA, times = nrow(te.temp))
te.temp[,"Residual"] <- te.temp[,"2015"] - te.temp[,"Pred2015"]

te.rmse <- sd(te.temp[,"Residual"], na.rm = TRUE)
te.rmse
unadj.te.rmse <- te.rmse

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

wr.temp["Pred2016"] <- wr.temp["Pre-Mean16"]*wr.b.fit+wr.a.fit 

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


### Projected Touches

espy.projections <- read.csv("~/Desktop/Moneyball/2016 fantasy football FLEX projections.csv", stringsAsFactors=FALSE)

# Cleaning Data
n.players=length(espy.projections[,"PLAYER..TEAM.POS"])
proj.players = sub('\\s*,.*','', espy.projections[,"PLAYER..TEAM.POS"])
espn.proj.reduced2 = data.frame("Player"=proj.players)
toString(espn.proj.reduced2[,"Player"])
espn.proj.reduced2[,"Player"]=sapply(espn.proj.reduced2[,"Player"], as.character)
for (i in 1:n.players) { # this should get rid of injury *s
  short.name=toString(espn.proj.reduced2[i,"Player"])
  short.name.length=nchar(short.name)
  if(substr(short.name, short.name.length, short.name.length)=="*"){
    espn.proj.reduced2[i,"Player"]=substr(short.name, 1, short.name.length-1)
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
espn.proj.reduced2[,"Pos"]=proj.positions
espn.proj.reduced2[,"RushAtt"]=espy.projections[,"RUSH"]
espn.proj.reduced2[,"Receptions"]=espy.projections[,"REC"]
#View(espn.proj.reduced2)

adj.ratio <- mean(ratio.receive)
espn.proj.reduced2[,"AdjTouches"] <- adj.ratio*espn.proj.reduced2[,"Receptions"] + 
  espn.proj.reduced2[,"RushAtt"]



## Matching to Other Temps
#RB
rb.index <- which(espn.proj.reduced2[,"Pos"] == "RB")
rb.other <- espn.proj.reduced2[rb.index, ]
it <- rb.temp[,"Player"]

rb.new <- rb.other[1,]
rb.players <- rb.other[,"Player"]
n.rb <- length(rb.players)
for(i in 1:n.rb) {
  him <- rb.players[i]
  index.him <- which(rb.other[,"Player"] == him)
  if(is.na(match(him, it))) {} else {
    newrow <- rb.other[index.him,]
    rb.new <- rbind(rb.new, newrow)
  }
}
rb.new <- rb.new[-c(1),]

#WR
wr.index <- which(espn.proj.reduced2[,"Pos"] == "WR")
wr.other <- espn.proj.reduced2[wr.index, ]
it <- wr.temp[,"Player"]

wr.players <- wr.other[,"Player"]
n.wr <- length(wr.players)
wr.new <- wr.other[1,]
for(i in 1:n.wr) {
  him <- wr.players[i]
  index.him <- which(wr.other[,"Player"] == him)
  if(is.na(match(him, it))) {} else {
    newrow <- wr.other[index.him,]
    wr.new <- rbind(wr.new, newrow)
  }
}
wr.new <- wr.new[-c(1),]

#TE
te.index <- which(espn.proj.reduced2[,"Pos"] == "TE")
te.other <- espn.proj.reduced2[te.index, ]
it <- te.temp[,"Player"]

te.players <- te.other[,"Player"]
n.te <- length(te.players)
te.new <- te.other[1,]
for(i in 1:n.te) {
  him <- te.players[i]
  index.him <- which(te.other[,"Player"] == him)
  if(is.na(match(him, it))) {} else {
    newrow <- te.other[index.him,]
    te.new <- rbind(te.new, newrow)
  }
}

te.new <- te.new[-c(1),]


### Transferring Data
# Projecting fantasy points
n.espn.players=length(espn.proj.reduced2[,"Player"])
n.rb.players=length(rb.temp[,"Player"])
n.wr.players=length(wr.temp[,"Player"])
n.te.players=length(te.temp[,"Player"])
proj.fer.16=rep(NA, times=n.espn.players)
espn.proj.reduced2[,"2016 FER proj"]=proj.fer.16
for (i in 1:n.espn.players) {
  name.espn=espn.proj.reduced2[i, "Player"]
  if(espn.proj.reduced2[i,"Pos"]=="RB") {
    for (j in 1:n.rb.players) {
      name.rb=rb.temp[j, "Player"]
      name.rb <- sapply(name.rb, as.character)
      name.espn <- sapply(name.espn, as.character)
      if(name.rb == name.espn){
        espn.proj.reduced2[i, "2016 FER proj"]=rb.temp[j, "Pred2016"]
      }
    }
  } else if (espn.proj.reduced2[i, "Pos"]=="WR"){
    for (j in 1:n.wr.players) {
      name.wr=wr.temp[j, "Player"]
      name.wr <- sapply(name.wr, as.character)
      name.espn <- sapply(name.espn, as.character)
      if(name.wr==name.espn){
        espn.proj.reduced2[i, "2016 FER proj"]=wr.temp[j, "Pred2016"]
      }
    }
  } else if (espn.proj.reduced2[i, "Pos"]=="TE"){
    for (j in 1:n.te.players) {
      name.te=te.temp[j, "Player"]
      name.te <- sapply(name.te, as.character)
      name.espn <- sapply(name.espn, as.character)
      if(name.te==name.espn){
        espn.proj.reduced2[i, "2016 FER proj"]=te.temp[j, "Pred2016"]
      }
    }
  }
}

espn.proj.reduced2[,"2016 proj pts"]=(espn.proj.reduced2[,"AdjTouches"]*
                                        espn.proj.reduced2[,"2016 FER proj"])/10


######
# RIGHT STUFF
######


### Seasons Vectors

seasons <- 2005:2015
n.seasons <- length(seasons)

### Avg. Yards per Rush and Catch per Year


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

### Avg. Yards per Rush/Reception/Pass per Year (Year 6 SUCKS!!!)

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

### Forming the TD-Equality Ratio

ratio.td <- avg.tdpcatch / avg.tdprush
ratio.td

### Add a column for touches

nfl.boxscores[,"Touches"] <- nfl.boxscores[,"Receptions"] + nfl.boxscores[,"RushAtt"]

### Remove non-RB,WR,TEs

index <- which(nfl.boxscores[,"FantPos"] == "RB" | 
                 nfl.boxscores[,"FantPos"] == "WR" | 
                 nfl.boxscores[,"FantPos"] == "TE")

reduced.nfl <- nfl.boxscores[index, ]

### Seasons Vectors (Again)

seasons <- 2005:2015
n.seasons <- length(seasons)

### Add a column for ADJUSTED touches and ADJUSTED TDs
nflrows <- nrow(reduced.nfl)
reduced.nfl[,"AdjTouches"] <- rep(NA , times = nflrows)
reduced.nfl[,"AdjTD"] <- rep(NA , times = nflrows)

for(i in 1:n.seasons){
  year <- seasons[i]
  index.year <- which(reduced.nfl[, "Year"] == year) 
  temp <- reduced.nfl[index.year, ]
  #Touches
  temp[,"AdjTouches"] <- ratio.receive[i]*temp[,"Receptions"] + temp[,"RushAtt"]
  adjtouches <- temp[,"AdjTouches"]
  reduced.nfl[index.year,"AdjTouches"] <- adjtouches
  temp[,"AdjTD"] <- ratio.td[i]*temp[,"RushTD"] + temp[,"RecTD"]
  adjtds <- temp[,"AdjTD"]
  reduced.nfl[index.year,"AdjTD"] <- adjtds
}
### Calculate Fantasy Efficiency Rating

reduced.nfl[,"FER"] <- ((reduced.nfl[,"RecYds"]) + 
                          (reduced.nfl[,"RushYds"]) +
                          60 * (reduced.nfl[,"AdjTD"])) /
  (reduced.nfl[,"AdjTouches"])


### Set parameters
limit <- 150
final.index <- which(reduced.nfl[,"AdjTouches"] >= limit)
final.nfl <- reduced.nfl[final.index, ]

### Finding Duplicates
year <- 2010
first.index <- which(final.nfl[,"Year"] == year)
first.temp <- final.nfl[first.index, ]
h.index <- duplicated(first.temp[,"Player"])
h.temp <- first.temp[h.index, ]

### Eliminate duplicates
number <- which(final.nfl[,"Player"] == "Adrian Peterson" & final.nfl[,"Tm"] == "CHI")
final.nfl[number,"Player"] <- "Adrian Peterson 2"

number <- which(final.nfl[,"Player"] == "Steve Smith" & final.nfl[,"Tm"] == "NYG")
final.nfl[number,"Player"] <- "Steve Smith 2"

number <- which(final.nfl[,"Player"] == "Mike Williams" & final.nfl[,"Tm"] == "SEA")
final.nfl[number,"Player"] <- "Mike Williams 2"

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

final.nfl[which.max(final.nfl[,"FER"]),]
final.nfl[which.min(final.nfl[,"FER"]),]


## Max and Min (Separate)
# RB
final.rb[which.max(final.rb[,"FER"]),]
final.rb[which.min(final.rb[,"FER"]),]
# Receivers
final.catch[which.max(final.catch[,"FER"]),]
final.catch[which.min(final.catch[,"FER"]),]
# WR
final.wr[which.max(final.wr[,"FER"]),]
final.wr[which.min(final.wr[,"FER"]),]
# TE
final.te[which.max(final.te[,"FER"]),]
final.te[which.min(final.te[,"FER"]),]

###### Standardization

## Standardize per position

positions <- c("RB", "WR", "TE")
n.positions <- length(positions)
final.nfl[,"zFER"] <- rep(NA, times = nrow(final.nfl))

for(i in 1:n.positions){
  spot <- positions[i]
  z.index <- which(final.nfl[,"FantPos"] == spot)
  z.temp <- final.nfl[z.index,]
  z.mu <- mean(z.temp[,"FER"])
  z.sd <- sd(z.temp[,"FER"])
  z.fer <- (z.temp[,"FER"] - z.mu) / z.sd
  final.nfl[z.index,"zFER"] <- z.fer
}

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

## Histograms

#hist(final.qb[,"zFER"])
hist(final.rb[,"zFER"], main = "Standardized FER: RBs", xlab = "zFER", col = "lightgreen", ylim = c(0,0.6), probability = TRUE)
lines(density(final.rb[,"zFER"]), col = "red", lwd = 2)
hist(final.catch[,"zFER"], main = "Standardized FER: WR and TE", xlab = "zFER", col = "lightblue", ylim = c(0,0.6), probability = TRUE)
lines(density(final.catch[,"zFER"]), col = "red", lwd = 2)
hist(final.wr[,"zFER"], main = "Standardized FER: WRs", xlab = "zFER", col = "tan", ylim = c(0,0.6), probability = TRUE)
lines(density(final.wr[,"zFER"]), col = "red", lwd = 2)
hist(final.te[,"zFER"], main = "Standardized FER: TEs", xlab = "zFER", col = "purple", ylim = c(0,0.6), probability = TRUE)
lines(density(final.te[,"zFER"]), col = "red", lwd = 2)
hist(final.nfl[,"zFER"], main = "Standardized FER: RB, WR, and TE", xlab = "zFER", col = "grey", ylim = c(0,0.6), probability = TRUE)
lines(density(final.nfl[,"zFER"]), col = "red", lwd = 2)

## Min and Max (zFER)

final.nfl[which.max(final.nfl[,"zFER"]),]
final.nfl[which.min(final.nfl[,"zFER"]),]
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
plot(jitter(final.nfl[,"Year"]), final.nfl[,"zFER"], xlab = "Season", ylab = "zFER", 
     main = "zFER of All Players from 2005-2006 to 2015-2016, min 150 Adjusted Touches", 
     col = "blue", cex = 0.6)

## Best Players each Season in zFER
seasons <- 2005:2015
n.seasons <- length(seasons)

max.players <- data.frame(Year = rep(NA, times = 11), Player = rep(NA, times = 11), 
                          FER = rep(NA, times = 11), zFER = rep(NA, times = 11))

for(i in 1:n.seasons){
  year <- seasons[i]
  year.index <- which(final.nfl[,"Year"] == year)
  temp <- final.nfl[year.index,]
  max.data <- temp[which.max(temp[,"zFER"]),]
  new.max <- max.data[c(1,2,23,24)]
  max.players[i,] <- new.max
}

## Look at Specific Players
player <- "DeAndre Hopkins"
player.index <- which(final.nfl[,"Player"] == player)
player.temp <- final.nfl[player.index, ]

plot(player.temp[,"Year"], player.temp[,"zFER"], xlab = "Season", ylab = "zFER", 
     main = "zFER of Specific Player, min 150 Adjusted Touches", 
     col = "red" , cex = 0.8, pch = 16)



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
#view(multi.year)



######### 2014 Analysis (FER)

prediction.year <- 2015
predictor.index <- which(multi.year[,"2015"] != 0 | (multi.year[,"2014"] != 0 | 
                                                       multi.year[,"2013"] != 0 | multi.year[,"2012"] != 0))
predictor.temp <- multi.year[predictor.index, ]

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



### Separating Positions
rb.index <- which(predictor.temp[,"FantPos"] == "RB")
rb.temp <- predictor.temp[rb.index,]

wr.index <- which(predictor.temp[,"FantPos"] == "WR")
wr.temp <- predictor.temp[wr.index,]

te.index <- which(predictor.temp[,"FantPos"] == "TE")
te.temp <- predictor.temp[te.index,]

plot(rb.temp[,"Pre-Mean14"], rb.temp[,"2014"], 
     xlab = "FER Average Before 2014", ylab = "2014 FER", main = "2014 FER vs. FER Before 2014", col = "blue")
points(wr.temp[,"Pre-Mean14"], wr.temp[,"2014"], col = "green")
points(te.temp[,"Pre-Mean14"], te.temp[,"2014"], col = "red")
legend("bottomright", c("RB", "WR", "TE"), col = c("blue", "green", "red"), lty = c(1,1,1), lwd = c(2,2,2))


rb.jinx <- which(rb.temp[,"Pre-Mean14"] != 0 & rb.temp[,"2014"] != 0)
rb.jinx.temp <- rb.temp[rb.jinx,]
cor(rb.jinx.temp[,"Pre-Mean14"], rb.jinx.temp[,"2014"])

wr.jinx <- which(wr.temp[,"Pre-Mean14"] != 0 & wr.temp[,"2014"] != 0)
wr.jinx.temp <- wr.temp[wr.jinx,]
cor(wr.jinx.temp[,"Pre-Mean14"], wr.jinx.temp[,"2014"])

te.jinx <- which(te.temp[,"Pre-Mean14"] != 0 & te.temp[,"2014"] != 0)
te.jinx.temp <- te.temp[te.jinx,]
cor(te.jinx.temp[,"Pre-Mean14"], te.jinx.temp[,"2014"])

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
te.a.fit <- te.fit.pre_15$coefficients[1]
te.b.fit <- te.fit.pre_15$coefficients[2]
abline(a = te.a.fit, b = te.b.fit, col = "red")
te.a.fit
te.b.fit

# 2015 Prediction

prediction.year <- 2015
predictor.index2 <- which(predictor.temp[,"2015"] != 0 | predictor.temp[,"2014"] != 0)
predictor.temp2 <- predictor.temp[predictor.index2, ]


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


plot(rb.temp[,"Pre-Mean15"], rb.temp[,"2015"], 
     xlab = "FER Average Before 2015", ylab = "2015 FER", main = "2015 FER vs. FER Before 2015", col = "blue")
points(wr.temp[,"Pre-Mean15"], wr.temp[,"2015"], col = "green")
points(te.temp[,"Pre-Mean15"], te.temp[,"2015"], col = "red")
legend("bottomright", c("RB", "WR", "TE"), col = c("blue", "green", "red"), lty = c(1,1,1), lwd = c(2,2,2))

abline(a = rb.a.fit, b = rb.b.fit, col = "dark blue")
abline(a = wr.a.fit, b = wr.b.fit, col = "dark green")
abline(a = te.a.fit, b = te.b.fit, col = "red")

#view(rb.temp)
#view(wr.temp)
#view(te.temp)
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

wr.temp["Pred2016"] <- wr.temp["Pre-Mean16"]*wr.b.fit+wr.a.fit 

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


### Projected Touches

espy.projections <- read.csv("~/Desktop/Moneyball/2016 fantasy football FLEX projections.csv", stringsAsFactors=FALSE)

# Cleaning Data
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
#view(espn.proj.reduced)

adj.ratio <- mean(ratio.receive)
espn.proj.reduced[,"AdjTouches"] <- adj.ratio*espn.proj.reduced[,"Receptions"] + 
  espn.proj.reduced[,"RushAtt"]



## Matching to Other Temps
#RB
rb.index <- which(espn.proj.reduced[,"Pos"] == "RB")
rb.other <- espn.proj.reduced[rb.index, ]
it <- rb.temp[,"Player"]

rb.new <- rb.other[1,]
rb.players <- rb.other[,"Player"]
n.rb <- length(rb.players)
for(i in 1:n.rb) {
  him <- rb.players[i]
  index.him <- which(rb.other[,"Player"] == him)
  if(is.na(match(him, it))) {} else {
    newrow <- rb.other[index.him,]
    rb.new <- rbind(rb.new, newrow)
  }
}
rb.new <- rb.new[-c(1),]

#WR
wr.index <- which(espn.proj.reduced[,"Pos"] == "WR")
wr.other <- espn.proj.reduced[wr.index, ]
it <- wr.temp[,"Player"]

wr.players <- wr.other[,"Player"]
n.wr <- length(wr.players)
wr.new <- wr.other[1,]
for(i in 1:n.wr) {
  him <- wr.players[i]
  index.him <- which(wr.other[,"Player"] == him)
  if(is.na(match(him, it))) {} else {
    newrow <- wr.other[index.him,]
    wr.new <- rbind(wr.new, newrow)
  }
}
wr.new <- wr.new[-c(1),]

#TE
te.index <- which(espn.proj.reduced[,"Pos"] == "TE")
te.other <- espn.proj.reduced[te.index, ]
it <- te.temp[,"Player"]

te.players <- te.other[,"Player"]
n.te <- length(te.players)
te.new <- te.other[1,]
for(i in 1:n.te) {
  him <- te.players[i]
  index.him <- which(te.other[,"Player"] == him)
  if(is.na(match(him, it))) {} else {
    newrow <- te.other[index.him,]
    te.new <- rbind(te.new, newrow)
  }
}

te.new <- te.new[-c(1),]


### Transferring Data
# Projecting fantasy points
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
      name.rb <- sapply(name.rb, as.character)
      name.espn <- sapply(name.espn, as.character)
      if(name.rb == name.espn){
        espn.proj.reduced[i, "2016 FER proj"]=rb.temp[j, "Pred2016"]
      }
    }
  } else if (espn.proj.reduced[i, "Pos"]=="WR"){
    for (j in 1:n.wr.players) {
      name.wr=wr.temp[j, "Player"]
      name.wr <- sapply(name.wr, as.character)
      name.espn <- sapply(name.espn, as.character)
      if(name.wr==name.espn){
        espn.proj.reduced[i, "2016 FER proj"]=wr.temp[j, "Pred2016"]
      }
    }
  } else if (espn.proj.reduced[i, "Pos"]=="TE"){
    for (j in 1:n.te.players) {
      name.te=te.temp[j, "Player"]
      name.te <- sapply(name.te, as.character)
      name.espn <- sapply(name.espn, as.character)
      if(name.te==name.espn){
        espn.proj.reduced[i, "2016 FER proj"]=te.temp[j, "Pred2016"]
      }
    }
  }
}

espn.proj.reduced[,"2016 proj pts"]=(espn.proj.reduced[,"AdjTouches"]*
                                       espn.proj.reduced[,"2016 FER proj"])/10

final.table <- espn.proj.reduced[,c(1,2,3,4,5,6)]
final.table[,"2016 proj pts"] <- espn.proj.reduced2[,"2016 proj pts"]
#view(final.table)

## Adding RMSE

positions <- c("RB", "WR", "TE")
n.positions <- length(positions)
final.table[,"FER +/-"] <- rep(NA, times = nrow(final.table))
rmse.v <- c(rb.rmse, wr.rmse, te.rmse)
final.table[,"2016 pts +/-"] <- rep(NA, times = nrow(final.table))
unadj.rmse.v <- c(unadj.rb.rmse, unadj.wr.rmse, unadj.te.rmse)
final.table[,"2016 proj zFER"] <- rep(NA, times = nrow(final.table))

for(i in 1:n) {
  spot <- positions[i]
  spot.index <- which(final.table[,"Pos"] == spot)
  spot.temp <- final.table[spot.index,]
  spot.temp[,"FER +/-"] <- rmse.v[i]
  final.table[spot.index,"FER +/-"] <- spot.temp[,"FER +/-"]
  spot.temp[,"2016 pts +/-"] <- (unadj.rmse.v[i] * spot.temp[,"AdjTouches"]) / 10
  final.table[spot.index,"2016 pts +/-"] <- spot.temp[,"2016 pts +/-"]
  z.mu <- mean(spot.temp[,"2016 FER proj"], na.rm = TRUE)
  z.sd <- sd(spot.temp[,"2016 FER proj"], na.rm = TRUE)
  z.fer <- (spot.temp[,"2016 FER proj"] - z.mu) / z.sd
  final.table[spot.index,"2016 proj zFER"] <- z.fer
}

final.table <- final.table[,c(1,2,3,4,5,6,10,8,9,7)]


rb.index <- which(final.table[,"Pos"] == "RB")
rb.temp <- final.table[rb.index,]
rb.temp[,"Number"] <- 1
final.table[rb.index,"Number"] <- rb.temp[,"Number"]

wr.index <- which(final.table[,"Pos"] == "WR")
wr.temp <- final.table[wr.index,]
wr.temp[,"Number"] <- 2
final.table[wr.index,"Number"] <- wr.temp[,"Number"]

te.index <- which(final.table[,"Pos"] == "TE")
te.temp <- final.table[te.index,]
te.temp[,"Number"] <- 3
final.table[te.index,"Number"] <- te.temp[,"Number"]

plot(jitter(final.table[,"Number"]), final.table[,"2016 FER proj"], main = "Predicted 2016 FER by Position", 
     xlab = "Position (1 = RB, 2 = WR, 3 = TE)", ylab = "2016 Predicted FER", col = "dark green")

plot(jitter(final.table[,"Number"]), final.table[,"2016 proj pts"], main = "Predicted 2016 Fantasy Points by Position", 
     xlab = "Position (1 = RB, 2 = WR, 3 = TE)", ylab = "2016 Predicted Fantasy Points", col = "blue")

# number of seasons played
n.players=length(multi.year[,"Player"])
for (i in 1:n.players) {
  multi.year[i, "Seasons"]=sum(multi.year[i, 3:13]>0, na.rm = TRUE)
}

# add experience to final table
final.n.players=228
final.table[,"Seasons"]=rep(NA, times=final.n.players)
for (i in 1:final.n.players) {
  name.final=final.table[i, "Player"]
  for (j in 1:n.players) {
    name.multi=multi.year[j, "Player"]
    if(name.final==name.multi){
      final.table[i, "Seasons"]=multi.year[j, "Seasons"]
    }
  }
}
final.table[2, "Seasons"]=2

#na.zero <- function (x) { # creates na.zero function
#  x[is.na(x)] <- 0        # turns NAs into 0s
#  return(x)
#}

#na.zero(final.table[,"2016 proj pts"])

final.table[is.na(final.table)]=0
index.touches=which(final.table[,"AdjTouches"]>50)
index.final=which(final.table[,"2016 proj pts"]>0)

final.table.1.5=final.table[index.touches,]
final.table.2=final.table.1.5[index.final,]
plus.min=final.table.2[,"2016 pts +/-"]
plus.min=plus.min*sqrt(3.2/final.table.2[,"Seasons"])
final.table.2[,"2016 pts +/-"]=plus.min


public.table=data.frame("Player"=final.table.2[,"Player"])
public.table[,"Pos"]=final.table.2[,"Pos"]
public.table[,"Avg Pts"]=round(final.table.2[,"2016 proj pts"], digits = 1)
public.table[,"Ceiling"]=round(final.table.2[,"2016 proj pts"]+
                          2*final.table.2[,"2016 pts +/-"], digits=1)
public.table[,"Floor"]=round(final.table.2[,"2016 proj pts"]-
                                 2*final.table.2[,"2016 pts +/-"], digits=1)
public.table[,"Seasons"]=final.table.2[,"Seasons"]
public.table[,"+/-"]=round(final.table.2[,"2016 pts +/-"], digits = 1)


write.xlsx(public.table, "fantasy.projections.xlsx")

