### Getting the Data
nfl.boxscores <- read.csv("~/Desktop/Moneyball/nfl.boxscores.post.2005.csv", stringsAsFactors=FALSE)
View(nfl.boxscores)

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


### Avg. Yards per Rush/Reception/Pass per Year for 2010 (Working around “NA” in 2010)

n <- 6
index.2010 <- which(nfl.boxscores[,"Year"] == 2010)
vector <- nfl.boxscores[index.2010,]
vector <- vector[-c(575),]
rush.yds <- vector[,"RushYds"]
rush.att <- vector[,"RushAtt"]
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

nfl.boxscores[,"Touches"] <- nfl.boxscores[,"Receptions"] + nfl.boxscores[,"RushAtt"] + nfl.boxscores[,"PassAtt"]
View(nfl.boxscores)

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

### Calculate Efficiency Rating

reduced.nfl[,"FER"] <- ((reduced.nfl[,"RecYds"]) + 
                          (reduced.nfl[,"RushYds"]) +
                          60 * (reduced.nfl[,"AdjTD"])) /
  (reduced.nfl[,"AdjTouches"])

View(reduced.nfl)


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

View(final.rb)
View(final.catch)
View(final.wr)
View(final.te)

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
plot(jitter(final.nfl[,"Year"]), final.nfl[,"zFER"], xlab = "Season", ylab = "zFER", 
     main = "zTRB of All Players from 2005-2006 to 2015-2016, min 150 Adjusted Touches", 
     col = "blue", cex = 0.6)

## Best Players each Season in zFER
seasons <- 2005:2015
n.seasons <- length(seasons)

max.players <- data.frame(Year = rep(NA, times = 11), Name = rep(NA, times = 11), 
                          FER = rep(NA, times = 11), zFER = rep(NA, times = 11))

for(i in 1:n.seasons){
  year <- seasons[i]
  year.index <- which(final.nfl[,"Year"] == year)
  temp <- final.nfl[year.index,]
  max.data <- temp[which.max(temp[,"zFER"]),]
  new.max <- max.data[c(1,2,23,24)]
  max.players[i,] <- new.max
}

View(max.players)

## Look at Specific Players
player <- "LeSean McCoy"
player.index <- which(final.nfl[,"Player"] == player)
player.temp <- final.nfl[player.index, ]
View(player.temp)

plot(player.temp[,"Year"], player.temp[,"zFER"], xlab = "Season", ylab = "zFER", 
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


