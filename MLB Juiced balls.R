MLB_HR_rate <- read_excel("~/Desktop/Summer 2017/MLB HR rate.xlsx")
MLB_HR_rate$Num=1:27
{
plot(MLB_HR_rate$Months, 100*MLB_HR_rate$`HR/AB`, type = "p", 
    ylab = "HR% per AB", xlab="Month (April 2013-June 2017)", 
    xaxt="n", main = "Home Runs per At-Bat")
lines(MLB_HR_rate$Months, 100*MLB_HR_rate$`HR/AB`, col="red")
abline(v=c(10, 22, 34, 46), col="blue") # separate seasons
axis(1, at=c(3.5, 15.5, 27.5, 39.5, 50), 
     labels = as.character(2013:2017)) # label seasons
points(28, 100*MLB_HR_rate[16, "HR/AB"], lwd=3)
text(28, 100*MLB_HR_rate[16, "HR/AB"], 
     labels = "July 2015", pos = 4, cex = 0.7)
} # HR/AB
{
plot(MLB_HR_rate$Months, 100*MLB_HR_rate$`HR/FB`, type = "p", 
     ylab = "HR% per FB", xlab="Month (April 2013-June 2017)", 
     xaxt="n", main = "Home Runs per Fly Ball")
lines(MLB_HR_rate$Months, 100*MLB_HR_rate$`HR/FB`, col="red")
abline(v=c(10, 22, 34, 46), col="blue") # separate seasons
axis(1, at=c(3.5, 15.5, 27.5, 39.5, 50), 
     labels = as.character(2013:2017)) # label seasons
points(28, 100*MLB_HR_rate[16, "HR/FB"], lwd=3)
text(28, 100*MLB_HR_rate[16, "HR/FB"], 
     labels = "July 2015", pos = 4, cex = 0.7)
year.hrfb.avg=NA
for(i in 1:5){
  year=i+2012
  year.hrfb.avg[i]=sum(MLB_HR_rate[which(
    MLB_HR_rate$Year==year), "HR"])/sum(
      MLB_HR_rate[which(MLB_HR_rate$Year==year), "FB"])
}
} # HR/FB
{
fb.ab=MLB_HR_rate$FB/MLB_HR_rate$AB
plot(MLB_HR_rate$Months, 100*fb.ab, type = "p", 
     ylab = "FB% per AB", xlab="Month (April 2013-June 2017)", 
     xaxt="n", main = "Fly Balls per At Bat")
lines(MLB_HR_rate$Months, 100*fb.ab, col="black")
abline(v=c(10, 22, 34, 46), col="blue") # separate seasons
axis(1, at=c(3.5, 15.5, 27.5, 39.5, 50), 
     labels = as.character(2013:2017)) # label seasons
year.fbab.avg=NA
for(i in 1:5){
  year=i+2012
  year.fbab.avg[i]=sum(MLB_HR_rate[which(
    MLB_HR_rate$Year==year), "FB"])/sum(
      MLB_HR_rate[which(MLB_HR_rate$Year==year), "AB"])
  
} # yearly averages
points(c(3.5, 15.5, 27.5, 39.5, 50.5), 100*year.fbab.avg, col="red", lwd=3)
lines(c(3.5, 15.5, 27.5, 39.5, 50.5), 100*year.fbab.avg, col="red")

legend(35, 42.7, c("Monthly Average", "Yearly Average"), lty=NULL, 
      pch=1, cex = 0.6, col = c("black", "red"), lwd = c(1, 3))

} # FB/AB
july.15.before=1:15
july.15.after=17:27
