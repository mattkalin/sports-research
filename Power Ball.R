Power_Ball_Data <- read_excel("~/Desktop/Summer 2017/PowerBall.xlsx", 
  col_types = c("text","text","text","text","numeric","numeric","numeric"))
Power_Ball_Data$Base=as.numeric(Power_Ball_Data$Base)
{
money.model=glm(Power_Ball_Data$Winner~Power_Ball_Data$`Real amount`, family = 
                  binomial(link = 'logit'), data = Power_Ball_Data)
drawnum.model=glm(Power_Ball_Data$Winner~Power_Ball_Data$`th drawing`,family =
                    binomial(link = 'logit'), data = Power_Ball_Data)
money.line=predict(money.model, newdata = list(seq(from=
  min(Power_Ball_Data$`Real amount`),to=max(Power_Ball_Data$`Real amount`), 
  length.out = 1000)))
sorted.jackpot=sort.default(Power_Ball_Data$`Real amount`)
sorted.fit.money=sort.default(fitted(money.model))
sorted.fit.drawnum=sort.default(fitted(drawnum.model))
sorted.drawing=sort.default(Power_Ball_Data$`th drawing`)
inputs=seq(from=min(sorted.drawing),to=max(sorted.drawing),
                  length.out = length(Power_Ball_Data$`th drawing`))
drawnum.predictions=predict(drawnum.model, 
                            newdata = Power_Ball_Data, type = "response")
} # model
power_ball_probabilities=data.frame("Drawing"=
    1:max(Power_Ball_Data$`th drawing`), "Model Prob"=NA, "Occur Freq"=NA, 
    "Winners"=NA, "Win|Occur"=NA, "Occur|Win"=NA)
total.wins=sum(Power_Ball_Data$Winner)
for(i in 1:max(Power_Ball_Data$`th drawing`)){
  draw.num=i
  index=which(Power_Ball_Data$`th drawing`==draw.num)[1]
  model=drawnum.predictions[index]
  occur.freq=sum(Power_Ball_Data$`th drawing`==draw.num)
  win.freq=sum((Power_Ball_Data[
    which(Power_Ball_Data$`th drawing`==draw.num), "Winner"]))
  power_ball_probabilities[i, "Model.Prob"]=model
  power_ball_probabilities[i, "Occur.Freq"]=occur.freq
  power_ball_probabilities[i, "Winners"]=win.freq
  power_ball_probabilities[i, "Win|Occur"]=win.freq/occur.freq
  power_ball_probabilities[i, "Occur|Win"]=win.freq/total.wins
}
power_ball_probabilities$Win.Occur=NULL
power_ball_probabilities$Occur.Win=NULL

win.total.model=lm(power_ball_probabilities$Winners ~ as.double(1:24), 
                   data = power_ball_probabilities)
win.total.fit=predict(win.total.model, 
                      newdata=list(x=seq(0,24,length.out=1000)))

plot(1:24, power_ball_probabilities$`Win|Occur`, 
     xlab = "Drawings Since Last Winner", ylab = "Winner Probability")
lines(1:24, power_ball_probabilities$Model.Prob, col="red")

plot(1:24, power_ball_probabilities$Winners,xlab="Drawings Since Last Winner", 
    ylab = "Total Number of Winners", main = "Total Winners for Each Drawing")
lines(1:24, win.total.fit, col="red")

plot(Power_Ball_Data$`th drawing`, Power_Ball_Data$`Real amount`, 
     xlab = "Drawings Since Last Winner", ylab = "Jackpot Amount")
