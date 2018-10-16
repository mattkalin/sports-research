start.time=Sys.time()
n.sims=10000000
# ten million sims 
n.players=5
library(holdem)
{
# handeval # Returns a number based on your best 5-card poker hand
# bid1 # runs a round of preflop bidding
# bid2 # postflop bidding
# calcwin1 # determines who wins the hand
# deal1 # deals cards
} # notes
{
  poker.hands=data.frame("Low"=NA, "High"=NA, "Same.Suit"=NA, "Name"=NA, 
                         "Freq"=NA, "Wins"=NA)
  cards.names=c(2:10, "J", "Q", "K", "A")
  for(card1 in 2:14){
    for(card2 in 2:14){
      if(card1<=card2){
        index=nrow(poker.hands)+1
        if(is.na(poker.hands[1,1])){
          index=1
        }
        poker.hands[index, "Low"]=card1
        poker.hands[index, "High"]=card2
        name1=cards.names[card1-1]
        name2=cards.names[card2-1]
        hand.name=paste(name2, name1, sep = "")
        poker.hands[index, "Name"]=hand.name
        
        # poker.hands[index+91, "Low"]=card1
        # poker.hands[index+91, "High"]=card2
      }
    }
  }
  poker.hands$Same.Suit=FALSE
  # low.cards=append(poker.hands$Low, poker.hands$Low)
  # high.cards=append(poker.hands$High, poker.hands$High)
  # poker.hands$Low=low.cards
  pocket.suits=poker.hands
  pocket.suits$Same.Suit=TRUE
  pocket.suits$Name=paste(pocket.suits$Name, "F")
  poker.hands=rbind(poker.hands, pocket.suits)
  poker.hands$Freq=0
  poker.hands$Wins=0
  poker.hands$Ttl.Val=0
  
  suited.pairs=poker.hands[which(poker.hands$Low==poker.hands$High & 
                                   poker.hands$Same.Suit),]
} # holdem hand dataframe
for(k in 1:n.sims){
deal.result=deal1(numpl = n.players)
hand.vals=NA
boardcards=deal.result$brdnum1
boardsuits=deal.result$brdsuit1
all.handcards=deal.result$plnum1
deal.result$plsuit1
all.handcards
same.suit=NA
for(i in 1:n.players){
  handcards=all.handcards[i,]
  handsuits=deal.result$plsuit1[i,]
  value=handeval(c(boardcards, handcards), c(boardsuits, handsuits))
  hand.vals[i]=value
  # handcards=sort(handcards) (this is already done lmao)
  # all.handcards[i,]=handcards
  same.suit[i]=handsuits[1]==handsuits[2]
  pkr.index=which(poker.hands$Low==handcards[2] &
                    poker.hands$High==handcards[1] & 
                    poker.hands$Same.Suit==same.suit[i])
  poker.hands[pkr.index, "Freq"]=poker.hands[pkr.index, "Freq"]+1
  poker.hands[pkr.index, "Ttl.Val"]=poker.hands[pkr.index, "Ttl.Val"]+value
  # if(handcards[1]==handcards[2] & same.suit[i]){
  #   stdpr.index=handcards[1]-1
  #   suited.pairs[stdpr.index, "Freq"]=suited.pairs[stdpr.index, "Freq"]+1
  #   suited.pairs[stdpr.index, "Ttl.Val"]=
  #     suited.pairs[stdpr.index, "Ttl.Val"]+value
  # }
}
win.index=match(max(hand.vals), hand.vals)
win.nums=all.handcards[win.index,]
win.suits=deal.result$plsuit1[win.index,]
win.same.suit=win.suits[1]==win.suits[2]
pkr.index=which(poker.hands$Low==win.nums[2] &
                  poker.hands$High==win.nums[1] & 
                  poker.hands$Same.Suit==win.same.suit)
poker.hands[pkr.index, "Wins"]=poker.hands[pkr.index, "Wins"]+1
if(win.nums[1]==win.nums[2] & win.same.suit){
  x.handcards=all.handcards
  x.handsuits=deal.result$plsuit1
  x.win.index=win.index
}

# deal.result
# hand.vals
# win.index
} # simulation
CalcPokerWpct=function(your.hand, opp.hand, you.flush=FALSE, 
                       opp.flush=FALSE){
  if(length(your.hand)!=2 | length(opp.hand)!=2){
    stop("Hands must contain two numbers")
  }
  your.hand=sort(your.hand)
  opp.hand=sort(opp.hand)
  your.index=which(poker.hands$Low==your.hand[1] & 
                     poker.hands$High==your.hand[2] & 
                     poker.hands$Same.Suit==you.flush)
  your.wpct=poker.hands[your.index, "Wpct"]
  opp.index=which(poker.hands$Low==opp.hand[1] & 
                    poker.hands$High==opp.hand[2] & 
                    poker.hands$Same.Suit==opp.flush)
  opp.wpct=poker.hands[opp.index, "Wpct"]
  total.wpct=(your.wpct-your.wpct*opp.wpct)/
    (your.wpct+opp.wpct-2*your.wpct*opp.wpct)
  return(total.wpct)
}
{
impossible.hands=which(poker.hands$Low==poker.hands$High & 
                         poker.hands$Same.Suit)
poker.hands=poker.hands[-c(impossible.hands),]
poker.hands$Wpct=poker.hands$Wins/poker.hands$Freq
poker.hands$Avg.Val=poker.hands$Ttl.Val/poker.hands$Freq
plot(poker.hands$Avg.Val, poker.hands$Wpct)
plot(poker.hands$High, poker.hands$Wpct)

pocket.pair=(poker.hands$Low==poker.hands$High)*1
low.card=poker.hands$Low
high.card=poker.hands$High
suit.pair=poker.hands$Same.Suit*1
win.pct=poker.hands$Wpct
poker.model=lm(win.pct~low.card+high.card+pocket.pair+suit.pair, 
                data = poker.hands)

} # analysis

poker.hands$Model=fitted(poker.model)
poker.hands$Resid=resid(poker.model)
poker.hands$Ttl.Val=NULL
poker.hands$Model = NULL
poker.hands$Resid = NULL
poker.hands$Return = poker.hands$Wpct * 5 - 1

end.time=Sys.time()
time.diff=end.time-start.time
time.diff
beepr::beep()

# 1000 sims with 6 players takes 4 secs
# 10000 sims with 2 players takes 18 secs
# 10 million sims with 2 players takes 5 hrs

