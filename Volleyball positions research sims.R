n.sims=1000
max.pts=25
n.rotations=6
{
one="1"
two="2"
three="3"
four="4"
five="5"
six="6"
rotations=one
rotations[2]=two
rotations[3]=three
rotations[4]=four
rotations[5]=five
rotations[6]=six
} # position vector
vball.sim.totals=data.frame("Team"=c("A", "B"), "One"=0, "Two"=0, "Three"=0, 
                            "Four"=0, "Five"=0, "Six"=0)

# team A serves first.  Team B serves second
for(i in 1:n.sims){
  a.serve.scores=rnorm(6, mean=0.25, sd=0.25)
  b.serve.scores=rnorm(6, mean=0.25, sd=0.25)
  a.serve.scores=sort(a.serve.scores, decreasing = TRUE)
  b.serve.scores=sort(b.serve.scores, decreasing = TRUE)
  a.serve.probs=pnorm(a.serve.scores)
  b.serve.probs=pnorm(b.serve.scores)
  a.rotation=1
  b.rotation=0
  serving.team="A"
  a.pts=0
  b.pts=0
  
  for(j in 1:60){
    if(b.rotation==0){
      b.column.thing=2
    } else {
      b.column.thing=b.rotation+1
    }
    vball.sim.totals[1, a.rotation+1]=vball.sim.totals[1, a.rotation+1]+1
    vball.sim.totals[2, b.column.thing]=vball.sim.totals[2, b.column.thing]+1
    if(serving.team=="A"){
      serve.prob=a.serve.probs[a.rotation]
    } else if (serving.team=="B"){
      serve.prob=b.serve.probs[b.rotation]
    } # finds the probability of the serving team winning the point
    serve.result=rbinom(1, 1, serve.prob)
    if(serve.result==1 & serving.team=="A"){
      a.pts=a.pts+1
    } else if (serve.result==0 & serving.team=="A"){
      b.pts=b.pts+1
      b.rotation=b.rotation+1
      if(b.rotation==7){
        b.rotation=1
      }
      serving.team="B"
    } else if (serve.result==1 & serving.team=="B"){
      b.pts=b.pts+1
    } else if (serve.result==0 & serving.team=="B"){
      a.pts=a.pts+1
      a.rotation=a.rotation+1
      if(a.rotation==7){
        a.rotation=1
      }
      serving.team="A"
    }
    if(a.pts>24.5 & (a.pts-b.pts)>1.5){
      break
    } else if (b.pts>24.5 & (b.pts-a.pts)>1.5){
      break
    }
  }
}
total.pts=sum(vball.sim.totals[1,2:7])
vball.sim.prob=vball.sim.totals
vball.sim.prob[,2:7]=vball.sim.prob[,2:7]/total.pts
