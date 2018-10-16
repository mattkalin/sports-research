timed=TRUE
start.time=Sys.time()
{
{
{
hitters_post1980 <- read.csv("~/Desktop/Moneyball/hitters_post1980.csv", 
                             stringsAsFactors=FALSE)
qual.hitters=hitters_post1980[which(hitters_post1980$AB>=200),]
hit.prv.indeces=data.frame("Name"=qual.hitters$name, "Year"=qual.hitters$year)
hit.prv.indeces[,3:17]=NA
names(hit.prv.indeces)[3:17]=paste("Yr x-", 1:15, sep = "")
n.qual=length(qual.hitters$name)
for (i in 1:n.qual) {
  name=qual.hitters[i, "name"]
  year=qual.hitters[i, "year"]
  for (j in 1:15) {
    index=which(qual.hitters$name==name & 
                  qual.hitters$year==year-j)
    if(length(index)==1){
      hit.prv.indeces[i, j+2]=index
    }
  }
}
rate.stats=data.frame("Name"=qual.hitters$name, "Year"=qual.hitters$year)
rate.stats$AVG=qual.hitters$H/qual.hitters$AB
rate.stats$OBP=(qual.hitters$H+qual.hitters$BB+qual.hitters$HBP)/(qual.hitters$AB+
                qual.hitters$BB+qual.hitters$HBP+qual.hitters$SF)
rate.stats$SLG=(qual.hitters$H+qual.hitters$X2B+2*qual.hitters$X3B+
                  3*qual.hitters$HR)/(qual.hitters$AB)
rate.stats$OPS=rate.stats$OBP+rate.stats$SLG
rate.stats$runs.created=rate.stats$OBP*rate.stats$SLG
rate.stats$FIOBP=(0.7*qual.hitters$HR+0.3*(qual.hitters$AB-qual.hitters$SO+
                    qual.hitters$SF)+qual.hitters$BB+qual.hitters$HBP)/
  (qual.hitters$AB+qual.hitters$BB+qual.hitters$HBP+qual.hitters$SF)
rate.stats$TBPH=rate.stats$SLG/rate.stats$AVG
rate.stats$KRC=4114*rate.stats$FIOBP^1.2258*rate.stats$TBPH^0.3617-507.5
rate.stats$ISO=rate.stats$SLG-rate.stats$AVG

prv.template=data.frame("Name"=qual.hitters$name, "Year"=qual.hitters$year)
prv.template[,"Yr x"]=NA
prv.template[,4:18]=NA
prv.template$Age=qual.hitters$age
names(prv.template)[4:18]=paste("Yr x-", 1:15, sep = "")
} # setup
{
prv.avg=prv.template
prv.avg$`Yr x`=rate.stats$AVG
for (i in 1:n.qual) {
  for (j in 4:18) {
    index=as.numeric(hit.prv.indeces[i, j-1])
    if(is.na(index)==FALSE)
    prv.avg[i, j]=rate.stats[index, "AVG"]
  }
}

{
  avg.yr1=prv.avg[which(is.na(prv.avg$`Yr x-1`)==FALSE),]
  plot(avg.yr1$`Yr x-1`, avg.yr1$`Yr x`) # cor of 0.409
  avg.yr2=prv.avg[which(is.na(prv.avg$`Yr x-1`)==FALSE & 
                           is.na(prv.avg$`Yr x-2`)==FALSE),]
  avg.avg.2=rowMeans(cbind(avg.yr2$`Yr x-1`, avg.yr2$`Yr x-2`))
  plot(avg.avg.2, avg.yr2$`Yr x`) # cor of 0.479
  avg.yr3=prv.avg[which(is.na(prv.avg$`Yr x-1`)==FALSE & 
                          is.na(prv.avg$`Yr x-2`)==FALSE & 
                    is.na(prv.avg$`Yr x-3`)==FALSE),]
  avg.avg.3=rowMeans(cbind(avg.yr3$`Yr x-1`, avg.yr3$`Yr x-2`, 
                           avg.yr3$`Yr x-3`))
  plot(avg.avg.3, avg.yr3$`Yr x`) # cor of 0.513
  avg.yr4=prv.avg[which(is.na(prv.avg$`Yr x-1`)==FALSE & 
                          is.na(prv.avg$`Yr x-2`)==FALSE & 
                          is.na(prv.avg$`Yr x-4`)==FALSE & 
                          is.na(prv.avg$`Yr x-3`)==FALSE),]
  avg.avg.4=rowMeans(cbind(avg.yr4$`Yr x-1`, avg.yr4$`Yr x-2`, 
                           avg.yr4$`Yr x-3`, avg.yr4$`Yr x-4`))
  plot(avg.avg.4, avg.yr4$`Yr x`) # cor of 0.527
  avg4.model=lm(avg.yr4$`Yr x`~avg.yr4$`Yr x-1`+avg.yr4$`Yr x-2`+ 
               avg.yr4$`Yr x-3`+ avg.yr4$`Yr x-4`, data = avg.yr4)
  avg4.preds=fitted(avg4.model)
  plot(avg4.preds, avg.yr4$`Yr x`) # cor of .534
}
} # batting avg cor of .534
{
  prv.obp=prv.template
  prv.obp$`Yr x`=rate.stats$OBP
  for (i in 1:n.qual) {
    for (j in 4:18) {
      index=as.numeric(hit.prv.indeces[i, j-1])
      if(is.na(index)==FALSE)
        prv.obp[i, j]=rate.stats[index, "OBP"]
    }
  }
  
  {
    obp.yr1=prv.obp[which(is.na(prv.obp$`Yr x-1`)==FALSE),]
    plot(obp.yr1$`Yr x-1`, obp.yr1$`Yr x`) # cor of 0.589
    obp.yr2=prv.obp[which(is.na(prv.obp$`Yr x-1`)==FALSE & 
                            is.na(prv.obp$`Yr x-2`)==FALSE),]
    obp.obp.2=rowMeans(cbind(obp.yr2$`Yr x-1`, obp.yr2$`Yr x-2`))
    plot(obp.obp.2, obp.yr2$`Yr x`) # cor of 0.643
    obp.yr3=prv.obp[which(is.na(prv.obp$`Yr x-1`)==FALSE & 
                            is.na(prv.obp$`Yr x-2`)==FALSE & 
                            is.na(prv.obp$`Yr x-3`)==FALSE),]
    obp.obp.3=rowMeans(cbind(obp.yr3$`Yr x-1`, obp.yr3$`Yr x-2`, 
                             obp.yr3$`Yr x-3`))
    plot(obp.obp.3, obp.yr3$`Yr x`) # cor of 0.663
    obp.yr4=prv.obp[which(is.na(prv.obp$`Yr x-1`)==FALSE & 
                            is.na(prv.obp$`Yr x-2`)==FALSE & 
                            is.na(prv.obp$`Yr x-4`)==FALSE & 
                            is.na(prv.obp$`Yr x-3`)==FALSE),]
    obp.obp.4=rowMeans(cbind(obp.yr4$`Yr x-1`, obp.yr4$`Yr x-2`, 
                             obp.yr4$`Yr x-3`, obp.yr4$`Yr x-4`))
    plot(obp.obp.4, obp.yr4$`Yr x`) # cor of 0.676
    obp4.model=lm(obp.yr4$`Yr x`~obp.yr4$`Yr x-1`+obp.yr4$`Yr x-2`+ 
                    obp.yr4$`Yr x-3`+ obp.yr4$`Yr x-4`, data = obp.yr4)
    obp4.preds=fitted(obp4.model)
    plot(obp4.preds, obp.yr4$`Yr x`) # cor of 0.687
    range.x=((1:1000)-min(obp4.preds)/(max(obp4.preds)-min(obp4.preds)))
    lines(range.x, range.x, col="red")
    obp.yr4$pred4=obp4.preds
  }
} # on base pct cor of .687
{
  prv.slg=prv.template
  prv.slg$`Yr x`=rate.stats$SLG
  for (i in 1:n.qual) {
    for (j in 4:18) {
      index=as.numeric(hit.prv.indeces[i, j-1])
      if(is.na(index)==FALSE)
        prv.slg[i, j]=rate.stats[index, "SLG"]
    }
  }
  
  {
    slg.yr1=prv.slg[which(is.na(prv.slg$`Yr x-1`)==FALSE),]
    plot(slg.yr1$`Yr x-1`, slg.yr1$`Yr x`) # cor of 0.632
    slg.yr2=prv.slg[which(is.na(prv.slg$`Yr x-1`)==FALSE & 
                            is.na(prv.slg$`Yr x-2`)==FALSE),]
    slg.slg.2=rowMeans(cbind(slg.yr2$`Yr x-1`, slg.yr2$`Yr x-2`))
    plot(slg.slg.2, slg.yr2$`Yr x`) # cor of 0.680
    slg.yr3=prv.slg[which(is.na(prv.slg$`Yr x-1`)==FALSE & 
                            is.na(prv.slg$`Yr x-2`)==FALSE & 
                            is.na(prv.slg$`Yr x-3`)==FALSE),]
    slg.slg.3=rowMeans(cbind(slg.yr3$`Yr x-1`, slg.yr3$`Yr x-2`, 
                             slg.yr3$`Yr x-3`))
    plot(slg.slg.3, slg.yr3$`Yr x`) # cor of 0.693
    slg.yr4=prv.slg[which(is.na(prv.slg$`Yr x-1`)==FALSE & 
                            is.na(prv.slg$`Yr x-2`)==FALSE & 
                            is.na(prv.slg$`Yr x-4`)==FALSE & 
                            is.na(prv.slg$`Yr x-3`)==FALSE),]
    slg.slg.4=rowMeans(cbind(slg.yr4$`Yr x-1`, slg.yr4$`Yr x-2`, 
                             slg.yr4$`Yr x-3`, slg.yr4$`Yr x-4`))
    plot(slg.slg.4, slg.yr4$`Yr x`) # cor of 0.695
    slg4.model=lm(slg.yr4$`Yr x`~slg.yr4$`Yr x-1`+slg.yr4$`Yr x-2`+ 
                    slg.yr4$`Yr x-3`+ slg.yr4$`Yr x-4`, data = slg.yr4)
    slg4.preds=fitted(slg4.model)
    plot(slg4.preds, slg.yr4$`Yr x`) # cor of 0.710
    range.slg=seq(from=min(slg4.preds)-IQR(slg4.preds), 
                  to=max(slg4.preds)+IQR(slg4.preds), length.out = 1000)
    lines(range.slg, range.slg, col="red")
    slg.yr4$pred4=slg4.preds
  }
} # slugging pct cor of .710
{
  prv.ops=prv.template
  prv.ops$`Yr x`=rate.stats$OPS
  for (i in 1:n.qual) {
    for (j in 4:18) {
      index=as.numeric(hit.prv.indeces[i, j-1])
      if(is.na(index)==FALSE)
        prv.ops[i, j]=rate.stats[index, "OPS"]
    }
  }
  
  {
    ops.yr1=prv.ops[which(is.na(prv.ops$`Yr x-1`)==FALSE),]
    plot(ops.yr1$`Yr x-1`, ops.yr1$`Yr x`) # cor of 0.6114
    ops.yr2=prv.ops[which(is.na(prv.ops$`Yr x-1`)==FALSE & 
                            is.na(prv.ops$`Yr x-2`)==FALSE),]
    ops.ops.2=rowMeans(cbind(ops.yr2$`Yr x-1`, ops.yr2$`Yr x-2`))
    plot(ops.ops.2, ops.yr2$`Yr x`) # cor of 0.661
    ops.yr3=prv.ops[which(is.na(prv.ops$`Yr x-1`)==FALSE & 
                            is.na(prv.ops$`Yr x-2`)==FALSE & 
                            is.na(prv.ops$`Yr x-3`)==FALSE),]
    ops.ops.3=rowMeans(cbind(ops.yr3$`Yr x-1`, ops.yr3$`Yr x-2`, 
                             ops.yr3$`Yr x-3`))
    plot(ops.ops.3, ops.yr3$`Yr x`) # cor of 0.677
    ops.yr4=prv.ops[which(is.na(prv.ops$`Yr x-1`)==FALSE & 
                            is.na(prv.ops$`Yr x-2`)==FALSE & 
                            is.na(prv.ops$`Yr x-4`)==FALSE & 
                            is.na(prv.ops$`Yr x-3`)==FALSE),]
    ops.ops.4=rowMeans(cbind(ops.yr4$`Yr x-1`, ops.yr4$`Yr x-2`, 
                             ops.yr4$`Yr x-3`, ops.yr4$`Yr x-4`))
    plot(ops.ops.4, ops.yr4$`Yr x`) # cor of 0.682
    ops4.model=lm(ops.yr4$`Yr x`~ops.yr4$`Yr x-1`+ops.yr4$`Yr x-2`+ 
                    ops.yr4$`Yr x-3`+ ops.yr4$`Yr x-4`, data = ops.yr4)
    ops4.preds=fitted(ops4.model)
    plot(ops4.preds, ops.yr4$`Yr x`) # cor of 0.698
    range.ops=seq(from=min(ops4.preds)-IQR(ops4.preds), 
                  to=max(ops4.preds)+IQR(ops4.preds), length.out = 1000)
    lines(range.ops, range.ops, col="red")
    ops.yr4$pred4=ops4.preds
  }
} # ops cor of .698
{
  prv.rnc=prv.template
  prv.rnc$`Yr x`=rate.stats$runs.created
  for (i in 1:n.qual) {
    for (j in 4:18) {
      index=as.numeric(hit.prv.indeces[i, j-1])
      if(is.na(index)==FALSE)
        prv.rnc[i, j]=rate.stats[index, "runs.created"]
    }
  }
  
  {
    rnc.yr1=prv.rnc[which(is.na(prv.rnc$`Yr x-1`)==FALSE),]
    plot(rnc.yr1$`Yr x-1`, rnc.yr1$`Yr x`) # cor of 0.626
    rnc.yr2=prv.rnc[which(is.na(prv.rnc$`Yr x-1`)==FALSE & 
                            is.na(prv.rnc$`Yr x-2`)==FALSE),]
    rnc.rnc.2=rowMeans(cbind(rnc.yr2$`Yr x-1`, rnc.yr2$`Yr x-2`))
    plot(rnc.rnc.2, rnc.yr2$`Yr x`) # cor of 0.674
    rnc.yr3=prv.rnc[which(is.na(prv.rnc$`Yr x-1`)==FALSE & 
                            is.na(prv.rnc$`Yr x-2`)==FALSE & 
                            is.na(prv.rnc$`Yr x-3`)==FALSE),]
    rnc.rnc.3=rowMeans(cbind(rnc.yr3$`Yr x-1`, rnc.yr3$`Yr x-2`, 
                             rnc.yr3$`Yr x-3`))
    plot(rnc.rnc.3, rnc.yr3$`Yr x`) # cor of 0.688
    rnc.yr4=prv.rnc[which(is.na(prv.rnc$`Yr x-1`)==FALSE & 
                            is.na(prv.rnc$`Yr x-2`)==FALSE & 
                            is.na(prv.rnc$`Yr x-4`)==FALSE & 
                            is.na(prv.rnc$`Yr x-3`)==FALSE),]
    rnc.rnc.4=rowMeans(cbind(rnc.yr4$`Yr x-1`, rnc.yr4$`Yr x-2`, 
                             rnc.yr4$`Yr x-3`, rnc.yr4$`Yr x-4`))
    plot(rnc.rnc.4, rnc.yr4$`Yr x`) # cor of 0.693
    rnc4.model=lm(rnc.yr4$`Yr x`~rnc.yr4$`Yr x-1`+rnc.yr4$`Yr x-2`+ 
                    rnc.yr4$`Yr x-3`+ rnc.yr4$`Yr x-4`, data = rnc.yr4)
    rnc4.preds=fitted(rnc4.model)
    plot(rnc4.preds, rnc.yr4$`Yr x`) # cor of 0.710
    range.rnc=seq(from=min(rnc4.preds)-IQR(rnc4.preds), 
                  to=max(rnc4.preds)+IQR(rnc4.preds), length.out = 1000)
    lines(range.rnc, range.rnc, col="red")
    rnc.yr4$pred4=rnc4.preds
  }
} # rnc cor of .710
{
    prv.iso=prv.template
    prv.iso$`Yr x`=rate.stats$ISO
    for (i in 1:n.qual) {
      for (j in 4:18) {
        index=as.numeric(hit.prv.indeces[i, j-1])
        if(is.na(index)==FALSE)
          prv.iso[i, j]=rate.stats[index, "ISO"]
      }
    }
    
    {
      iso.yr1=prv.iso[which(is.na(prv.iso$`Yr x-1`)==FALSE),]
      plot(iso.yr1$`Yr x-1`, iso.yr1$`Yr x`) # cor of 0.739
      iso.yr2=prv.iso[which(is.na(prv.iso$`Yr x-1`)==FALSE & 
                              is.na(prv.iso$`Yr x-2`)==FALSE),]
      iso.iso.2=rowMeans(cbind(iso.yr2$`Yr x-1`, iso.yr2$`Yr x-2`))
      plot(iso.iso.2, iso.yr2$`Yr x`) # cor of 0.773
      iso.yr3=prv.iso[which(is.na(prv.iso$`Yr x-1`)==FALSE & 
                              is.na(prv.iso$`Yr x-2`)==FALSE & 
                              is.na(prv.iso$`Yr x-3`)==FALSE),]
      iso.iso.3=rowMeans(cbind(iso.yr3$`Yr x-1`, iso.yr3$`Yr x-2`, 
                               iso.yr3$`Yr x-3`))
      plot(iso.iso.3, iso.yr3$`Yr x`) # cor of 0.780
      iso.yr4=prv.iso[which(is.na(prv.iso$`Yr x-1`)==FALSE & 
                              is.na(prv.iso$`Yr x-2`)==FALSE & 
                              is.na(prv.iso$`Yr x-4`)==FALSE & 
                              is.na(prv.iso$`Yr x-3`)==FALSE),]
      iso.iso.4=rowMeans(cbind(iso.yr4$`Yr x-1`, iso.yr4$`Yr x-2`, 
                               iso.yr4$`Yr x-3`, iso.yr4$`Yr x-4`))
      plot(iso.iso.4, iso.yr4$`Yr x`) # cor of 0.779
      iso4.model=lm(iso.yr4$`Yr x`~iso.yr4$`Yr x-1`+iso.yr4$`Yr x-2`+ 
                      iso.yr4$`Yr x-3`+ iso.yr4$`Yr x-4`, data = iso.yr4)
      iso4.preds=fitted(iso4.model)
      plot(iso4.preds, iso.yr4$`Yr x`) # cor of 0.792
      range.iso=seq(from=min(iso4.preds)-IQR(iso4.preds), 
                    to=max(iso4.preds)+IQR(iso4.preds), length.out = 1000)
      lines(range.iso, range.iso, col="red")
      iso.yr4$pred4=iso4.preds
    }
  } # iso cor of .792
{
  prv.fiobp=prv.template
  prv.fiobp$`Yr x`=rate.stats$FIOBP
  for (i in 1:n.qual) {
    for (j in 4:18) {
      index=as.numeric(hit.prv.indeces[i, j-1])
      if(is.na(index)==FALSE)
        prv.fiobp[i, j]=rate.stats[index, "FIOBP"]
    }
  }
  
  {
    fiobp.yr1=prv.fiobp[which(is.na(prv.fiobp$`Yr x-1`)==FALSE),]
    plot(fiobp.yr1$`Yr x-1`, fiobp.yr1$`Yr x`) # cor of 0.740
    fiobp.yr2=prv.fiobp[which(is.na(prv.fiobp$`Yr x-1`)==FALSE & 
                            is.na(prv.fiobp$`Yr x-2`)==FALSE),]
    fiobp.fiobp.2=rowMeans(cbind(fiobp.yr2$`Yr x-1`, fiobp.yr2$`Yr x-2`))
    plot(fiobp.fiobp.2, fiobp.yr2$`Yr x`) # cor of 0.767
    fiobp.yr3=prv.fiobp[which(is.na(prv.fiobp$`Yr x-1`)==FALSE & 
                            is.na(prv.fiobp$`Yr x-2`)==FALSE & 
                            is.na(prv.fiobp$`Yr x-3`)==FALSE),]
    fiobp.fiobp.3=rowMeans(cbind(fiobp.yr3$`Yr x-1`, fiobp.yr3$`Yr x-2`, 
                             fiobp.yr3$`Yr x-3`))
    plot(fiobp.fiobp.3, fiobp.yr3$`Yr x`) # cor of 0.779
    fiobp.yr4=prv.fiobp[which(is.na(prv.fiobp$`Yr x-1`)==FALSE & 
                            is.na(prv.fiobp$`Yr x-2`)==FALSE & 
                            is.na(prv.fiobp$`Yr x-4`)==FALSE & 
                            is.na(prv.fiobp$`Yr x-3`)==FALSE),]
    fiobp.fiobp.4=rowMeans(cbind(fiobp.yr4$`Yr x-1`, fiobp.yr4$`Yr x-2`, 
                             fiobp.yr4$`Yr x-3`, fiobp.yr4$`Yr x-4`))
    plot(fiobp.fiobp.4, fiobp.yr4$`Yr x`) # cor of 0.783
    fiobp4.model=lm(fiobp.yr4$`Yr x`~fiobp.yr4$`Yr x-1`+fiobp.yr4$`Yr x-2`+ 
                    fiobp.yr4$`Yr x-3`+ fiobp.yr4$`Yr x-4`, data = fiobp.yr4)
    fiobp4.model.age=lm(fiobp.yr4$`Yr x`~fiobp.yr4$`Yr x-1`+
                          fiobp.yr4$`Yr x-2`+ fiobp.yr4$`Yr x-3`+ 
                          fiobp.yr4$`Yr x-4`+fiobp.yr4$Age, data = fiobp.yr4)
    fiobp4.preds=fitted(fiobp4.model)
    plot(fiobp4.preds, fiobp.yr4$`Yr x`) # cor of 0.795
    range.fiobp=seq(from=min(fiobp4.preds)-IQR(fiobp4.preds), 
                  to=max(fiobp4.preds)+IQR(fiobp4.preds), length.out = 1000)
    lines(range.fiobp, range.fiobp, col="red")
    fiobp.yr4$pred4=fiobp4.preds
    fiobp.yr4$resid=fiobp.yr4$`Yr x`-fiobp.yr4$pred4
  }
} # fiobp cor of .795 (fielding independent on base percentage)
{
  prv.tbph=prv.template
  prv.tbph$`Yr x`=rate.stats$TBPH
  for (i in 1:n.qual) {
    for (j in 4:18) {
      index=as.numeric(hit.prv.indeces[i, j-1])
      if(is.na(index)==FALSE)
        prv.tbph[i, j]=rate.stats[index, "TBPH"]
    }
  }
  
  {
    tbph.yr1=prv.tbph[which(is.na(prv.tbph$`Yr x-1`)==FALSE),]
    plot(tbph.yr1$`Yr x-1`, tbph.yr1$`Yr x`) # cor of 0.764
    tbph.yr2=prv.tbph[which(is.na(prv.tbph$`Yr x-1`)==FALSE & 
                            is.na(prv.tbph$`Yr x-2`)==FALSE),]
    tbph.tbph.2=rowMeans(cbind(tbph.yr2$`Yr x-1`, tbph.yr2$`Yr x-2`))
    plot(tbph.tbph.2, tbph.yr2$`Yr x`) # cor of 0.799
    tbph.yr3=prv.tbph[which(is.na(prv.tbph$`Yr x-1`)==FALSE & 
                            is.na(prv.tbph$`Yr x-2`)==FALSE & 
                            is.na(prv.tbph$`Yr x-3`)==FALSE),]
    tbph.tbph.3=rowMeans(cbind(tbph.yr3$`Yr x-1`, tbph.yr3$`Yr x-2`, 
                             tbph.yr3$`Yr x-3`))
    plot(tbph.tbph.3, tbph.yr3$`Yr x`) # cor of 0.810
    tbph.yr4=prv.tbph[which(is.na(prv.tbph$`Yr x-1`)==FALSE & 
                            is.na(prv.tbph$`Yr x-2`)==FALSE & 
                            is.na(prv.tbph$`Yr x-4`)==FALSE & 
                            is.na(prv.tbph$`Yr x-3`)==FALSE),]
    tbph.tbph.4=rowMeans(cbind(tbph.yr4$`Yr x-1`, tbph.yr4$`Yr x-2`, 
                             tbph.yr4$`Yr x-3`, tbph.yr4$`Yr x-4`))
    plot(tbph.tbph.4, tbph.yr4$`Yr x`) # cor of 0.809
    tbph4.model=lm(tbph.yr4$`Yr x`~tbph.yr4$`Yr x-1`+tbph.yr4$`Yr x-2`+ 
                    tbph.yr4$`Yr x-3`+ tbph.yr4$`Yr x-4`, data = tbph.yr4)
    tbph4.model.age=lm(tbph.yr4$`Yr x`~tbph.yr4$`Yr x-1`+tbph.yr4$`Yr x-2`+ 
                     tbph.yr4$`Yr x-3`+ tbph.yr4$`Yr x-4`+tbph.yr4$Age,
                     data = tbph.yr4)
    tbph4.preds=fitted(tbph4.model)
    plot(tbph4.preds, tbph.yr4$`Yr x`) # cor of 0.818
    range.tbph=seq(from=min(tbph4.preds)-IQR(tbph4.preds), 
                  to=max(tbph4.preds)+IQR(tbph4.preds), length.out = 1000)
    lines(range.tbph, range.tbph, col="red")
    tbph.yr4$pred4=tbph4.preds
    tbph.yr4$pred4=tbph4.preds
    tbph.yr4$resid=tbph.yr4$`Yr x`-tbph.yr4$pred4
  }
} # tbph cor of .818 (total bases per hit)
{
  prv.krc=prv.template
  prv.krc$`Yr x`=rate.stats$KRC
  for (i in 1:n.qual) {
    for (j in 4:18) {
      index=as.numeric(hit.prv.indeces[i, j-1])
      if(is.na(index)==FALSE)
        prv.krc[i, j]=rate.stats[index, "KRC"]
    }
  }
  
  {
    krc.yr1=prv.krc[which(is.na(prv.krc$`Yr x-1`)==FALSE),]
    plot(krc.yr1$`Yr x-1`, krc.yr1$`Yr x`) # cor of 0.758
    krc.yr2=prv.krc[which(is.na(prv.krc$`Yr x-1`)==FALSE & 
                            is.na(prv.krc$`Yr x-2`)==FALSE),]
    krc.krc.2=rowMeans(cbind(krc.yr2$`Yr x-1`, krc.yr2$`Yr x-2`))
    plot(krc.krc.2, krc.yr2$`Yr x`) # cor of 0.785
    krc.yr3=prv.krc[which(is.na(prv.krc$`Yr x-1`)==FALSE & 
                            is.na(prv.krc$`Yr x-2`)==FALSE & 
                            is.na(prv.krc$`Yr x-3`)==FALSE),]
    krc.krc.3=rowMeans(cbind(krc.yr3$`Yr x-1`, krc.yr3$`Yr x-2`, 
                             krc.yr3$`Yr x-3`))
    plot(krc.krc.3, krc.yr3$`Yr x`) # cor of 0.796
    krc.yr4=prv.krc[which(is.na(prv.krc$`Yr x-1`)==FALSE & 
                            is.na(prv.krc$`Yr x-2`)==FALSE & 
                            is.na(prv.krc$`Yr x-4`)==FALSE & 
                            is.na(prv.krc$`Yr x-3`)==FALSE),]
    krc.krc.4=rowMeans(cbind(krc.yr4$`Yr x-1`, krc.yr4$`Yr x-2`, 
                             krc.yr4$`Yr x-3`, krc.yr4$`Yr x-4`))
    plot(krc.krc.4, krc.yr4$`Yr x`) # cor of 0.798
    krc4.model=lm(krc.yr4$`Yr x`~krc.yr4$`Yr x-1`+krc.yr4$`Yr x-2`+ 
                    krc.yr4$`Yr x-3`+ krc.yr4$`Yr x-4`, data = krc.yr4)
    krc4.model.age=lm(krc.yr4$`Yr x`~krc.yr4$`Yr x-1`+krc.yr4$`Yr x-2`+ 
                    krc.yr4$`Yr x-3`+ krc.yr4$`Yr x-4`+krc.yr4$Age,
                    data = krc.yr4)
    krc4.preds=fitted(krc4.model)
    plot(krc4.preds, krc.yr4$`Yr x`) # cor of 0.811
    range.krc=seq(from=min(krc4.preds)-IQR(krc4.preds), 
                  to=max(krc4.preds)+IQR(krc4.preds), length.out = 1000)
    lines(range.krc, range.krc, col="red")
    krc.yr4$pred4=krc4.preds
    krc.yr4$pred4=krc4.preds
    krc.yr4$resid=krc.yr4$`Yr x`-krc.yr4$pred4
  }
} # krc cor of .811
} # individual hitting stats
{
{
team.hitting.data <- read.csv(
  "~/Desktop/Miscellaneous/Baseball/Team hitting data.csv", 
  stringsAsFactors=FALSE)
team.stats=data.frame("Year"=team.hitting.data$Year,"Team"=team.hitting.data$Team)
team.stats$Runs=team.hitting.data$Runs
team.stats$FIOBP=(0.7*team.hitting.data$HR+0.3*(team.hitting.data$AB-
  team.hitting.data$SO+team.hitting.data$SF)+team.hitting.data$BB+
  team.hitting.data$HBP)/(team.hitting.data$AB+team.hitting.data$BB+
                            team.hitting.data$HBP+team.hitting.data$SF)
team.stats$TBPH=team.hitting.data$SLG/team.hitting.data$AVG

} # setup
#krc.formula=lm(team.stats$Runs~team.stats$prod)
#krc.formula=
  #lm(team.stats$Runs~team.stats$FIOBP+team.stats$TBPH,data = team.stats)
krc.formula=nls(team.stats$Runs~(c*team.stats$FIOBP^a*team.stats$TBPH^b+d), 
      data=team.stats, start=c(a=2, b=2, c=600, d=450))
cor(fitted(krc.formula), team.stats$Runs)
krc.formula
team.stats$model=fitted(krc.formula)
plot(team.stats$model, team.stats$Runs)
range.runs=seq(from=min(team.stats$Runs)-IQR(team.stats$Runs), 
      to=max(team.stats$Runs)+IQR(team.stats$Runs), length.out = 1000)
lines(range.runs, range.runs, col="red")
} # team hitting stats
# i need AB, HR, SO/K, SF, BB, HBP, AVG, SLG (or H and TB) for KRC
{
Batting <- read.csv("~/Downloads/baseballdatabank-master/core/Batting.csv", 
                    stringsAsFactors=FALSE)
hit.post.2010=Batting[which(Batting$yearID>2010 & Batting$AB>100),]
Master <- read.csv("~/Downloads/baseballdatabank-master/core/Master.csv", 
                   stringsAsFactors=FALSE)

Master$Name=paste(Master$nameFirst, Master$nameLast)
hit.post.2010$Name=NA
hit.post.2010$Age.17=NA
for (i in 1:length(hit.post.2010$playerID)) {
  id.hit=hit.post.2010[i, "playerID"]
  mas.ind=which(Master$playerID==id.hit)
  j=mas.ind
    id.master=Master[j, "playerID"]
      name=Master[j, "Name"]
      hit.post.2010[i, "Name"]=name
      age.17=2017-Master[j, "birthYear"]
      hit.post.2010[i, "Age.17"]=age.17
} # fills in full names and ages
hit.proj.2016 <- 
  read.csv("~/Desktop/Miscellaneous/Baseball/hitting projections 2016.csv", 
           stringsAsFactors=FALSE)
hit.proj.2016$team.id=NA
names.15=hit.post.2010[which(hit.post.2010$yearID==2015), "Name"]
names.15=c(names.15, setdiff(hit.proj.2016$Name, names.15))
hit.indeces=data.frame("Name"=names.15, "2015"=NA, "2014"=NA, "2013"=NA, 
                       "2012"=NA, "2011"=NA)
for (i in 1:length(names.15)) {
  name.1=as.character(hit.indeces[i, "Name"])
  for (j in 2:6) {
    year.1=2017-j
    index=which(name.1==hit.post.2010[, "Name"] & 
                  year.1==hit.post.2010[, "yearID"])
    if(length(index)==1){
      hit.indeces[i, j]=index
    }
  }
} # indeces
hit.template=data.frame("Name"=hit.indeces$Name, "2015"=NA, "2014"=NA, 
                        "2013"=NA, "2012"=NA, "2011"=NA)


hit.fiobp=hit.template
hit.tbph=hit.template
wgts.fiobp=c(0.41793, 0.22106, 0.16801, 0.08962, 0.103)
wgts.tbph=c(0.4159, 0.2367, 0.1595, 0.1106, 0.077)
hit.fiobp$proj15=NA
hit.tbph$proj15=NA
hit.fiobp$proj16=NA
hit.tbph$proj16=NA
for (i in 1:length(hit.fiobp$Name)) {
  for (j in 2:6) {
    index=hit.indeces[i, j]
    ab=hit.post.2010[index, "AB"]
    hr=hit.post.2010[index, "HR"]
    so=hit.post.2010[index, "SO"]
    sf=hit.post.2010[index, "SF"]
    bb=hit.post.2010[index, "BB"]
    hbp=hit.post.2010[index, "HBP"]
    hits=hit.post.2010[index, "H"]
    tb=hit.post.2010[index, "H"]+hit.post.2010[index, "X2B"]+
      2*hit.post.2010[index, "X3B"]+3*hit.post.2010[index, "HR"]
    
    fiobp=(0.7*hr+0.3*(ab-so+sf)+bb+hbp)/(ab+bb+hbp+sf)
    tbph=tb/hits
    hit.fiobp[i, j]=fiobp
    hit.tbph[i, j]=tbph
  }
  inputs.fiobp=c(as.numeric(hit.fiobp[i, 3:6]), 0.310)
  inputs.tbph=c(as.numeric(hit.tbph[i, 3:6]), 1.610)
  proj.fiobp=weighted.mean(inputs.fiobp, wgts.fiobp, na.rm = TRUE)
  proj.tbph=weighted.mean(inputs.tbph, wgts.tbph, na.rm = TRUE)
  hit.fiobp[i, "proj15"]=proj.fiobp
  hit.tbph[i, "proj15"]=proj.tbph
  inputs.fiobp=c(as.numeric(hit.fiobp[i, 2:5]), 0.310)
  inputs.tbph=c(as.numeric(hit.tbph[i, 2:5]), 1.589)
  proj.fiobp=weighted.mean(inputs.fiobp, wgts.fiobp, na.rm = TRUE)
  proj.tbph=weighted.mean(inputs.tbph, wgts.tbph, na.rm = TRUE)
  hit.fiobp[i, "proj16"]=proj.fiobp
  hit.tbph[i, "proj16"]=proj.tbph
}

wgts=c(0.4159, 0.2367, 0.1595, 0.1106, 0.077)
{
hit.4.tbph=hit.tbph[which(is.na(hit.tbph$X2015)==FALSE &
  is.na(hit.tbph$X2014)==FALSE & is.na(hit.tbph$X2013)==FALSE &
    is.na(hit.tbph$X2012)==FALSE & is.na(hit.tbph$X2011)==FALSE),]
hit.4.tbph$wgavg=NA
hit.4.tbph$model=NA
modeled=NA
for (i in 1:length(hit.4.tbph$Name)) {
  inputs=c(as.numeric(hit.4.tbph[i, 3:6]), 0.310)
  hit.4.tbph[i, "wgavg"]=weighted.mean(inputs, wgts)
  for (j in 3:6) {
    modeled[j-2]=hit.4.tbph[i, j]*wgts[j-2]
  }
  hit.4.tbph[i, "model"]=sum(modeled)
}
} # testing weighted average
sd(hit.4.tbph$wgavg)
sd(hit.4.tbph$model)
if(sd(hit.4.tbph$wgavg)>sd(hit.4.tbph$model)){
  print("Increase int wgt")
} else if (sd(hit.4.tbph$wgavg)<sd(hit.4.tbph$model)){
  print("Decrease int wgt")
} 




plot(hit.tbph$proj15, hit.tbph$X2015)
lines(range.tbph, range.tbph, col="red")
plot(hit.fiobp$proj15, hit.fiobp$X2015)
lines(range.fiobp, range.fiobp, col="red")
cor(hit.fiobp[which(is.na(hit.fiobp$X2015)==FALSE), "proj15"], 
    hit.fiobp[which(is.na(hit.fiobp$X2015)==FALSE),"X2015"])
} # hitting projections 2015
{
# hit.proj.2016 stuff up with other dataset imports
hit.proj.2016$proj.fiobp=NA
hit.proj.2016$proj.tbph=NA
for (i in 1:length(hit.proj.2016$Name)) {
  {
    if(i==1){
      team.id=1
    }else if(hit.proj.2016[i, "Team"]==hit.proj.2016[i-1, "Team"]){
      team.id=hit.proj.2016[i-1, "team.id"]
    } else {
      team.id=hit.proj.2016[i-1, "team.id"]+1
    }
    hit.proj.2016[i, "team.id"]=team.id
  } # team id
  name=hit.proj.2016[i, "Name"]
  hit.index=match(name, hit.fiobp$Name)
  proj.fiobp=hit.fiobp[hit.index, "proj16"]
  proj.tbph=hit.tbph[hit.index, "proj16"]
  hit.proj.2016[i, "proj.fiobp"]=proj.fiobp
  hit.proj.2016[i, "proj.tbph"]=proj.tbph
} # team id, 2016 projections

# START OF TEAM PROJECTIONS

team.proj.2016=data.frame("Team"=NA, "FIOBP"=NA, "TBPH"=NA, "Runs"=NA, "ESPN"=NA)
for (i in 1:30) {
  team.players=which(hit.proj.2016$team.id==i)
  at.bats=hit.proj.2016[team.players, "AB"]
  team.name=(hit.proj.2016[team.players, "Team"])[1]
  team.fiobp=weighted.mean(hit.proj.2016[team.players, "proj.fiobp"], at.bats)
  team.tbph=weighted.mean(hit.proj.2016[team.players, "proj.tbph"], at.bats)
  team.proj.2016[i, "Team"]=team.name
  team.proj.2016[i, "FIOBP"]=team.fiobp
  team.proj.2016[i, "TBPH"]=team.tbph
  team.proj.2016[i, "ESPN"]=sum(hit.proj.2016[team.players, "Runs."])
}
proj.runs=4663.7473*team.proj.2016$FIOBP^1.7373*team.proj.2016$TBPH^0.5357-131
{
  park.factor=1.062
  park.factor[2]=0.937
  park.factor[3]=1.228
  park.factor[4]=1.191
  park.factor[5]=0.95
  park.factor[6]=1.115
  park.factor[7]=1.261
  park.factor[8]=1.436
  park.factor[9]=0.904
  park.factor[10]=0.902
  park.factor[11]=0.927
  park.factor[12]=1.019
  park.factor[13]=0.861
  park.factor[14]=0.918
  park.factor[15]=0.74
  park.factor[16]=1.103
  park.factor[17]=0.994
  park.factor[18]=0.87
  park.factor[19]=1.022
  park.factor[20]=0.944
  park.factor[21]=1.038
  park.factor[22]=0.933
  park.factor[23]=0.931
  park.factor[24]=0.878
  park.factor[25]=0.845
  park.factor[26]=0.857
  park.factor[27]=0.94
  park.factor[28]=1.141
  park.factor[29]=0.906
  park.factor[30]=1
} # park factors
team.proj.2016$Runs=proj.runs*((park.factor-1)/3+1)
team.proj.2016$Actual=NA
hit.proj.2016$KRC=(4664*hit.proj.2016$proj.fiobp^1.7373*
  hit.proj.2016$proj.tbph^0.5357-131)*hit.proj.2016$AB/5500
{
  act.runs=752
  act.runs[2]=649
  act.runs[3]=744
  act.runs[4]=878
  act.runs[5]=808
  act.runs[6]=716
  act.runs[7]=777
  act.runs[8]=845
  act.runs[9]=686
  act.runs[10]=750
  act.runs[11]=724
  act.runs[12]=675
  act.runs[13]=717
  act.runs[14]=725
  act.runs[15]=655
  act.runs[16]=671
  act.runs[17]=722
  act.runs[18]=671
  act.runs[19]=680
  act.runs[20]=653
  act.runs[21]=610
  act.runs[22]=729
  act.runs[23]=686
  act.runs[24]=768
  act.runs[25]=715
  act.runs[26]=779
  act.runs[27]=672
  act.runs[28]=765
  act.runs[29]=759
  act.runs[30]=763
} # actual runs
team.proj.2016$Actual=act.runs
team.proj.2016$Resid=team.proj.2016$Actual-team.proj.2016$Runs

# END OF TEAM PROJECTIONS

hit.proj.2016$KRC=(4663.7473*hit.proj.2016$proj.fiobp^1.7373*
  hit.proj.2016$proj.tbph^0.5357-131)*hit.proj.2016$AB/5000
hit.proj.2016$KRCrk=length(hit.proj.2016$KRC)+1-rank(hit.proj.2016$KRC)
} # hitting projections 2016
} # hitting pre-2016
{
{
# inputs: AVG, ERA, HR/9, BB/9, K/9
Pitching.slg.data <- 
  read.csv("~/Desktop/Miscellaneous/Baseball/Pitching slg data.csv", 
           stringsAsFactors=FALSE)
pit.tbph.model=lm(Pitching.slg.data$TBPH~Pitching.slg.data$AVG+
          Pitching.slg.data$ERA+Pitching.slg.data$K.9+Pitching.slg.data$BB.9+
            Pitching.slg.data$HR.9, data = Pitching.slg.data)
Pitching.slg.data$Model=fitted(pit.tbph.model)



} # estimating TBPH with given data
{
Pitching <- read.csv("~/Downloads/baseballdatabank-master/core/Pitching.csv", 
                     stringsAsFactors=FALSE)
pit.post.2010=Pitching[which(Pitching$yearID>2010 & Pitching$stint==1),]
pit.post.2010$AB=pit.post.2010$BFP-pit.post.2010$BB-pit.post.2010$HBP-
  pit.post.2010$SF-pit.post.2010$IBB-pit.post.2010$SH
pit.post.2010$Name=NA
pit.post.2010=pit.post.2010[which(pit.post.2010$IPouts>99),]
for (i in 1:length(pit.post.2010$playerID)) {
  pit.id=pit.post.2010[i, "playerID"]
  index=match(pit.id, Master$playerID)
  pit.name=Master[index, "Name"]
  pit.post.2010[i, "Name"]=pit.name
}

pit.stats=data.frame("Name"=pit.post.2010$Name, "Year"=pit.post.2010$yearID,
              "Team"=pit.post.2010$teamID, "IP"=round(pit.post.2010$IPouts/3),
                     "FIOBP"=NA, "TBPH"=NA)
pit.stats$FIOBP=(0.7*pit.post.2010$HR+0.3*(pit.post.2010$AB-pit.post.2010$SO+
  pit.post.2010$SF)+pit.post.2010$BB+pit.post.2010$HBP)/(pit.post.2010$AB+
  pit.post.2010$BB+pit.post.2010$HBP+pit.post.2010$SF)
pit.AVG=pit.post.2010$BAOpp
pit.ERA=pit.post.2010$ERA
Kper9=pit.post.2010$SO/pit.post.2010$IPouts*27
BBper9=pit.post.2010$BB/pit.post.2010$IPouts*27
HRper9=pit.post.2010$HR/pit.post.2010$IPouts*27
pit.stats$TBPH=1.625869-1.790023*pit.AVG+0.008591*pit.ERA-0.002626*Kper9+
  0.010958*BBper9+0.389008*HRper9
pit.stats$KERA=(4664*pit.stats$FIOBP^1.7373*pit.stats$TBPH^0.5357-131)/162
pit.stats$ERA=pit.post.2010$ERA
} # FIOBP, TBPH for each pitcher/year
{
pit.prv.indeces=data.frame("Name"=pit.stats$Name, "Year"=pit.stats$Year)
pit.prv.indeces[,3:7]=NA
names(pit.prv.indeces)[3:7]=paste("Yr x-", 0:4, sep = "")
n.qual=length(pit.stats$Name)
for (i in 1:n.qual) {
  name=as.character(pit.stats[i, "Name"])
  year=pit.stats[i, "Year"]
  for (j in 0:4) {
    index=which(pit.stats$Name==name & 
                  pit.stats$Year==year-j)
    if(length(index)==1){
      pit.prv.indeces[i, j+3]=index
    }
  }
}
} # previous indeces
{

pit.prv.fiobp=data.frame("Name"=pit.stats$Name, "Year"=pit.stats$Year)
pit.prv.fiobp[,3:7]=NA
names(pit.prv.fiobp)[3:7]=paste("Yr x-", 0:4, sep = "")
pit.prv.tbph=pit.prv.fiobp
for (i in 1:length(pit.prv.indeces$Name)) {
  for (j in 3:7) {
    index=pit.prv.indeces[i, j]
    fiobp=pit.stats[index, "FIOBP"]
    tbph=pit.stats[index, "TBPH"]
    pit.prv.fiobp[i, j]=fiobp
    pit.prv.tbph[i, j]=tbph
  }
}

pit.prv4.fiobp=pit.prv.fiobp[which(is.na(pit.prv.fiobp$`Yr x-1`)==FALSE & 
  is.na(pit.prv.fiobp$`Yr x-2`)==FALSE & 
    is.na(pit.prv.fiobp$`Yr x-3`)==FALSE & 
  is.na(pit.prv.fiobp$`Yr x-4`)==FALSE),]

pit.fiobp.model=lm(pit.prv4.fiobp[,"Yr x-0"]~(pit.prv4.fiobp[,"Yr x-1"]+
  pit.prv4.fiobp[,"Yr x-2"]+pit.prv4.fiobp[,"Yr x-3"]+ 
  pit.prv4.fiobp[,"Yr x-4"]), data=pit.prv4.fiobp)

# pit.fiobp.model=nls(pit.prv4.fiobp[,"Yr x-0"]~(a*pit.prv4.fiobp[,"Yr x-1"]
#   +b*pit.prv4.fiobp[,"Yr x-2"]+c*pit.prv4.fiobp[,"Yr x-3"]+ 
#   d*pit.prv4.fiobp[,"Yr x-4"]+ e*i), data=pit.prv4.fiobp, start = 
#     c(a=0.4, b=0.2, c=0.16, d=0.09, e=0.05, i=0.310))
pit.prv4.fiobp$model=fitted(pit.fiobp.model)
pit.wgts.fiobp=c(0.50766, 0.17439, 0.02210, 0.05615, 0.24)
pit.prv4.fiobp$wgavg=NA
for (i in 1:length(pit.prv4.fiobp$Name)) {
  yrs1=pit.prv4.fiobp[i, "Yr x-1"]
  yrs2=pit.prv4.fiobp[i, "Yr x-2"]
  yrs3=pit.prv4.fiobp[i, "Yr x-3"]
  yrs4=pit.prv4.fiobp[i, "Yr x-4"]
  inputs=c(yrs1, yrs2, yrs3, yrs4, 0.310)
  pit.prv4.fiobp[i, "wgavg"]=weighted.mean(inputs, pit.wgts.fiobp)
}



pit.prv4.tbph=pit.prv.tbph[which(is.na(pit.prv.tbph$`Yr x-1`)==FALSE & 
    is.na(pit.prv.tbph$`Yr x-2`)==FALSE & 
     is.na(pit.prv.tbph$`Yr x-3`)==FALSE & 
   is.na(pit.prv.tbph$`Yr x-4`)==FALSE),]

pit.tbph.model=lm(pit.prv4.tbph[,"Yr x-0"]~(pit.prv4.tbph[,"Yr x-1"]+
       pit.prv4.tbph[,"Yr x-2"]+pit.prv4.tbph[,"Yr x-3"]+ 
        pit.prv4.tbph[,"Yr x-4"]), data=pit.prv4.tbph)

# pit.tbph.model=nls(pit.prv4.tbph[,"Yr x-0"]~(a*pit.prv4.tbph[,"Yr x-1"]
#   +b*pit.prv4.tbph[,"Yr x-2"]+c*pit.prv4.tbph[,"Yr x-3"]+ 
#   d*pit.prv4.tbph[,"Yr x-4"]+ e*i), data=pit.prv4.tbph, start = 
#     c(a=0.4, b=0.2, c=0.16, d=0.09, e=0.05, i=0.310))
pit.prv4.tbph$model=fitted(pit.tbph.model)
pit.wgts.tbph=c(0.50766, 0.17439, 0.02210, 0.05615, 0.24)
pit.prv4.tbph$wgavg=NA
for (i in 1:length(pit.prv4.tbph$Name)) {
  yrs1=pit.prv4.tbph[i, "Yr x-1"]
  yrs2=pit.prv4.tbph[i, "Yr x-2"]
  yrs3=pit.prv4.tbph[i, "Yr x-3"]
  yrs4=pit.prv4.tbph[i, "Yr x-4"]
  inputs=c(yrs1, yrs2, yrs3, yrs4, 0.310)
  pit.prv4.tbph[i, "wgavg"]=weighted.mean(inputs, pit.wgts.tbph)
}
} # pitcher FIOBP and TBPH
{
pit.stats$FIP=((13*pit.post.2010$HR+3*(pit.post.2010$BB+pit.post.2010$HBP)-2*
                  pit.post.2010$SO))/pit.post.2010$IPouts*3+3.1

pit.prv.fip=data.frame("Name"=pit.stats$Name, "Year"=pit.stats$Year)
pit.prv.fip[,3:7]=NA
names(pit.prv.fip)[3:7]=paste("Yr x-", 0:4, sep = "")
for (i in 1:length(pit.prv.indeces$Name)) {
  for (j in 3:7) {
    index=pit.prv.indeces[i, j]
    fip=pit.stats[index, "FIP"]
    pit.prv.fip[i, j]=fip
  }
}
pit.prv4.fip=pit.prv.fip[which(is.na(pit.prv.fip$`Yr x-1`)==FALSE & 
                                     is.na(pit.prv.fip$`Yr x-2`)==FALSE & 
                                     is.na(pit.prv.fip$`Yr x-3`)==FALSE & 
                                     is.na(pit.prv.fip$`Yr x-4`)==FALSE),]

pit.fip.model=lm(pit.prv4.fip[,"Yr x-0"]~(pit.prv4.fip[,"Yr x-1"]+
    pit.prv4.fip[,"Yr x-2"]+pit.prv4.fip[,"Yr x-3"]+ 
    pit.prv4.fip[,"Yr x-4"]), data=pit.prv4.fip)

pit.prv4.fip$model=fitted(pit.fip.model)
pit.wgts.fip=c(0.37083, 0.28226, 0.10532, 0.02892, 0.212)
pit.prv4.fip$wgavg=NA
for (i in 1:length(pit.prv4.fip$Name)) {
  yrs1=pit.prv4.fip[i, "Yr x-1"]
  yrs2=pit.prv4.fip[i, "Yr x-2"]
  yrs3=pit.prv4.fip[i, "Yr x-3"]
  yrs4=pit.prv4.fip[i, "Yr x-4"]
  inputs=c(yrs1, yrs2, yrs3, yrs4, 4.5)
  pit.prv4.fip[i, "wgavg"]=weighted.mean(inputs, pit.wgts.fip)
}
sd(pit.prv4.fip$model)
sd(pit.prv4.fip$wgavg)
} # 2015 FIP projections
{
pit.proj.2016 <- 
  read.csv("~/Desktop/Miscellaneous/Baseball/Pitching projections 2016.csv", 
           stringsAsFactors=FALSE)

pit.names.16=pit.proj.2016$Name
pit.fip.proj=data.frame("Name"=pit.names.16, "2015"=NA, "2014"=NA, "2013"=NA, 
                        "2012"=NA, "2011"=NA, "proj15"=NA, "proj16"=NA)
for (i in 1:length(pit.names.16)) {
  for (j in 2:6) {
    name=pit.names.16[i]
    year=2017-j
    index=which(pit.stats$Name==name & pit.stats$Year==year)
    if(length(index)==1){
      player.fip=pit.stats[index, "FIP"]
      pit.fip.proj[i, j]=player.fip
    }
  }
}

pit.wgts.fip=c(0.37083, 0.28226, 0.10532, 0.02892, 0.212)
for (i in 1:length(pit.names.16)) {
  inputs.15=c(as.numeric(pit.fip.proj[i, 3:6]), 4.5)
  proj.15=weighted.mean(inputs.15, pit.wgts.fip, na.rm = TRUE)
  inputs.16=c(as.numeric(pit.fip.proj[i, 2:5]), 4.5)
  proj.16=weighted.mean(inputs.16, pit.wgts.fip, na.rm = TRUE)
  pit.fip.proj[i, "proj15"]=proj.15
  pit.fip.proj[i, "proj16"]=proj.16
}


pit.proj.2016$team.id=NA
pit.proj.2016$proj.fip=NA
for (i in 1:length(pit.proj.2016$Name)) {
  {
    if(i==1){
      team.id=1
    }else if(pit.proj.2016[i, "Team"]==pit.proj.2016[i-1, "Team"]){
      team.id=pit.proj.2016[i-1, "team.id"]
    } else {
      team.id=pit.proj.2016[i-1, "team.id"]+1
    }
    pit.proj.2016[i, "team.id"]=team.id
  } # team id
  
  name=pit.proj.2016[i, "Name"]
  pit.index=match(name, pit.fip.proj$Name)
  proj.fip=pit.fip.proj[pit.index, "proj16"]
  pit.proj.2016[i, "proj.fip"]=proj.fip
} # team id, 2015/16 projections
team.proj.2016$FIP=NA
team.proj.2016$Allowed=NA
team.proj.2016$ESPNa=NA
team.proj.2016$ActAlw=NA
team.proj.2016$AlwRes=NA
team.proj.2016$Wins=NA
team.proj.2016$ESPNw=NA
team.proj.2016$ActWin=NA
team.proj.2016$PythWin=NA
team.proj.2016$WinRes=NA
for (i in 1:30) {
  team.players=which(pit.proj.2016$team.id==i)
  innings=pit.proj.2016[team.players, "IP"]
  team.name=(pit.proj.2016[team.players, "Team"])[1]
  team.fip=weighted.mean(pit.proj.2016[team.players, "proj.fip"], innings)
  team.proj.2016[i, "Team"]=team.name
  team.proj.2016[i, "FIP"]=team.fip
  team.proj.2016[i, "ESPNa"]=sum(pit.proj.2016[team.players, "ER"])/0.926
}
team.proj.2016$Allowed=team.proj.2016$FIP*162/0.926
{
  allowed=890
  allowed[2]=779
  allowed[3]=715
  allowed[4]=694
  allowed[5]=556
  allowed[6]=854
  allowed[7]=676
  allowed[8]=860
  allowed[9]=715
  allowed[10]=721
  allowed[11]=701
  allowed[12]=712
  allowed[13]=727
  allowed[14]=638
  allowed[15]=682
  allowed[16]=733
  allowed[17]=889
  allowed[18]=617
  allowed[19]=702
  allowed[20]=761
  allowed[21]=796
  allowed[22]=758
  allowed[23]=770
  allowed[24]=707
  allowed[25]=631
  allowed[26]=712
  allowed[27]=713
  allowed[28]=757
  allowed[29]=666
  allowed[30]=612
} # runs actually allowed
team.proj.2016$ActAlw=allowed
team.proj.2016$AlwRes=allowed-team.proj.2016$Allowed
team.proj.2016$Wins=162*(team.proj.2016$Runs^1.83)/
  (team.proj.2016$Runs^1.83+team.proj.2016$Allowed^1.83)
team.proj.2016$ESPNw=162*(team.proj.2016$ESPN^1.83)/
  (team.proj.2016$ESPN^1.83+team.proj.2016$ESPNa^1.83)
team.proj.2016$PythWin=162*(team.proj.2016$Actual^1.83)/
  (team.proj.2016$Actual^1.83+team.proj.2016$ActAlw^1.83)
{
  wins=69
  wins[2]=68
  wins[3]=89
  wins[4]=93
  wins[5]=103
  wins[6]=68
  wins[7]=94
  wins[8]=75
  wins[9]=78
  wins[10]=86
  wins[11]=84
  wins[12]=81
  wins[13]=74
  wins[14]=91
  wins[15]=79
  wins[16]=73
  wins[17]=59
  wins[18]=87
  wins[19]=84
  wins[20]=69
  wins[21]=71
  wins[22]=78
  wins[23]=68
  wins[24]=86
  wins[25]=87
  wins[26]=86
  wins[27]=68
  wins[28]=95
  wins[29]=89
  wins[30]=95
} # actual wins
team.proj.2016$ActWin=wins
team.proj.2016$WinRes=team.proj.2016$ActWin-team.proj.2016$Wins
} # 2016 pitching projections
} # pitching pre-2016
# pre-2016 stuff takes 15-20 minutes to run
{
hitting.stats.2016 <- 
  read.csv("~/Desktop/Miscellaneous/Baseball/Hitting stats 2016.csv", 
           stringsAsFactors=FALSE)
hitting.stats.2016$FIOBP=(0.7*hitting.stats.2016$HR+0.3*(hitting.stats.2016$AB
        -hitting.stats.2016$SO+hitting.stats.2016$SF)+hitting.stats.2016$BB+
          hitting.stats.2016$HBP)/(hitting.stats.2016$AB+hitting.stats.2016$BB
          +hitting.stats.2016$HBP+hitting.stats.2016$SF)
hitting.stats.2016$TBPH=(hitting.stats.2016$X1B+2*hitting.stats.2016$X2B+
                           3*hitting.stats.2016$X3B+
                           4*hitting.stats.2016$HR)/hitting.stats.2016$H
hitting.stats.2016$KRC=(4664*hitting.stats.2016$FIOBP^1.7373*
            hitting.stats.2016$TBPH^0.5357-131)/5400*hitting.stats.2016$AB
} # 2016 fiobp, tbph, and krc
{
  hitting_projections_2017 <- read_excel(
    "~/Desktop/Miscellaneous/Baseball/hitting projections 2017.xlsx", 
    sheet = "clean", col_types = c("text", "text", "text"))
  hitting_projections_2017[which(hitting_projections_2017$Name==
        "Albert Almora Jr."), "Name"]="Albert Almora"
  
hit.names.17=hitting_projections_2017$Name
hit.fiobp.2017=data.frame("Name"=hit.names.17, "2016"=NA, "2015"=NA, 
                          "2014"=NA, "2013"=NA, "Proj17"=NA)
hit.tbph.2017=hit.fiobp.2017
matt.joyce="Matt Joyce"
hit.post.2010[which(hit.post.2010$Name=="Matthew Joyce"), "Name"]=matt.joyce
wgts.fiobp=c(0.41793, 0.22106, 0.16801, 0.08962, 0.103) 
wgts.tbph=c(0.4159, 0.2367, 0.1595, 0.1106, 0.077) 
for (i in 1:length(hit.names.17)) {
  name=hit.names.17[i]
  for (j in 2:5) {
    if(j==2){
      index=which(hitting.stats.2016$Name==name)
      fiobp=hitting.stats.2016[index, "FIOBP"]
      tbph=hitting.stats.2016[index, "TBPH"]
    } else {
      year=2018-j
      index=which(hit.post.2010$Name==name & hit.post.2010$yearID==year)
      HR=sum(hit.post.2010[index, "HR"])
      AB=sum(hit.post.2010[index, "AB"])
      SO=sum(hit.post.2010[index, "SO"])
      SF=sum(hit.post.2010[index, "SF"])
      BB=sum(hit.post.2010[index, "BB"])
      HBP=sum(hit.post.2010[index, "HBP"])
      HTS=sum(hit.post.2010[index, "H"])
      X2B=sum(hit.post.2010[index, "X2B"])
      X3B=sum(hit.post.2010[index, "X3B"])
      
      
      
      fiobp=(0.7*HR+0.3*(AB-SO+SF)+BB+HBP)/(AB+BB+HBP+SF)
      tbph=(HTS+X2B+2*X3B+3*HR)/HTS
    }
    if(length(index)>0){
      hit.fiobp.2017[i, j]=fiobp
      hit.tbph.2017[i, j]=tbph
    }
  }
  # weighted averages
  inputs.fiobp=c(as.numeric(hit.fiobp.2017[i, 2:5]), 0.310)
  inputs.tbph=c(as.numeric(hit.tbph.2017[i, 2:5]), 1.589)
  proj.fiobp=weighted.mean(inputs.fiobp, wgts.fiobp, na.rm = TRUE)
  proj.tbph=weighted.mean(inputs.tbph, wgts.tbph, na.rm = TRUE)
  hit.fiobp.2017[i, "Proj17"]=proj.fiobp
  hit.tbph.2017[i, "Proj17"]=proj.tbph
} # fiobp, tbph proj for 2017 players
hit.proj.2017=data.frame("Name"=hit.names.17, "FIOBP"=hit.fiobp.2017$Proj17, 
                         "TBPH"=hit.tbph.2017$Proj17, "KRC"=NA, "KRCrk"=NA)
hit.proj.2017$KRC=(4664*hit.proj.2017$FIOBP^1.7373*
                     hit.proj.2017$TBPH^0.5357-131)/9
hit.proj.2017$KRCrk=1+length(hit.names.17)-rank(hit.proj.2017$KRC)
} # 2017 hitting projections
{
pitching.stats.2016 <- 
  read.csv("~/Desktop/Miscellaneous/Baseball/pitching stats 2016.csv", 
           stringsAsFactors=FALSE)
pitching.stats.2016$FIP=(13*pitching.stats.2016$HR+3*(pitching.stats.2016$BB+
        pitching.stats.2016$HBP)-2*pitching.stats.2016$SO)/
  pitching.stats.2016$IP+3.1
pit.names.17=c(pitching.stats.2016$Name, setdiff(pit.proj.2016$Name, 
                            pitching.stats.2016$Name)) # change later
pit.fip.2017=data.frame("Name"=pit.names.17, "2016"=NA, "2015"=NA, "2014"=NA, 
                        "2013"=NA, "Proj17"=NA)
pit.wgts.fip=c(0.37083, 0.28226, 0.10532, 0.02892, 0.212)
for (i in 1:length(pit.names.17)) {
  name=pit.names.17[i]
  for (j in 2:5) {
    if(j==2){
      index=which(pitching.stats.2016$Name==name)
      fip=pitching.stats.2016[index, "FIP"]
    } else {
      year=2018-j
      index=which(pit.post.2010$Name==name & pit.post.2010$yearID==year)
      HR=sum(pit.post.2010[index, "HR"])
      BB=sum(pit.post.2010[index, "BB"])
      HBP=sum(pit.post.2010[index, "HBP"])
      SO=sum(pit.post.2010[index, "SO"])
      IP=sum(pit.post.2010[index, "IPouts"])/3
      
      fip=(13*HR+3*(BB+HBP)-2*SO)/IP+3.1
    }
    if(length(index)>0){
      pit.fip.2017[i, j]=fip
    }
  }
  inputs.fip=c(as.numeric(pit.fip.2017[i, 2:5]), 4.5)
  proj.fip=weighted.mean(inputs.fip, pit.wgts.fip, na.rm = TRUE
                         )
  pit.fip.2017[i, "Proj17"]=proj.fip
}
} # 2017 pitching projections

if(timed){
  end.time=Sys.time()
  time.diff=end.time-start.time
  time.diff
}

beepr::beep()
