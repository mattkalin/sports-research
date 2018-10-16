library(XML)
library(beepr)
{
# tmp = tempfile()
# on.exit(unlink(tmp))
# download.file("http://www.maxpreps.com/leaders/basketball-winter-16-17/,scoring/stat-leaders.htm?classyear=12&position=all", destfile = tmp, quiet = TRUE)
# x = readLines(tmp, warn = FALSE)
# y = readHTMLTable(x, header = TRUE)
# header.garbage = names(y)
# z = as.data.frame(y) # header names are now replaced with garbage and V1
# a = readHTMLTable("http://www.maxpreps.com/leaders/basketball-winter-16-17/,field+goals/stat-leaders.htm?classyear=12&position=all", header = TRUE)
# b = as.data.frame(a)
# names(b) = gsub(paste(header.garbage, ".", sep = ""), "", names(b))
# second.page = "http://www.maxpreps.com/leaders/basketball-winter-16-17/,field+goals/stat-leaders-2.htm?classyear=12&position=all"
} # research
categories = c("field+goals", "assists", "rebounds", "free+throws")
downloadMpFile = function(end.year, page, category){
  page.string = ""
  if(page > 1){
    page.string = paste("-", page, sep = "")
  }
  if(end.year > 2000){
    end.year = end.year - 2000
  }
  url.string = paste("http://www.maxpreps.com/leaders/basketball-winter-", 
              end.year - 1, "-", end.year, "/,", category, "/stat-leaders", 
                     page.string, ".htm?classyear=12&position=all", sep = "")
  html.table = readHTMLTable(url.string, header = TRUE, stringsAsFactors = FALSE)
  df = as.data.frame(html.table)
  header.garbage = names(html.table)
  names(df) = gsub(paste(header.garbage, ".", sep = ""), "", names(df))
  for(i in 4:ncol(df)){
    df[, i] = as.numeric(df[, i])
  }
  return(df)
}
CombineDF=function(old.df1, old.df2, old.df3=NULL, 
                   old.df4=NULL, old.df5=NULL){
  new.df<-old.df1
  rows=nrow(old.df1)
  if(is.null(old.df1)){
    new.df = old.df2
  } else if(is.null(old.df2)==FALSE){
    rows2=nrow(old.df2)
    new.df[(rows+1):(rows + rows2),]<-old.df2
  }
  # if(is.null(old.df3)==FALSE){
  #   new.df[(2*rows+1):(3*rows),]<-old.df3
  # }
  # if(is.null(old.df4)==FALSE){
  #   new.df[(3*rows+1):(4*rows),]<-old.df4
  # }
  # if(is.null(old.df5)==FALSE){
  #   new.df[(4*rows+1):(5*rows),]<-old.df5
  # }
  return(new.df)
} # combine df
spring.year = 2016
for(i in 1:10){
  if(i==1){
    field.goal.data = NULL
    assists.data = NULL
    rebounds.data = NULL
    free.throw.data = NULL
  }
  field.goal.data = CombineDF(field.goal.data, 
                              downloadMpFile(spring.year, i, categories[1]))
  assists.data = CombineDF(assists.data,
                           downloadMpFile(spring.year, i, categories[2]))
  rebounds.data = CombineDF(rebounds.data, 
                            downloadMpFile(spring.year, i, categories[3]))
  free.throw.data = CombineDF(free.throw.data, 
                              downloadMpFile(spring.year, i, categories[4]))
}
large.comb.data = field.goal.data
three.made = large.comb.data$FGM - large.comb.data$`2PM`
large.comb.data$EFG = (0.5 * three.made + large.comb.data$FGM) / 
  large.comb.data$FGA
large.comb.data$TO = NA 
large.comb.data$OReb = NA 
large.comb.data$FTA = NA
large.comb.data$REB = NA 
large.comb.data$FTM = NA
large.comb.data$AST = NA
for(i in 1:nrow(large.comb.data)){ # change to nrow(large.comb.data)
  name = as.character(large.comb.data[i, "Name"])
  asst.index = match(name, assists.data$Name)
  reb.index = match(name, rebounds.data$Name)
  fthrow.index = match(name, free.throw.data$Name)
  if(length(asst.index) == 1){
    large.comb.data[i, "TO"] = assists.data[asst.index, "TO"]
    large.comb.data[i, "AST"] = assists.data[asst.index, "Ast"]
  }
  if(length(reb.index)){
    large.comb.data[i, "OReb"] = rebounds.data[reb.index, "Off"]
    large.comb.data[i, "REB"] = rebounds.data[reb.index, "Reb"]
  }
  if(length(fthrow.index) == 1){
    large.comb.data[i, "FTA"] = free.throw.data[fthrow.index, "FTA"]
    large.comb.data[i, "FTM"] = free.throw.data[fthrow.index, "FTM"]
  }
}


# stats i need
# eff field goal pct (incl as AFG.)
# turnover pct
  # turnovers incl
  # possessions = FGA - OReb + TO + 0.475*fta
# oreb pct can't be done, so i'll just do like reb/pos or something like that
# ftrate = ftm / fga



beep()
