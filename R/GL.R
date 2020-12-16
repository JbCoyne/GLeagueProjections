auto.glproject <- function(the.df){
  new.df <- as.data.frame(the.df)
  USG <- the.df[,"USG"]
  G_FGA <- the.df[,"G_FGA"]
  G_FG. <- the.df[,"G_FGpct"]
  G_3PA <- the.df[,"G_3PA"]
  G_3P. <- the.df[,"G_3Ppct"]
  G_FTA <- the.df[,"G_FTA"]
  G_FT. <- the.df[,"G_FTpct"]
  G_REB <- the.df[,"G_REB"]
  G_AST <- the.df[,"G_AST"]
  G_TOV <- the.df[,"G_TOV"]
  G_STL <- the.df[,"G_STL"]
  G_BLK <- the.df[,"G_BLK"]
  G_PF <- the.df[,"G_PF"]
  
  
  glprojection <- 671.8174179 - (270.0541246 * USG) - (-0.5566571 * G_FGA) - (3.8808976 * G_FG.) - (6.9875941 * G_3PA) + (1.4492974 * G_3P.) - (3.6217892 * G_FTA) - (0.8521167 * G_FT.) - (5.2822759 * G_REB) - (6.8648295 * G_AST) + (24.2693587 * G_TOV) - (23.9522787 * G_STL) - (21.2636724 * G_BLK) + (14.8210331 * G_PF)
  
  temp.df <- (cbind(new.df, glprojection))
  
  Tier <- ifelse(temp.df$glprojection <= 300, Tier <- 1, Tier <- 2)
  
  final.df <- cbind(temp.df, Tier)
  
  Proj.Min <- -(0.0000000316*(glprojection)^3)+(0.0000944366*(glprojection)^2)-(0.0800462429*(glprojection))+33.6529100168
  
  Prob_InRotation <- round(ifelse(Proj.Min >= 24, Prob_InRotation <- 99.9, Prob_InRotation <- (-0.2857142857142857*glprojection)+142.85714285714286),2)
  
  Output.df <- cbind(final.df, Prob_InRotation)
}

manual.glproject <- function(Usage, FG.Att, FG.Pct, Threes.Att, Threes.Pct, FT.Att, FT.Pct, Rebounds, Assists, Turnovers, Steals, Blocks, Fouls){
  new.df <- as.data.frame(the.df)
  USG <- Usage
  G_FGA <- FG.Att
  G_FG. <- FG.Pct
  G_3PA <- Threes.Att
  G_3P. <- Threes.Pct
  G_FTA <- FT.Att
  G_FT. <- FT.Pct
  G_REB <- Rebounds
  G_AST <- Assists
  G_TOV <- Turnovers
  G_STL <- Steals
  G_BLK <- Blocks
  G_PF <- Fouls
  
  
  glprojection <- 671.8174179 - (270.0541246 * USG) - (-0.5566571 * G_FGA) - (3.8808976 * G_FG.) - (6.9875941 * G_3PA) + (1.4492974 * G_3P.) - (3.6217892 * G_FTA) - (0.8521167 * G_FT.) - (5.2822759 * G_REB) - (6.8648295 * G_AST) + (24.2693587 * G_TOV) - (23.9522787 * G_STL) - (21.2636724 * G_BLK) + (14.8210331 * G_PF)
  
  temp.df <- (cbind(new.df, glprojection))
  
  Tier <- ifelse(temp.df$glprojection <= 300, Tier <- 1, Tier <- 2)
  
  final.df <- cbind(temp.df, Tier)
  
  Proj.Min <- -(0.0000000316*(glprojection)^3)+(0.0000944366*(glprojection)^2)-(0.0800462429*(glprojection))+33.6529100168
  
  Prob_InRotation <- round(ifelse(Proj.Min >= 24, Prob_InRotation <- 99.9, Prob_InRotation <- (-0.2857142857142857*glprojection)+142.85714285714286), 2)
  
  Output.df <- cbind(final.df, Prob_InRotation)
}