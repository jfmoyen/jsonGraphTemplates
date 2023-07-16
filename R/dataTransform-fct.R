########## Functions for data transformation ################
# Mostly we use those from GCDkit, but we can define more

############### Calculate anhydrous composition ####################
#' @param where A matrix with whole-rock composition, probably coming from GCDkit
recastAnhydrous <- function(where=WR){
  anhydrous.short<-c("SiO2","TiO2","Al2O3","FeOt","MnO","MgO","CaO","Na2O","K2O","P2O5")
  anhydrous.long<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5")

  if(all(is.na(where[,"Fe2O3"]))){
    anhydrous<-anhydrous.short
  }else{
    anhydrous<-c(anhydrous.long,"FeOt")
  }

  WRanh<-matrix(data = NA,
                ncol = length(anhydrous), nrow = nrow(where),
                byrow = TRUE, dimnames = NULL)

  rownames(WRanh)<-rownames(where)
  colnames(WRanh)<-anhydrous

  ee<-lapply(rownames(where),function(f){
    if(any(is.na(where[,"Fe2O3"]))|any(is.na(where[,"FeO"]))){
      WRanh[f,anhydrous.short]<<-where[f,anhydrous.short]/sum(where[f,anhydrous.short],na.rm=TRUE)*100
    }else{
      # Only if both FeO and Fe2O3 are available for ALL the analyses!
      WRanh[f,anhydrous.long]<<-where[f,anhydrous.long]/sum(where[f,anhydrous.long],na.rm=TRUE)*100
    }
  })
  #assign("WRanh",WRanh,.GlobalEnv)
  return(WRanh)
}

