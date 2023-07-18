########## Functions for data transformation ################
# Mostly we use those from GCDkit, but we can define more

### This is to avoid check() notes when using global variables
# or unquoted arguments in dplyr/ggplot
utils::globalVariables(c("WR","mw"))



############### Calculate anhydrous composition ####################
#' Recalculate WR composition to anhydrous, as done in gCDkit (First.r)
#' Ultimately there will probably be a GCDkit function by that name !
#' @export
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



############### Calculate Ohta & Arai parameters ####################
#' Recalculate Otha + Arai M, F and W parameters
#' @export
#' @param where A matrix with whole-rock composition, probably coming from GCDkit

OhtaAraiParams<-function(where){

  # This will fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this data transform",
      call. = FALSE
    )
  }

  usedox<-c("SiO2","TiO2","Al2O3","Fe2O3t","MgO","CaO_corr","Na2O","K2O")
  oxsubset<-c("SiO2","TiO2","Al2O3","FeOt","MgO","CaO","Na2O","K2O","P2O5","CO2")

  my.WR<-where[,oxsubset]

  Fe2O3t<-my.WR[,"FeOt"]*1.111
  my.WR[is.na(my.WR)]<-0

  mCaO<-mw["Ca"]+mw["O"]
  mP2O5<-2*mw["P"]+5*mw["O"]
  mCO2<-mw["C"]+2*mw["O"]

  apatite<-(my.WR[,"P2O5"]/mP2O5)/3
  carb<-my.WR[,"CO2"]/mCO2
  CaO_corr<-my.WR[,"CaO"]-carb*mCaO-apatite*mCaO*10

  ### Empirical fix for missing TiO2 values: TiO2 = FeOT/7
    fixme<-my.WR[,"TiO2"]==0
    my.WR[fixme,"TiO2"]<-my.WR[fixme,"FeOt"]/7

  my.WR<-cbind(my.WR,Fe2O3t,CaO_corr)

  y <- t(apply(my.WR[,usedox],
                FUN = function(r){
                  r / sum(r,na.rm = T) * 100
                },
                MARGIN = 1))

#  my.WR<-GCDkit::normalize2total(my.WR[,usedox],100)

  M <- exp(-0.395*log(my.WR[,"SiO2"])+0.206*log(my.WR[,"TiO2"])-0.316*log(my.WR[,"Al2O3"])+0.160*log(my.WR[,"Fe2O3t"])
               +0.246*log(my.WR[,"MgO"])+0.368*log(my.WR[,"CaO_corr"])+0.073*log(my.WR[,"Na2O"])-0.342*log(my.WR[,"K2O"])+2.266)

  F <- exp(0.191*log(my.WR[,"SiO2"])-0.397*log(my.WR[,"TiO2"])+0.020*log(my.WR[,"Al2O3"])-0.375*log(my.WR[,"Fe2O3t"])
               -0.243*log(my.WR[,"MgO"])+0.079*log(my.WR[,"CaO_corr"])+0.392*log(my.WR[,"Na2O"])+0.333*log(my.WR[,"K2O"])-0.892)

  W <- exp(0.203*log(my.WR[,"SiO2"])+0.191*log(my.WR[,"TiO2"])+0.296*log(my.WR[,"Al2O3"])+0.215*log(my.WR[,"Fe2O3t"])
               -0.002*log(my.WR[,"MgO"])-0.448*log(my.WR[,"CaO_corr"])-0.464*log(my.WR[,"Na2O"])+0.008*log(my.WR[,"K2O"])-1.374)

  ret <- cbind(M,F,W)
  return(as.matrix(ret))
  }
