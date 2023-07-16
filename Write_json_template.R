

#### Main function ####
readFigaroTemplate<-function(diagram,plot=F,...){

  if(class(diagram)=="character"){
    do.call(diagram,list(...))
  }else{
    diagram(...)
  }

  xlims <- sheet$demo$call$xlim
  ylims <- sheet$demo$call$ylim

  xAxis <- as.character(sheet$demo$call$xlab)
  yAxis <- as.character(sheet$demo$call$ylab)

  ## X and Y scale (log or natural)

  islog <- as.character(sheet$demo$call$log)

  template <- sheet$demo$template
  template1 <- template

  for(i in 1:length(template)){
    # Add name to the first element
    nm <- names(template[[i]])
    names(template[[i]]) <- c("type",nm[-1])
    # # Tag the lines used for classification
    # if(i %in% template$clssf$use ){
    #   template[[i]][["clssf"]] <- T
    # }
  }


  return(list(
    name = diagram,
    fullName = sheet$demo$template$GCDkit$plot.name,
    details = "",
    reference = "",
    url = "",
    templateAuthor = "J.-F. Moyen [jfmoyen@gmail.com]",
    templateHistory = "Converted from original GCDkit function",
    diagramType=sheet$demo$template$GCDkit$plot.type,
    dataTransform = NULL,
    axesDefinition = list(X=xAxis,Y=yAxis),
    limits=list(X=xlims,Y=ylims),
    log=islog,
    template = template
  ))
}

##########
ee<-readFigaroTemplate("Sylvester")
#ee$template$B$text<-NULL
ee.json <- prettify(toJSON(ee,null="null",auto_unbox = T))
write(ee.json,"./inst/json_templates/Sylvester.json")
