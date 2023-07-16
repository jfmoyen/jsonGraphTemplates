
### This is to avoid check() notes when using global variables
# or unquoted arguments in dplyr/ggplot
utils::globalVariables(c("demo","sheet","x.data","y.data"))


############### Plot json template in Figaro ####################
#' Read a template defined in a json file and import it to  figaro graph
#' @export
#' @importFrom jsonlite read_json
#'
#' @param wrdata A matrix containing WR data. Probably GCDkit's WR,
#' as in many cases we need GCDkit-calculated things such as A/CNK etc.
#' @param lbl A data frame containing gCDkit labels.
#' @param json Name of the template file
#' @param path Path to json file
#' @param verbose Give debugging info
#' If not specified, looks in the json_template folder of the package
#' @details
#' This function reads a json template and plots a Figaro graph
#'  (figaro is GCDkit's internal plotting system, that allows
#'  a level of editing and interactivity). It can NOT work if GCDkit
#'  is not installed. This is the original function, intended as a
#'  replacement for GDkit::plotDiagram.
#'

plotDiagram_json <- function(wrdata=WR,lbl=labels,json, path = NULL,verbose=F){

# Debugging info
  if(verbose)(cat("Plotting",json,"\n",sep=" "))

# This is a pure GCDkit function ! It should fail if GCDkit is not here
  if (!requireNamespace("GCDkit", quietly = TRUE)) {
    stop(
      "Package \"GCDkit\" must be installed to use this function.",
      call. = FALSE
    )
  }

#### Read the json template ####
  if(is.null(path)){
    thejson <- system.file("json_templates",json,
                           package="jsonGraphTemplates")
  }else{
    thejson<-paste(path,json,sep="/")
  }

  graphDef<-jsonlite::read_json(thejson,simplifyVector = T)

#### Prepare the data ####
  preparedData <-data_preparation(graphDef,wrdata,lbl)

#### Main switch - what are we trying to plot ? ####

switch(EXPR = graphDef$diagramType,
       "binary" = pp<-plotDiagram_json_binary(graphDef,preparedData$wrdata,preparedData$lbl),
       "ternary" = pp<-plotDiagram_json_ternary(graphDef,preparedData$wrdata,preparedData$lbl),
       stop(paste("Sorry, plotting of type",graphDef$diagramType,"is not implemented yet",sep=" "))
       )

  invisible(pp)
}

############### Plot json template in Figaro: BINARY ####################
#' Inner function, for binary plots
#'
#' @param graphDef The template, loaded into a list from json
#' @param dd The plotting dataset
#' @param lbl The labels (GCDkit style)
#' @details
#' Internal function, that does the actual plotting
#' in the case of a binary plot

plotDiagram_json_binary <- function(graphDef,dd, lbl){
  # Get the X and Y values
  x.data <- GCDkit::calcCore(graphDef$axesDefinition$X,where="dd")$results
  y.data <- GCDkit::calcCore(graphDef$axesDefinition$Y,where="dd")$results

  ##### Optional tags #####
  # Parse axes labels, axes suppression and log scale
  parsedAxesOptions <- axes_parser_binary(graphDef)

  #### Parse the template proper #####
  if(is.null(graphDef$template)){
    template_nice <- NULL
  }else{
    template_nice<-lapply(graphDef$template,
                          template_element_parser)
  }


  #### Build the figaro "style sheet" ####
  sheet<-list(demo=list(fun="plot",
                        call=list(xlim = graphDef$limits$X,
                                  ylim = graphDef$limits$Y,
                                  xlab=parsedAxesOptions$xlab,
                                  ylab=parsedAxesOptions$ylab,
                                  log=parsedAxesOptions$which.log,
                                  bg="transparent",
                                  fg="black",
                                  xaxs = "i", yaxs = "i",
                                  #asp=1,
                                  axes=parsedAxesOptions$axes),
                        template=template_nice))

  # Assign to global env
  assign("sheet", sheet, .GlobalEnv)
  assign("x.data", x.data, .GlobalEnv)
  assign("y.data", y.data, .GlobalEnv)

  #### Create the actual figaro object and plot ####
  pp <- GCDkit::figaro(demo, prefix = "sheet")

  pp$draw(x.data, y.data,
          main=GCDkit::annotate(graphDef$fullName),
          xlab=parsedAxesOptions$xlab,
          ylab=parsedAxesOptions$ylab,
          col = subset(lbl, rownames(dd) %in% names(x.data),
                       "Colour",
                       drop = TRUE),
          pch = subset(lbl, rownames(dd) %in% names(x.data),
                       "Symbol",
                       drop = TRUE),
          cex = subset(lbl, rownames(dd) %in% names(x.data),
                       "Size",
                       drop = TRUE),
          plotting.function = "fromJSON",
          new = T
  )
invisible(pp)
}

############### Plot json template in Figaro: TERNARY ####################
#' Inner function, for binary plots
#'
#' @param graphDef The template, loaded into a list from json
#' @param dd The plotting dataset
#' @param lbl The labels (GCDkit style)
#' @details
#' Internal function, that does the actual plotting
#' in the case of a binary plot

plotDiagram_json_ternary <- function(graphDef,dd,lbl){

  # Get the A, B and C data (apices)
  a.data <- GCDkit::calcCore(graphDef$axesDefinition$A,where="dd")$results
  b.data <- GCDkit::calcCore(graphDef$axesDefinition$B,where="dd")$results
  c.data <- GCDkit::calcCore(graphDef$axesDefinition$C,where="dd")$results

  # Convert to X and Y
  sum_apices <- a.data+b.data+c.data

  x.data <- ((c.data/sum_apices) + (b.data / sum_apices) /2)
  y.data <- (sqrt(3)*(b.data / sum_apices)/2)

  ##### Optional tags #####
  # Parse axes labels, axes suppression and log scale
  parsedAxesOptions <- axes_parser_ternary(graphDef)

  #### Parse the template proper #####
  template_nice<-lapply(graphDef$template,
                      template_element_parser)

  #### Prepare the "pseudo-axes" labels, A, B and C ####
  A=list(type="text",x=0,y=-0.03,text=annotate(parsedAxesOptions$alab),adj=0.5)
  B=list(type="text",x=0.5,y=sqrt(3)/2+.03,text=annotate(parsedAxesOptions$blab),adj=0.5)
  C=list(type="text",x=1,y=-0.03,text=annotate(parsedAxesOptions$clab),adj=0.5)

  ####Merge the elements ####
  template_nice<- c(template_nice,A=list(A),B=list(B),C=list(C))

  #### Build the figaro "style sheet" ####
  sheet<-list(demo=list(fun="plot",
                        call=list(xlim = graphDef$limits$X,
                                  ylim = graphDef$limits$Y,
                                  xlab=NULL,
                                  ylab=NULL,
                                  log="",
                                  bg="transparent",
                                  fg="black",
                                  xaxs = "i", yaxs = "i",
                                  asp=1,
                                  axes=FALSE),
                        template=template_nice ))

  # Assign to global env
  assign("sheet", sheet, .GlobalEnv)
  assign("x.data", x.data, .GlobalEnv)
  assign("y.data", y.data, .GlobalEnv)

  #### Create the actual figaro object and plot ####
  pp <- GCDkit::figaro(demo, prefix = "sheet")

  pp$draw(x.data, y.data,
          main=GCDkit::annotate(graphDef$fullName),
          xlab=NULL,
          ylab=NULL,
          col = subset(lbl, rownames(dd) %in% names(x.data),
                       "Colour",
                       drop = TRUE),
          pch = subset(lbl, rownames(dd) %in% names(x.data),
                       "Symbol",
                       drop = TRUE),
          cex = subset(lbl, rownames(dd) %in% names(x.data),
                       "Size",
                       drop = TRUE),
          plotting.function = "fromJSON",
          new = T
  )

invisible(pp)
}

#### Template prettyfier ####
#' This function looks at individual template elements,
#' and modifies them as desired
#' @param tpl_el A template element
template_element_parser<-function(tpl_el){

#### Prettyfy text by using annotate, where possible
  # if(tpl_el$type=="text"&&!grepl("\\n",tpl_el$text)){
  #   tpl_el$text <- as.expression(GCDkit::annotate(tpl_el$text) )
  # }

  return(tpl_el)
}

#### Custom axes names ####
#' Ancilary function to get proper labels for axes, if supplied
#' @param graphDef A graph definition, loaded from json
#' @param W Axis to process
.make.names<-function(graphDef,W){
  if(!is.null(graphDef$axesName[[W]])){
    wlab <- GCDkit::annotate(graphDef$axesName[[W]])
  }else{
    wlab <- GCDkit::annotate(graphDef$axesDefinition[[W]])
  }
}


#### Axes option parser (binary diagrams) ####
#' This function parses various elements of a graph definition
#' used to control the appearance of axes. They include
#' xlab, ylab, axes suppression of needed (fr ternary, mostly),
#' log scale.
#' @param graphDef A graph definition, loaded from json
axes_parser_binary<-function(graphDef){

  # Custom axes names
  xlab <- .make.names(graphDef,"X")
  ylab <- .make.names(graphDef,"Y")

  # Log scales
  if(is.null(graphDef$log)){
    which.log <- ""
  }else{
    which.log <- graphDef$log
  }

  # Suppress axes
  if(is.null(graphDef$suppressAxes) || !graphDef$suppressAxes){
    axes <- TRUE
  }else{
    axes <- FALSE
  }

  return(list(xlab = xlab,
              ylab=ylab,
              which.log=which.log,
              axes=axes))
}

#### Axes option parser (ternary diagrams) ####
#' This function parses various elements of a graph definition
#' used to control the appearance of axes. They include
#' xlab, ylab, axes suppression of needed (fr ternary, mostly),
#' log scale.
#' @param graphDef A graph definition, loaded from json
axes_parser_ternary<-function(graphDef){

  alab <- .make.names(graphDef,"A")
  blab <- .make.names(graphDef,"B")
  clab <- .make.names(graphDef,"C")

  return(list(alab = alab,
              blab=blab,
              clab=clab))
}

#### Data preparation ####
#' This function prepares the data by filtering it,
#' if a filter condition was supplied; and by transform it, if needed.
#'
#' @param graphDef A graph definition, loaded from json
#' @param wrdata WR data (typically WR in GCDkit, or equivalent)
#' @param lbl label data (typically labels in GCDkit, or equivalent)
#'
data_preparation<-function(graphDef,wrdata,lbl){
  # If required by the template, calculate the transformed data
  if(is.null(graphDef$dataTransform)){
    dd<-wrdata
  }else{
    dd<-eval(parse(text=graphDef$dataTransform))()
  }

  # If we have a filter

  if(!is.null(graphDef$dataFilter)){
    selected <- selectSubset(what=graphDef$dataFilter,
                             where=cbind(lbl,dd),
                             all.nomatch=F,
                             save=F)
    if(selected==""){stop("No data to plot matching criteria")}
    dd <- dd[selected,,drop=F]
    lbl <- lbl[selected,,drop=F]
  }

  return(list(wrdata = dd,
              lbl = lbl))

}

