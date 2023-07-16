
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

  tpl<-jsonlite::read_json(thejson,simplifyVector = T)

#### Prepare the data ####

# If required by the template, calculate the tranformed data
if(is.null(tpl$dataTransform)){
  dd<-wrdata
}else{
  dd<-eval(parse(text=tpl$dataTransform))()
}

#### Main switch - what are we trying to plot ? ####

switch(EXPR = tpl$diagramType,
       "binary" = plotDiagram_json_binary(tpl,dd,lbl),
       "ternary" = plotDiagram_json_ternary(tpl,dd,lbl),
       stop(paste("Sorry, plotting of type",tpl$diagramType,"is not implemented yet",sep=" "))
       )
}

############### Plot json template in Figaro: BINARY ####################
#' Inner function, for binary plots
#'
#' @param tpl The template, loaded into a list from json
#' @param dd The plotting dataset
#' @param lbl The labels (GCDkit style)
#' @details
#' Internal function, that does the actual plotting
#' in the case of a binary plot

plotDiagram_json_binary <- function(tpl,dd, lbl){
  # Multiplier (optional)
  if(is.null(tpl$dataMultiply)){
    dataMultiply <- 1
  }else{
    dataMultiply <- tpl$dataMultiply
    }

  # Get the X and Y values
  x.data <<- GCDkit::calcCore(tpl$axesDefinition$X,where="dd")$results * dataMultiply
  y.data <<- GCDkit::calcCore(tpl$axesDefinition$Y,where="dd")$results * dataMultiply

  #### Optional tags ####
  # Custom axes names
  if(!is.null(tpl$axesName$X)){
    xlab <- GCDkit::annotate(tpl$axesName$X)
  }else{
    xlab <- GCDkit::annotate(tpl$axesDefinition$X)
  }

  if(!is.null(tpl$axesName$Y)){
    ylab <- GCDkit::annotate(tpl$axesName$Y)
  }else{
    ylab <- GCDkit::annotate(tpl$axesDefinition$Y)
  }

  # Log scales
  if(is.null(tpl$log)){
    which.log <- ""
  }else{
    which.log <- tpl$log
  }

  # Suppress axes
  if(is.null(tpl$supressAxes) || !tpl$supressAxes){
    axes <- TRUE
  }else{
    axes <- FALSE
  }

  #### Build the figaro "style sheet" ####
  sheet<-list(demo=list(fun="plot",
                        call=list(xlim = tpl$limits$X,
                                  ylim = tpl$limits$Y,
                                  xlab=xlab,
                                  ylab=ylab,
                                  log=which.log,
                                  bg="transparent",
                                  fg="black",
                                  xaxs = "i", yaxs = "i",
                                  #asp=1,
                                  axes=axes),
                        template=tpl$template))

  # Assign to global env
  assign("sheet", sheet, .GlobalEnv)
  assign("x.data", x.data, .GlobalEnv)
  assign("y.data", y.data, .GlobalEnv)

  #### Create the actual figaro object and plot ####
  pp <- GCDkit::figaro(demo, prefix = "sheet")

  pp$draw(x.data, y.data,
          main=GCDkit::annotate(tpl$fullName),
          xlab=xlab,
          ylab=ylab,
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
}

############### Plot json template in Figaro: TERNARY ####################
#' Inner function, for binary plots
#'
#' @param tpl The template, loaded into a list from json
#' @param dd The plotting dataset
#' @param lbl The labels (GCDkit style)
#' @details
#' Internal function, that does the actual plotting
#' in the case of a binary plot

plotDiagram_json_ternary <- function(tpl,dd,lbl){

  # Multiplier (optional)
  if(is.null(tpl$dataMultiply)){
    dataMultiply <- 1
  }else{
    dataMultiply <- tpl$dataMultiply
  }

  # Get the A, B and C data (apices)
  a.data <- GCDkit::calcCore(tpl$axesDefinition$A,where="dd")$results
  b.data <- GCDkit::calcCore(tpl$axesDefinition$B,where="dd")$results
  c.data <- GCDkit::calcCore(tpl$axesDefinition$C,where="dd")$results

  # Convert to X and Y
  sum_apices <- a.data+b.data+c.data

  x.data <<- ((c.data/sum_apices) + (b.data / sum_apices) /2) * dataMultiply
  y.data <<- (sqrt(3)*(b.data / sum_apices)/2)* dataMultiply

  #### Optional tags ####
  # Custom axes names
  if(!is.null(tpl$axesName$X)){
    xlab <- GCDkit::annotate(tpl$axesName$X)
  }else{
    xlab <- GCDkit::annotate(tpl$axesDefinition$X)
  }

  if(!is.null(tpl$axesName$Y)){
    ylab <- GCDkit::annotate(tpl$axesName$Y)
  }else{
    ylab <- GCDkit::annotate(tpl$axesDefinition$Y)
  }

  # Log scales
  if(is.null(tpl$log)){
    which.log <- ""
  }else{
    which.log <- tpl$log
  }

  # Suppress axes
  if(is.null(tpl$supressAxes) || !tpl$supressAxes){
    axes <- TRUE
  }else{
    axes <- FALSE
  }


template_nice<-lapply(tpl$template,
       niceText)

  #### Build the figaro "style sheet" ####
  sheet<-list(demo=list(fun="plot",
                        call=list(xlim = tpl$limits$X,
                                  ylim = tpl$limits$Y,
                                  xlab=xlab,
                                  ylab=ylab,
                                  log=which.log,
                                  bg="transparent",
                                  fg="black",
                                  xaxs = "i", yaxs = "i",
                                  asp=1,
                                  axes=axes),
                        template=template_nice))

  # Assign to global env
  assign("sheet", sheet, .GlobalEnv)
  assign("x.data", x.data, .GlobalEnv)
  assign("y.data", y.data, .GlobalEnv)

  #### Create the actual figaro object and plot ####
  pp <- GCDkit::figaro(demo, prefix = "sheet")

  pp$draw(x.data, y.data,
          main=GCDkit::annotate(tpl$fullName),
          xlab=xlab,
          ylab=ylab,
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
}
##### ancillary function to format text using GCDkit annotate
#' @param tpl_el A template element
#' @return if the element is a text AND has no line break,
#' an expression containing the formatted version of it.
#' else, the element itself.
niceText <- function(tpl_el){
  if(tpl_el$type=="text"&&!grepl("\\n",tpl_el$text)){
    tpl_el$text <- GCDkit::annotate(tpl_el$text)
  }
return(tpl_el)
}
