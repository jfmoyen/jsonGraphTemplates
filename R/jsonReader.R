
### This is to avoid check() notes when using global variables
# or unquoted arguments in dplyr/ggplot
utils::globalVariables(c("WR","demo","sheet","x.data","y.data"))


############### Plot json template in Figaro ####################
#' Read a template defined in a json file and import it to  figaro graph
#' @export
#' @importFrom jsonlite read_json
#'
#' @param json Name of the template file
#' @param path Path to json file
#' @param verbose Give debugging info
#' If not specified, looks in the json_template folder of the package
#' Details
#' This function reads a json template and plots a Figaro graph
#'  (figaro is GCDkit's internal plotting system, that allows
#'  a level of editing and interactivity). It can NOT work if GCDkit
#'  is not installed. This is the original function, intended as a
#'  replacement for GDkit::plotDiagram.
#'

plotDiagram_json <- function(json, path = NULL,verbose=F){

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
  dd<-WR
}else{
  dd<-eval(parse(text=tpl$dataTransform))()
}

# Get the X and Y values
  x.data <- calcCore(tpl$axesDefinition$X,where="dd")$results
  y.data <- calcCore(tpl$axesDefinition$Y,where="dd")$results

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
          col = subset(labels, rownames(WR) %in% names(x.data),
                       "Colour",
                       drop = TRUE),
          pch = subset(labels, rownames(WR) %in% names(x.data),
                       "Symbol",
                       drop = TRUE),
          cex = subset(labels, rownames(WR) %in% names(x.data),
                       "Size",
                       drop = TRUE),
          plotting.function = "fromJSON",
          new = T
  )

}
