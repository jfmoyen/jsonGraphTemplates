#######################################################
#
#          Figaro converter
#
#######################################################

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
#' If not specified, looks in the json_template folder of the package
#' @param verbose Give debugging info
#' @param template_options Options passed to the parser.
#' They are of the form list(switch1 = T, switch2 = F, etc)
#' and they wll affect template elements that have a switch.
#' @details
#' This function reads a json template and plots a Figaro graph
#'  (figaro is GCDkit's internal plotting system, that allows
#'  a level of editing and interactivity). It can NOT work if GCDkit
#'  is not installed. This is the original function, intended as a
#'  replacement for GDkit::plotDiagram.
#'

plotDiagram_json <- function(json, path = NULL,
                             wrdata=WR,lbl=labels,
                             verbose=F,
                             template_options=NULL,
                             template_colors=NULL){

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
  graphDef <- json_loader(json,path)

  #### Prepare the data ####
  preparedData <-data_preparation(graphDef,wrdata,lbl)

  #### Main switch - what are we trying to plot ? ####

  switch(EXPR = graphDef$diagramType,
         "binary" = pp<-plotDiagram_json_binary(graphDef,preparedData$wrdata,preparedData$lbl,template_options,template_colors),
         "ternary" = pp<-plotDiagram_json_ternary(graphDef,preparedData$wrdata,preparedData$lbl,template_options,template_colors),
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
#' @param template_options Further arguments passed to the parser, typically switches

#' @details
#' Internal function, that does the actual plotting
#' in the case of a binary plot

plotDiagram_json_binary <- function(graphDef,dd, lbl,
                                    template_options,template_colors){
  # Get the X and Y values
  x.data <- GCDkit::calcCore(graphDef$axesDefinition$X,where="dd")$results
  y.data <- GCDkit::calcCore(graphDef$axesDefinition$Y,where="dd")$results

  ##### Optional tags #####
  # Parse axes labels, axes suppression and log scale
  parsedAxesOptions <- axes_parser_binary(graphDef)

  #### Parse the template proper #####
  parsedTemplate <- parse_template(graphDef,template_options,template_colors)

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
                        template=parsedTemplate))

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
#' @param template_options Further arguments passed to the parser, typically switches

#' @details
#' Internal function, that does the actual plotting
#' in the case of a binary plot

plotDiagram_json_ternary <- function(graphDef,dd,lbl,
                                     template_options,template_colors){

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
  parsedTemplate <- parse_template(graphDef,template_options,template_colors)

  #### Prepare the "pseudo-axes" labels, A, B and C ####
  A=list(type="text",x=0,y=-0.03,text=annotate(parsedAxesOptions$alab),adj=0.5)
  B=list(type="text",x=0.5,y=sqrt(3)/2+.03,text=annotate(parsedAxesOptions$blab),adj=0.5)
  C=list(type="text",x=1,y=-0.03,text=annotate(parsedAxesOptions$clab),adj=0.5)

  ####Merge the elements ####
  parsedTemplate<- c(parsedTemplate,A=list(A),B=list(B),C=list(C))

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
                        template=parsedTemplate ))

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
