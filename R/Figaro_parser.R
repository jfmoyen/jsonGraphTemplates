#######################################################
#
#          Figaro converter
#
#######################################################

### This is to avoid check() notes when using global variables
# or unquoted arguments in dplyr/ggplot
utils::globalVariables(c("demo","sheet","x.data","y.data","WR","mw","plt.col"))


##NB There is deliberately no import from GCDkit to avoid creating a dependency !

############### Plot json template in Figaro ####################
#' Read a template defined in a json file and import it to  figaro graph
#' @export
#'
#' @param wrdata A matrix containing WR data. Probably GCDkit's WR,
#' as in many cases we need GCDkit-calculated things such as A/CNK etc.
#' @param lbl A data frame containing gCDkit labels.
#' @param json Name of the template file
#' @param path Path to json file
#' If not specified, looks in the json_template folder of the package
#' @param verbose Give debugging info
#' @param new Open in a new window?
#' @param template_options Options passed to the parser, as named vector
#' They are of the form c("switch1" = T, "switch2" = F, etc)
#' and they will affect template elements that have a switch.
#' @param color_options A named vector, with names corresponding to the colours
#' found in the template, e.g. c(col1="red",col2="blue"). Defaults to black.
#' @param transform_options Further options to be passed to data transformation function
#' @details
#' This function reads a json template and plots a Figaro graph
#'  (figaro is GCDkit's internal plotting system, that allows
#'  a level of editing and interactivity). It can NOT work if GCDkit
#'  is not installed. This is the original function, intended as a
#'  replacement for GDkit::plotDiagram.
#'
#'  This function will honour gCDkit global options, UNLESS the user overwrites them.
#'
#'  Optional text (controlled by "switch": "showText" in the template): If the user specifies
#'  the option in the call, with template_options=c("showText"=T) for instance,
#'  the user choice will be followed. Else, the function will use options("gcd.plot.text").
#'
#'  Likewise, if the user supplies colours for pltcol1, pltcol2 and pltcol3 they will be honoured.
#'  Otherwise, the system will use (1) black, if options("gcd.plot.bw") is true or
#'  (2) the content of plt.col[] if false.
#'

plotDiagram_json <- function(json, path = NULL,
                             wrdata=WR,lbl=labels,
                             verbose=F,new=T,
                             template_options=NULL,
                             color_options=NULL,
                             transform_options=NULL){

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

  #### GCDkit showText option ####
  # If not user-supplied, we use GCDkit defaults
  # Create a vector with all options defined in this template
  topt <- unlist(graphDef$optionDefaults)

  # Replace showText by GCDkit's definition
  topt["showText"] <- getOption("gcd.plot.text")

  # ... and, again, by user-definition, if any
  nm <- names(template_options)
  topt[nm] <- template_options[nm]

  template_options <- topt

  #### GCDkit color defaults ####
  # if the user did not specify them, use GCDkit defaults
  # NB in non-GCDkit parsers, plt.col should have sensible defaults !
  if(is.null(color_options)){

    if(getOption("gcd.plot.bw")){
      color_options<-c(
        "pltcol1" = "black",
        "pltcol2" = "black",
        "pltcol3" = "black"
      )
    }else{
      color_options<-c(
        "pltcol1" = plt.col[1],
        "pltcol2" = plt.col[2],
        "pltcol3" = plt.col[3]
      )
    }
  }
  #### Main switch - what are we trying to plot ? ####
  switch(EXPR = graphDef$diagramType,
         "binary" = pp<-plotDiagram_json_binary(graphDef,wrdata,lbl,new=new,template_options,color_options,transform_options),
         "ternary" = pp<-plotDiagram_json_ternary(graphDef,wrdata,lbl,new=new,template_options,color_options,transform_options),
         "plate" = pp<-plotDiagram_json_plate(graphDef,wrdata,lbl,template_options,color_options),
         stop(paste("Sorry, plotting of type",graphDef$diagramType,"is not implemented yet",sep=" "))
  )

  invisible(pp)
}

############### Plot json template in Figaro: BINARY ####################
#' Inner function, for binary plots
#'
#' @param graphDef The template, loaded into a list from json
#' @param wrdata The plotting dataset
#' @param lbl The labels (GCDkit style)
#' @param new Open in a new window?
#' @param template_options See plotDiagram_json
#' @param color_options See plotDiagram_json
#' @param transform_options See plotDiagram_json
#' @details
#' Internal function, that does the actual plotting
#' in the case of a binary plot

plotDiagram_json_binary <- function(graphDef,wrdata, lbl,
                                    new,
                                    template_options,color_options,transform_options){

  #### Prepare the data ####
  preparedData <-points_coordinates(graphDef,wrdata,lbl,transform_options,doFilter=T)

   # Get the X and Y values
  x.data <- preparedData$wrdata[,"x.data"]
  y.data <- preparedData$wrdata[,"y.data"]

  ##### Optional tags #####
  # Parse axes labels, axes suppression and log scale
  parsedAxesOptions <- axes_parser_binary(graphDef)

  #### Parse the template proper #####
  parsedTemplate <- parse_template(graphDef,template_options,color_options)

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
                                  axes=parsedAxesOptions$axes,
                                  new=new),
                        template=parsedTemplate))

  # Assign to global env
  assign("sheet", sheet, .GlobalEnv)
  assign("x.data", x.data, .GlobalEnv)
  assign("y.data", y.data, .GlobalEnv)

  #### Create the actual figaro object and plot ####
  pp <- GCDkit::figaro(demo, prefix = "sheet")
  # figRedraw()

  pp$draw(x.data, y.data,
          main=GCDkit::annotate(graphDef$fullName),
          xlab=parsedAxesOptions$xlab,
          ylab=parsedAxesOptions$ylab,
          col = subset(lbl, rownames(wrdata) %in% names(x.data),
                       "Colour",
                       drop = TRUE),
          pch = subset(lbl, rownames(wrdata) %in% names(x.data),
                       "Symbol",
                       drop = TRUE),
          cex = subset(lbl, rownames(wrdata) %in% names(x.data),
                       "Size",
                       drop = TRUE),
          plotting.function = "fromJSON",
          new = new
  )

  invisible(pp)
}

############### Plot json template in Figaro: TERNARY ####################
#' Inner function, for binary plots
#'
#' @param graphDef The template, loaded into a list from json
#' @param wrdata The plotting dataset
#' @param lbl The labels (GCDkit style)
#' @param new Open in a new window?
#' @param template_options See plotDiagram_json
#' @param color_options See plotDiagram_json
#' @param transform_options See plotDiagram_json
#' @details
#' Internal function, that does the actual plotting
#' in the case of a ternary plot

plotDiagram_json_ternary <- function(graphDef,wrdata,lbl,new,
                                     template_options,color_options,transform_options){

  #### Prepare the data ####
  preparedData <-points_coordinates(graphDef,wrdata,lbl,transform_options,doFilter=T)

  # Get the X and Y values
  x.data <- preparedData$wrdata[,"x.data"]
  y.data <- preparedData$wrdata[,"y.data"]

  ##### Optional tags #####
  # Parse axes labels, axes suppression and log scale
  parsedAxesOptions <- axes_parser_ternary(graphDef)

  #### Parse the template proper #####
  parsedTemplate <- parse_template(graphDef,template_options,color_options)

  #### Prepare the "pseudo-axes" labels, A, B and C ####
  A=list(type="text",x=0,y=-0.03,text=GCDkit::annotate(parsedAxesOptions$alab),adj=0.5)
  B=list(type="text",x=0.5,y=sqrt(3)/2+.03,text=GCDkit::annotate(parsedAxesOptions$blab),adj=0.5)
  C=list(type="text",x=1,y=-0.03,text=GCDkit::annotate(parsedAxesOptions$clab),adj=0.5)

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
                                  axes=FALSE,
                                  new=new),
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
          col = subset(lbl, rownames(wrdata) %in% names(x.data),
                       "Colour",
                       drop = TRUE),
          pch = subset(lbl, rownames(wrdata) %in% names(x.data),
                       "Symbol",
                       drop = TRUE),
          cex = subset(lbl, rownames(wrdata) %in% names(x.data),
                       "Size",
                       drop = TRUE),
          plotting.function = "fromJSON",
          new = new
  )

  invisible(pp)
}

############### Plot json template in Figaro: PLATES ####################
#' Inner function, for plates plots (deprecated)
#'
#' @param graphDef The template, loaded into a list from json
#' @param dd The plotting dataset
#' @param lbl The labels (GCDkit style)
#' @param template_options Further arguments passed to the parser, see main function
#' @details
#' Internal function, that does the actual plotting
#' in the case of a plate. This function is not very nicely written, it relies on
#' multiplePerPage and hopes it does the right thing. As multiplePerPage
#' uses command defined as texts, well...

# plotDiagram_json_plate_DEPRECATED <- function(graphDef,dd, lbl,
#                                     template_options,color_options){
#
# ## Not nice, there must be a better way to do this !
#   assign("dd", dd, .GlobalEnv)
#   assign("lbl", lbl, .GlobalEnv)
#
# ## multiplePerPage uses commands as strings, so we have to assemble them (! ...)
#   pltCommand <- paste("plotDiagram_json(json='",
#                       graphDef$plateSlots,
#                       "',wrdata=dd,lbl=lbl,",
#                       "new=F,",
#                       "template_options=template_options,",
#                       "color_options=color_options",")",sep="")
#
#   GCDkit::multiplePerPage(pltCommand,
#                           nrow = graphDef$nrow,
#                           ncol = graphDef$ncol,
#                           title= graphDef$fullName)
#
# ## Cleanup
#   remove("dd", envir= .GlobalEnv)
#   remove("lbl", envir = .GlobalEnv)
#
# }

############### Plot json template in Figaro: PLATES ####################
#' Inner function, for binary plots
#' @importFrom graphics mtext screen par
#' @importFrom grDevices n2mfrow
#'
#' @param graphDef The template, loaded into a list from json
#' @param dd The plotting dataset
#' @param lbl The labels (GCDkit style)
#' @param template_options Further arguments passed to the parser, see main function
#' @param color_options Further arguments passed to the parser, see main function
#' @details
#' Internal function, that does the actual plotting
#' in the case of a plate.
#' This is mostly code from multiplePerPage()
#' All this function does is call recursively plotDiagram_json to fill the plate slots.

plotDiagram_json_plate <- function(graphDef,dd, lbl,
                                     template_options,color_options){

  ## If something was already there, preserve it
  sheet.bak <- sheet
  x.data.bak <- x.data
  y.data.bak <- y.data

  ## Geometry of the plate - rows, columns
  ncol <- graphDef$ncol
  nrow <- graphDef$nrow

  nbslots <- length(graphDef$plateSlots)

  if(nrow==F||ncol==F){
    ncol <- grDevices::n2mfrow(nbslots)[1]
    nrow <- grDevices::n2mfrow(nbslots)[2]
  }

  ## Create the plate itself
  plate <- GCDkit::.plateSetup(nbslots, nrow, ncol, title = graphDef$fullName)

    ## Prepare the data structure, empty so far
  plate.data <- as.list(1:nbslots)
  plate.data <- lapply(1:nbslots, function(i) {
    plate.data[[i]] <- list(x = 1, y = 1)
  })
  names(plate.data) <- paste("Fig", 1:nbslots, sep = "")

  ## Make global
  assign("plate", plate, .GlobalEnv)
  assign("plate.data", plate.data, .GlobalEnv)

  ## Graphic setup and title
  graphics::par(oma = c(0, 0, 4, 0))
  graphics::mtext(text = GCDkit::annotate(plate$title), side = 3, line = 0.25,
        outer = TRUE, cex = 1.5)

    ## Construct every individual plot
  ee <- lapply(1:nbslots, function(i) {
    graphics::screen(i, new = FALSE)

    ## Geometric considerations
    if (.Platform$OS.type == "windows" & .Platform$GUI ==
        "Rgui") {
      graphics::par(mar = c(4.5, 5.5, 2, 1.5))
      graphics::par(pty = "s")
    }
    else {
      graphics::par(mar = c(2, 0.5, 1, 1))
      graphics::par(pty = "s")
    }

    ## The actual plot
      plotDiagram_json(json=graphDef$plateSlots[i],
                       wrdata=dd,lbl=lbl,
                       new=F,
                       template_options=template_options,
                       color_options=color_options)
      GCDkit::.saveCurPlotDef(i)
    })

  ## Restore the global environment the way it was
  assign("sheet", sheet.bak, .GlobalEnv)
  assign("x.data", x.data.bak, .GlobalEnv)
  assign("y.data", y.data.bak, .GlobalEnv)

  ## Final touch
  assign("scr.old", 1, .GlobalEnv)
  graphics::screen(1, new = FALSE)
  if (.Platform$OS.type == "windows" & .Platform$GUI == "Rgui")
    GCDkit::.menuPopUp()
  graphics::screen(1, new = FALSE)

}
