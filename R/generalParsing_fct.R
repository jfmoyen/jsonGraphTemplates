#######################################################
#
#          Parsing functions
#
#######################################################

#### Load json template ####
#' This function loads a json template and returns a list
#' @importFrom jsonlite read_json
#'
#' @param json Name of the template file
#' @param path Path to json file
json_loader<-function(json,path=NULL){

  if(is.null(path)){
    thejson <- system.file("json_templates",json,
                           package="jsonGraphTemplates")
  }else{
    thejson<-paste(path,json,sep="/")
  }

  graphDef<-jsonlite::read_json(thejson,simplifyVector = T)

  return(graphDef)
}

#### Show available template switches ####
#' Indicate the available options for this diagram
#'
#' @param graphDef graph definition object, loaded from json
show_switches<-function(graphDef){
    print(graphDef$optionSwitches)
}

#### Parse template ####
#' Mostly, a wrapper to parse individual template elements
#' Works with template_element_parser
#' @param json Name of the template file
#' @param path Path to json file
parse_template<-function(graphDef,template_options,template_colors){

  if(is.null(graphDef$template)){
    template_nice <- NULL
  }else{
    template_nice<-lapply(graphDef$template,
                          function(z){
                          template_element_parser(z,template_options,template_colors)
                         })
  }
  # Remove empty elements
  template_nice <- Filter(Negate(is.null),template_nice)

  return(template_nice)
}

#### Template prettyfier ####
#' This function looks at individual template elements,
#' and modifies them as desired
#' @param tpl_el A template element
template_element_parser<-function(tpl_el,template_options,template_colors){

  ## Elements that have a switch:
  if(any(names(tpl_el) == "switch")){
    # if the corresponding option is not set, remove switch (and plot the element)
    if( is.null(template_options[[tpl_el$switch]]) ){
      tpl_el$switch <- NULL
    }else{
      # the option is set explicitely
      if(template_options[[tpl_el$switch]] ){
        # .. and is TRUE, remove switch
        tpl_el$switch <- NULL
      }else{
        #... and is FALSE, remove element
        tpl_el <- NULL
      }
    }
}

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
#' if a filter condition was supplied; and by transforming it, if needed.
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

