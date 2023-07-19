#######################################################
#
#          Parsing functions
#
#######################################################

#### Load json template ####
#' This function loads a json template and returns a list
#' @importFrom jsonlite read_json
#' @export
#'
#' @param json Name of the template file
#' @param path Path to json file
#' @details
#' If no path is supplied, the function will look in the json_template
#' directory of the package folder, and its subdirectories. It will return
#' the first match, starting by the top-level folder and exploring
#' sub-folders aphabetically.
#'
json_loader<-function(json,path=NULL){

  ## If no path is defined, look for the template in template dir & sub-dir
  if(is.null(path)){
    toc <- list.dirs(system.file("json_templates",package="jsonGraphTemplates"))
    for(i in (1:length(toc) )){
      ii <- paste(toc[i],json,sep="/")
      if(file.exists(ii)){ thejson <- ii; break() }
    }

  }else{
    thejson<-paste(path,json,sep="/")
  }

  graphDef<-jsonlite::read_json(thejson,simplifyVector = T)

  # Here we can do some syntaxic check
 if( !is.null(graphDef$optionDefaults)){
   if( sort(names(graphDef$optionDefaults)) != sort(names(graphDef$optionSwitches)) ){
         stop("Error in json template:\n
           all switches MUST have defaults")
     }
   }

  return(graphDef)
}

#### Show available template switches ####
#' Indicate the available options for this diagram
#' @export
#'
#' @param graphDef graph definition object, loaded from json
#'
show_switches<-function(graphDef){
    print(graphDef$optionSwitches)
}

#### Parse template ####
#' Mostly, a wrapper to parse individual template elements
#' Works with template_element_parser
#' @export
#'
#' @param graphDef Graph definition list, loaded by json_loader
#' @param template_options List of all the options used in template, i.e. must match
#' template's "optionDefaults": field
#' @param color_options See plotDiagram_json
parse_template<-function(graphDef,template_options=NULL,color_options=NULL){

  if(is.null(graphDef$template)){
    template_nice <- NULL
  }else{
    template_nice<-lapply(graphDef$template,
                          function(z){
                          template_element_parser(z,
                                                  template_options = template_options,
                                                  default_options = graphDef$optionDefaults,
                                                  color_options = color_options)
                         })
  }
  # Remove empty elements
  template_nice <- Filter(Negate(is.null),template_nice)

  # In the case of an empty template, return one dummy element to make Figaro happy
  if(length(template_nice)==0){
    template_nice <- list(nothing=list(plotFun=""))
    }

  return(template_nice)
}

#### Template prettyfier ####
#' This function looks at individual template elements,
#' and modifies them as desired
#' @param tpl_el A template element
#' @param default_options Switch options, by default
#' @param template_options See plotDiagram_json, parse_template
#' @param color_options See plotDiagram_json
template_element_parser<-function(tpl_el,default_options,
                                  template_options,color_options){

#### Conditional display ####
  if(any(names(tpl_el) == "switch")){

    # If there are no defaults, complain loudly !
    if(  !all(tpl_el$switch %in% names(default_options) ) ){
       stop("If you use switches, they must be defined !")
    }

    # At that point we can rely on all options being defined in template_options
    if( all(template_options[tpl_el$switch]) ){
      # remove switch if all true, keep the element
      tpl_el$switch <- NULL
    }else{
      # else drop the element and exit the function
      tpl_el <- NULL; return(tpl_el)
    }
  }

#### Convert colors ####
  if(any(names(tpl_el) == "col")){
    # This element has a color definition, let's dig further
    if(!isColor(tpl_el$col)){
      # If the color is a legitimate colour name, don't touch it.
      # However, if it is not...
      if(any(names(color_options) == tpl_el$col)){
        # The user has supplied an equivalence
        tpl_el$col <- color_options[tpl_el$col]
      }else{
        # Default
        tpl_el$col <- "black"
      }
    }
  }

#### Text defined as expressions - parse it!
  if(tpl_el$plotFun == "text"){
    if( grepl("expression",tpl_el$text) ){
      tpl_el$text <- eval(parse(text = tpl_el$text) )
    }
  }

  return(tpl_el)
}

#### Custom axes names ####
#' Ancillary function to get proper labels for axes, if supplied
#' @param graphDef A graph definition, loaded from json
#' @param W Axis to process
#' /!\ uses GCDkit !! Should maybe move to Figaro
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
#' /!\ uses GCDkit !! Should maybe move to Figaro
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
#' /!\ uses GCDkit !! Should maybe move to Figaro
axes_parser_ternary<-function(graphDef){

  alab <- .make.names(graphDef,"A")
  blab <- .make.names(graphDef,"B")
  clab <- .make.names(graphDef,"C")

  return(list(alab = alab,
              blab=blab,
              clab=clab))
}


#### Data transformation ####
#' This function transforms the data using the function required
#'
#' @export
#'
#' @param graphDef A graph definition, loaded from json
#' @param wrdata WR data (typically WR in GCDkit, or equivalent).
#' This is a matrix, as per GCDkit convention, but is actually
#' useful for ggplot as well as it will allow coordinate mapping for instance!
#' If used in ggplot, convert back to tibble.
#' @param transform_options Options to pass to the transformation function
#' @details the transform fn may, or may not be GCDkit. Obviously if it is,
#' this depends on GCDkit...
data_transformation<-function(graphDef,wrdata,transform_options){

  if(is.null(graphDef$dataTransform)){
      dd<-wrdata
    }else{
      trfFunction<-get(graphDef$dataTransform)
      trfParams <- graphDef$dataTransformParams

      args <- c(list(wrdata),trfParams,transform_options)
      dd <- do.call(trfFunction,args=args )
    }
    return(dd)
}

#### Calculate the coordinates of any point from a well-conformed data matrix ####
#' Calculate the x.data, y.data, a.data etc for any dataset, following template rules
#' This should include data transformation, and also filtering (if doFilter = T,
#' and of course if the template includes a filter).
#' This function should probably be made figaro-specific...#####
#' /!\ uses GCDkit !! Should maybe move to Figaro
#' @export
#' @param graphDef A graph definition, loaded from json
#' @param wrdata WR data (typically WR in GCDkit, or equivalent)
#' @param lbl label data (typically labels in GCDkit, or equivalent)
#' @param transform_options Options passed to the data transformation function
#' @param doFilter Boolean, should the data be filtered according to template rule?
#'
#'
points_coordinates<-function(graphDef,wrdata,lbl,transform_options=NULL,doFilter=T){

  # protect against odd things happening
  if(!(graphDef$diagramType%in%c("binary","ternary") ) ){
    msg <- paste("Sorry, cannot work on graph of type",graphDef$diagramType,"\n",sep=" ")
    stop(msg)
  }

  # Calculate the actual plot data
    wrdata<-data_transformation(graphDef,wrdata,transform_options)

  # Filter, if needed
  if(doFilter&&!is.null(graphDef$dataFilter)){
    selected <- GCDkit::selectSubset(what=graphDef$dataFilter,
                                     where=cbind(lbl,wrdata),
                                     all.nomatch=F,
                                     save=F)
    if(selected==""){stop("No data to plot matching criteria")}
    dd <- wrdata[selected,,drop=F]
    lbl <- lbl[selected,,drop=F]
  }

  if(graphDef$diagramType == "binary"){
    x.data <- GCDkit::calcCore(graphDef$axesDefinition$X,where="wrdata",redo=F)$results
    y.data <- GCDkit::calcCore(graphDef$axesDefinition$Y,where="wrdata",redo=F)$results

    retVal <- cbind(x.data,y.data)
  }

  if(graphDef$diagramType == "ternary"){
    # Get the A, B and C data (apices)
    a.data <- GCDkit::calcCore(graphDef$axesDefinition$A,where="wrdata",redo=F)$results
    b.data <- GCDkit::calcCore(graphDef$axesDefinition$B,where="wrdata",redo=F)$results
    c.data <- GCDkit::calcCore(graphDef$axesDefinition$C,where="wrdata",redo=F)$results

    # Convert to X and Y
    sum_apices <- a.data+b.data+c.data

    x.data <- ((c.data/sum_apices) + (b.data / sum_apices) /2)
    y.data <- (sqrt(3)*(b.data / sum_apices)/2)

    retVal <- cbind(x.data,y.data,a.data,b.data,c.data)
  }

  return(list(wrdata = retVal,
              lbl = lbl))

}
