
\name{json format to describe diagram templates}
\alias{jsonDiagramFormat}
\title{Recognized json tag in diagram descriptions}

\description{
Diagram templates are defined using json format. json follows a tag:value convention.}

\details{The functions from jsonGraphTemplates understand the following tags. Note, by the way, that json format does NOT permit comments - if you really need to, you can add somehing like "_comment":"blabla" (it will be loaded, but probably dropped by the parser)}

\section{Mandatory}{
  \itemize{
  The following items are mandatory and must be present in the template
    \item{"diagramType": }{One of "binary" or "ternary". Describes the geometry, and overall behviour of the diagram.}
    \item{"axesDefinition": }{Axes mapping. In a binary diagrams, axes are X and Y. In a ternary, apices are A, B and C. This should be either variables from the dataset, or vaues that can be calculated from them. In GCDkit they are calculated using calcCore, in ggplot using data-masking. In base R, only natural variables can be used; you can always calcualte in advance however.}
    \item{"limits": }{a list of two, X and Y, giving the range on each axis}
  }
}

\section{Optional}{

These items can be included to customize further the template
\subsection{metadata}{
\itemize{
\item{"name": }{Short name of the diagram, normally the GCDkit function it originates from}
\item{"fullName": }{Long version of the name, will be printed as diagram title}
\item{"details": }{If needed. Probably a more clever place to put comments if required}
\item{"reference": }{Original publication}
\item{"url": }{Original URL, DOI, etc.}
\item{"templateAuthor": }{Name and contact of the person who wrote the template file (not the original diagram)}
\item{"templateHistory": }{Further comments on the template itself}
}}


\subsection{Data mapping}{
\itemize{
\item{"dataTransform": }{null, or a function of wrdata that returns a data matrix. If null, the current data will be used. Otherwise code{dataTransform(WR)} will be used.}
\item{"dataMultiply": }{null, or a number by which the X and Y values will be multiplied before plotting (useful to convert values 0-1 in pct for instance)}
\item{"dataFilter": }{null, or a string corresponding to a boolean filter, used by GCDkit::selectSamples or dplyr::filter for instance}
\item{"axesName": }{The name of the axes, as will be printed on the graph. Defaults to axesDefintion.}
\item{"suppressAxes": }{If present (and true), the axes will not be drawn. Neded for ternary plots, and some special plots where custom axes are required (and should be defined in the template)}
\item{"log": }{If present (and other than ""), defines whoch axes use a log scale using base R convention : "x", "y" or "xy"}
}
}
}

\section{Template}{
The template proper is a list of potentially many elements. They all belong to a "template": element, itself a dictionary of the form "element_name":"description". Each template element starts with a "type" element that defines how it will be interpreted by the parser. The following types are understood:
\itemize{
\item{"type":"NULL" }{Elements of type "NULL" won't be plotted on the template. Their main use is for classification fields. Classifications fields are defined by a combination of template elements:
\itemize{
\item{"clssf": }{an element named clssf, having "type":"null" and containing "use","rcname", and a number of elements defining the classification polygons. "use" gives the numbers of the template elements that can define classification polygons. "rcname" gives the resulting classification names, and the subsequent elements are polygon coordinates. Note that the polygon coordinates can be of type "NULL" (they will not be plotted), or "line" (they will be drawn)}
}}
\item{"type":"lines" }{lines have x, y and possibly col (colour) and lty (line type) properties.}
\item{""}{}
\item{""}{}
}
}

