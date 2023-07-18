
\name{json format to describe diagram templates}
\alias{jsonDiagramFormat}
\title{Recognized json tag in diagram descriptions}

\description{
Diagram templates are defined using json format. json follows a tag:value convention.}

\details{The functions from jsonGraphTemplates understand the following tags. Note, by the way, that json format does NOT permit comments - if you really need to, you can add somehing like "_comment":"blabla" (it will be loaded, but probably dropped by the parser)}

\section{Mandatory}{
  \itemize{
  The following items are mandatory and must be present in the template
  \item{"Rdialect:" }{Indicates which "dialect" the template requires: plain R (\code{base}) or \code{GCDkit} ? }
    \item{"diagramType": }{One of "binary", "ternary" or "plate". Describes the geometry, and overall behviour of the diagram. Plates are a collections of simpler diagram and the parser should be able to deal with them.}
    \item{"axesDefinition": }{Axes mapping. In a binary diagrams, axes are X and Y. In a ternary, apices are A, B and C. This should be either variables from the dataset, or vaues that can be calculated from them. In GCDkit they are calculated using calcCore, in ggplot using data-masking. In base R, only natural variables can be used; you can always calcualte in advance however. Do NOT include corresponding template elements in the file (even for ternary), they are automatically created by the parser.}
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

\subsection{Data transformation}{
\itemize{
\item{"dataTransform": }{null, or a function of wrdata that returns a data matrix. If null, the current data will be used. Otherwise code{dataTransform(WR)} will be used. If a data transform function exists, it is possible to pass arguments to it using, in the parser, transform_options = list(...) }
\item{"dataTransformParams": }{Parameters to be passed to the data transform function (always). Typically things such as verbose = F ... }
}}

\subsection{Data mapping}{
\itemize{
\item{"dataFilter": }{null, or a string corresponding to a boolean filter, used by GCDkit::selectSamples or dplyr::filter for instance}
\item{"axesName": }{The name of the axes, as will be printed on the graph. Defaults to axesDefintion. They are X and Y for binary, A, B and C for ternary. Do NOT include corresponding template elements in the file, they are automatically created by the parser.}
\item{"suppressAxes": }{If present (and true), the axes will not be drawn. Neded for ternary plots, and some special plots where custom axes are required (and should be defined in the template)}
\item{"log": }{If present (and other than ""), defines whoch axes use a log scale using base R convention : "x", "y" or "xy"}
}
}
}

\section{Template}{
The template proper is a list of potentially many elements. They all belong to a "template": element, itself a dictionary of the form "element_name":"description". Each template element starts with a "type" element that defines how it will be interpreted by the parser. The following types are understood:
\itemize{
\item{"plotFun":"NULL" }{Indicates which plotting function to use. Elements with plotFun "NULL" won't be plotted on the template. They can, however, be used for clasification}
\item{"type":"lines"}{Elements of type lines will draw lines. Lines are defined by x, y, possibly col, lty }
\item{"type:""test" }{ .... }
}

\subsection{Special template items}{
The parser will interpret colors and switches, allowing to configure the template:
\itemize{

\item{colors: }{The field "col" of any template element will be interpreted by the parser. If an explicit color name is supplied ("black", "blue","#123456"), it will be used. If another name is supplied, the parser will lok in the vector called color_options and replace by the appropriate values, e.g. color_options = c("textcol"="blue","linecol"="red") will replace every occurence of textcol in the template by "blue", every occurence of linecol by "red", etc. Defaults to black if nothing was supplied.

Figaro parser tries to conform to GCDkit usage. Colours names pltcol1, pltcol2 and pltcol3 in json template will be replaced by plt.col[1] etc, unless the user explicitely says otherwise. If option gcd.plot.bw is set to TRUE, these three colours will be made black, no matter what.

The above will NOT affect any colour defined otherwise in json template : "red" or "myColour" won't be changed by gcd.plot.bw, for instance.}

\item{switching: }{Elements can be controlled by switching. If an element has a "switch" field, this fields contains the name of the (boolean) switch that will control its appearance. An element with no "switch" will always appear.
The list of switches should be present in "optionSwitches".
The defaults MUST be present in "optionDefaults", otherwise the code throws an error.
For example:

\code{
"optionSwitches": {"plotthis": "Plot the word Tholeitic",
                    "plotthat": "Plot the diagonal line"
                  },
"optionDefaults": {"plotthis": false,
                   "plotthat": true}
}
}
}
}

\subsection{classification}{
If an element is named clssf, it is used to define classification polygons, which may, or may not be plotted. clssf must have the following fields:
\itemize{
\item{"use": }{The numbers of the template elements to use for classification, in the order in which they appear in the template.}
\item{"rcname": }{The resulting classification names}
}

Classification requires closed polygons, so any subsequent definition must loop back to origin, like that:

\code{
 "lines1": {"type": "NULL",
            "x": [0,435,0,0],
            "y": [0,500,500,0]
        },
}

This specific "lines" will not be plotted (type is NULL).

As clssf explicitely refers to the item numbers, it would be very unwise to reorder them or remove some of them; in particular do NOT switch them !

For example:
\code{
 "clssf": {"type": "NULL",
            "use": [2,3,4,5,6,7],
            "rcname": ["I","II","III","IV","V","VI"]
        },
  "lines1": { ... },
  "lines2": { ... },
  ...
  "lines6": { ... },
  "lines7": { ... }, (etc)
}

will use elements lines1 to lines6 for classification (but not lines7 and subsequent)
}
}


