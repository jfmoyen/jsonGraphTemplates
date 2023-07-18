### Example of usage

library(GCDkit)
library(jsonGraphTemplates)

# Starting from a known point...
options(gcd.plot.bw = F)
options(gcd.plot.text = T)
plt.col <- c("blue","gray8","darkblue")

# Load two datasets
data("atacazo")
accessVar("atacazo")
WR.ata <- WR
lbl.ata <- labels

data("blatna")
accessVar("blatna")
WR.blatna <- WR
lbl.blatna <- labels

# plot one of them
plotDiagram_json(json="QANOR.json")

# or, if you want,
plotDiagram_json(json="QANOR.json",new=F)

# plot the other dataset
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="QANOR.json")
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LarochePlut.json")

#### Color options ####

# We can define manually some of them, such as
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LarochePlut.json",
                 color_options=c(pltcol2="purple"))

# If they are not defined, they default to plt.col
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LarochePlut.json")

# If we change plt.col...
plt.col <- c("blue","orange","darkblue")
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LarochePlut.json")

# Global options are honoured:
options(gcd.plot.bw = T)
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LarochePlut.json")

# ... until we explicitely say otherwise !
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LarochePlut.json",
                 color_options=c(pltcol2="purple"))

# Clean our mess...
options(gcd.plot.bw = F)
options(gcd.plot.text = T)
plt.col <- c("blue","gray8","darkblue")

#### Text options ####
# Likewise, for text display
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="TAS.json")
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="TAS.json",
                 template_options=c("showText"=F))

# In the same way, GCDkit global option will be followed,
# unless you explicitely say otherwise
options(gcd.plot.text = F)
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="TAS.json")
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="TAS.json",
                 template_options=c("showText"=T))


# Clean our mess...
options(gcd.plot.bw = F)
options(gcd.plot.text = T)
plt.col <- c("blue","gray8","darkblue")

##### Data transformation functions can now take extra arguments ####
#Default
plotDiagram_json("optionsDemo.json")

plotDiagram_json("optionsDemo.json",transform_options=c("doubleBB"=F))
plotDiagram_json("optionsDemo.json",transform_options=c("doubleBB"=T))

#### More advanced use ####
## We can break the diagram in useful(?) components
# Load into R
graph_definition<-json_loader("QANOR.json")

# The template itself
graph_template<-parse_template(graph_definition,
                               color_options = c(pltcol1="blue",pltcol2="darkblue",pltcol3='red'))

# Coordinates of arbitrary points in his diagram
blatna_QANOR_coords <- points_coordinates(graph_definition,wrdata=WR.blatna,lbl=lbl.blatna)

# and so...
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="QANOR.json")
points(x=blatna_QANOR_coords$wrdata[,"x.data"],
       y=blatna_QANOR_coords$wrdata[,"y.data"],
       pch=lbl.blatna$Symbol,col=lbl.blatna$Colour)

# Another one ?
graph_definition<-json_loader("LaRochePlut.json")

# The template itself
graph_template<-parse_template(graph_definition,
                               color_options = c(pltcol1="blue",pltcol2="darkblue",pltcol3='red'))

# Coordinates of arbitrary points in his diagram
blatna_LaRochePlut_coords <- points_coordinates(graph_definition,wrdata=WR.blatna,lbl=lbl.blatna)

# and so...
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LaRochePlut.json")
points(x=blatna_LaRochePlut_coords$wrdata[,"x.data"],
       y=blatna_LaRochePlut_coords$wrdata[,"y.data"],
       pch=lbl.blatna$Symbol,col=lbl.blatna$Colour)

# Or maybe even...
graph_definition<-json_loader("OConnorPlut.json")

# The template itself
graph_template<-parse_template(graph_definition,
                               color_options = c(pltcol1="blue",pltcol2="darkblue",pltcol3='red'))


# Coordinates of arbitrary points in his diagram
blatna_OConnorPlut_coords <- points_coordinates(graph_definition,wrdata=WR.blatna,lbl=lbl.blatna)

# and so...
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="OConnorPlut.json")
points(x=blatna_OConnorPlut_coords$wrdata[,"x.data"],
       y=blatna_OConnorPlut_coords$wrdata[,"y.data"],
       pch=lbl.blatna$Symbol,col=lbl.blatna$Colour)



#### Advanced switching
# the template has
# "optionSwitches": {"plotthetext": "Plot the word Tholeitic",
#   "plottheline": "Plot the diagonal line",
#   "plottheline2": "Also required to see the line",
#   "showText": "Default option to show diagram text"
# },
# "optionDefaults": {"plotthetext":false,
#   "plottheline": false,
#   "plottheline2": true,
#   "showText": true
# },
#
# {"lines0": {
#       ...
#      "switch":["plottheline","plottheline2"]
# },
#   "text0": {
#      ...
#     "switch":["plotthetext","showText"]
#   }

options(gcd.plot.text = T)

plotDiagram_json("testSwitches.json")
# nothing specified, use defaults + gcd.plot.text - all false

plotDiagram_json("testSwitches.json",template_options = c("plotthetext"=T))
# plot the text
options(gcd.plot.text = F)
plotDiagram_json("testSwitches.json",template_options = c("plotthetext"=T))
# Don't plot the text, because of gcd option !
# You can still force it with
plotDiagram_json("testSwitches.json",template_options = c("plotthetext"=T,"showText"=T))

options(gcd.plot.text = T)
#
plotDiagram_json("testSwitches.json",template_options = c("plottheline"=T))
# plots, because the two tags controlling the line are T

plotDiagram_json("testSwitches.json",
                 template_options = c("plottheline"=T,"plottheline2"=F))
# no line !
