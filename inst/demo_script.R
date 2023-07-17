### Example of usage

library(GCDkit)
library(jsonGraphTemplates)

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

## We can break the diagram in useful(?) components
# Load into R
graph_definition<-json_loader("QANOR.json")

# The template itself
graph_template<-parse_template(graph_definition,
                               template_colors = c(pltcol1="blue",pltcol2="darkblue",pltcol3='red'))

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
graph_template<-parse_template(graph_definition)

# Coordinates of arbitrary points in his diagram
blatna_LaRochePlut_coords <- points_coordinates(graph_definition,wrdata=WR.blatna,lbl=lbl.blatna)

# and so...
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LaRochePlut.json",new=F)
points(x=blatna_LaRochePlut_coords$wrdata[,"x.data"],
       y=blatna_LaRochePlut_coords$wrdata[,"y.data"],
       pch=lbl.blatna$Symbol,col=lbl.blatna$Colour)

# Or maybe even...
graph_definition<-json_loader("OConnorPlut.json")

# The template itself
graph_template<-parse_template(graph_definition)

# Coordinates of arbitrary points in his diagram
blatna_OConnorPlut_coords <- points_coordinates(graph_definition,wrdata=WR.blatna,lbl=lbl.blatna)

# and so...
plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="OConnorPlut.json",new=F)
points(x=blatna_OConnorPlut_coords$wrdata[,"x.data"],
       y=blatna_OConnorPlut_coords$wrdata[,"y.data"],
       pch=lbl.blatna$Symbol,col=lbl.blatna$Colour)


