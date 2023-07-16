library(GCDkit)
library(jsonGraphTemplates)

 data("atacazo")
 accessVar("atacazo")
 WR.ata <- WR
 lbl.ata <- labels

 data("blatna")
 accessVar("blatna")
 plotDiagram_json(json="QANOR.json")

 plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="QANOR.json")
 plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="LarochePlut.json")


##### Binary diagrams ######

plotDiagram_json(json="Sylvester.json")

plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="Sylvester.json")

 plotDiagram("Batchelor",F)
#jsonPlotDiagram(path="D:","test.json")

 ##### Ternary diagrams ######

 plotDiagram_json("OConnorVolc.json")

 plotDiagram("OConnorPlut",F)



