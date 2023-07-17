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

plotDiagram_json(json="Batchelor.json")

plotDiagram_json(wrdata=WR.ata,lbl=lbl.ata,json="Sylvester.json")

 plotDiagram("Batchelor",F)
#jsonPlotDiagram(path="D:","test.json")

 ##### Ternary diagrams ######

 plotDiagram_json(json="AFM.json")

 plotDiagram("OConnor",F)


#########

 templ_dir <- system.file("json_templates",package="jsonGraphTemplates")
 templ_list <- list.files(templ_dir)
 sapply(templ_list,
        function(json){
          thejson <- system.file("json_templates",json,
                                 package="jsonGraphTemplates")
          graphDef<-jsonlite::read_json(thejson,simplifyVector = T)
          if(graphDef$diagramType=="ternary"){cat(json,"is ternary\n")}
 })

###########

 plotDiagram_json("test2.json",new=F)
 plotDiagram_json("test2.json",new=F,template_options = "")
 plotDiagram_json("test2.json",new=F,template_options = NULL)
 plotDiagram_json("test2.json",new=F,template_options = c("plotthat"=FALSE))
 plotDiagram_json("test2.json",new=F,template_options = c("plotthat"=TRUE))
 plotDiagram_json("test2.json",new=F,template_options = c("plotthis"=FALSE))
 plotDiagram_json("test2.json",new=F,template_options = c("plotthis"=TRUE))
 plotDiagram_json("test2.json",new=F,template_options = c("plotthat"=FALSE, "plotthis"=FALSE))
 plotDiagram_json("test2.json",new=F,template_options = c("plotthat"=FALSE, "plotthis"=TRUE))
 plotDiagram_json("test2.json",new=F,template_options = c("plotthat"=TRUE, "plotthis"=FALSE))
 plotDiagram_json("test2.json",new=F,template_options = c("plotthat"=TRUE, "plotthis"=TRUE))

