show_transform<-function(diag){
  graphDef<-json_loader(diag)
  print(graphDef$dataTransform)
}

templ_dir <- system.file("json_templates",package="jsonGraphTemplates")
templ_list <- list.files(templ_dir,recursive=T,include.dirs = F)

sapply(templ_list,
       function(thediag){show_transform(thediag)
                          } )

