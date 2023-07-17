test_that("All diagrams plot as they should", {
  # This is not a very good test as it needs some data loaded...
  # data("blatna",package="GCDkit")
  # GCDkit::accessVar(blatna)

  expect_error({
    templ_dir <- system.file("json_templates",package="jsonGraphTemplates")
    templ_list <- list.files(templ_dir)
    sapply(templ_list,
           function(thediag){plotDiagram_json(json=thediag,
                                              wrdata=WR,lbl=labels,verbose=T)} )
  },
  NA)
})
