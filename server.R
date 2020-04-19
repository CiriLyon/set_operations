

server = function(input , output , session){
  
  library(VennDiagram)
  library(shinythemes)
  library(gplots)
  library(shiny)
  library(shinyjs)
  library(openxlsx)
  library(V8)
  
  jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
  
  
  
  
  
  observeEvent(input$resetData, {
    js$reset()
    excel     <<- NULL
    excelDemo <<- NULL
  })
  
  observeEvent(input$boutonUpload ,{
    
    if(exists("excelDemo")){
      if(!is.null(excelDemo)){shinyjs::info("You are currently working with a Demo dataset, 
Press the button 'Reset to start new analysis' 
before uploading your own dataset. ")
        return()}}
    
    
    if(exists("excel")){
      if(!is.null(excel)){shinyjs::info("You have already uploaded a dataset, 
Press the button 'Reset to start new analysis' 
before uploading a new dataset.")
        return()
      }}
    
    #______________________________________________________________________________ 
    #-- supprimer les fichier 'log' générés par la fonction 
    file.remove(dir()[grep('.log$|.zip$', dir())])
    #______________________________________________________________________________
    
    #_______________________________
    monDoc           <- input$boutonUpload
    
    if(is.null(monDoc)) {
      return()
    }
    
    
    
    monDocPath       <- monDoc$datapath #chemin + nom temporaires du fichier upload? assign? par l'ordi.
    
    monDocName       <- monDoc$name #nom du fichier upload? tel qu'il existe naturellement.
    
    trueNames        <- file.path(dirname(monDocPath), monDocName) #chemin + nom naturel
    
    file.copy(from = monDocPath , to = trueNames)
    
    if(!exists("PathLecture")){
      PathLecture      <- paste0(dirname(monDoc$datapath) , '/', monDoc$name)
      
      file.rename(from =  monDocPath  , to = PathLecture)
    }
    
    #output$fpath    <- renderText(PathLecture)
    excel            <<- read.xlsx(PathLecture , 1, na.strings = "")
    
    # dataDemo <- data.frame(Groupe1= sample(LETTERS,10, rep = F),Groupe2= sample(LETTERS,10, rep = F),
    #            Groupe3= sample(LETTERS,10, rep = F),Groupe4= sample(LETTERS,10, rep = F),Groupe5= sample(LETTERS,10, rep = F))
    
    #--Message d'erreur en cas de fichier avec plus que 5 colonnes.
    if(ncol(excel)>5){
      showModal(
        modalDialog(
          title =
            "Problem with groups number",
          paste("In a Venn diagram, you can not have more than 5 groups !! The file you uploaded has actually",
                ncol(excel), "groups. Please, close this message window and upload another file."
          ),
          footer = modalButton("OK"),
          size = "l"
        )
      )
      return()
    }
    #--output relatif ? la table des donn?es
    
    
    ######################################################################################
    
    updateSelectInput(session , "variables", choices = colnames(excel) , selected = NULL) 
    
    
    observeEvent(input$calcul,{
      if(length(input$variables)>0){
        
        
        monOp <- switch(input$operation,
                        intersect = intersect , 
                        union     = union, 
                        setdiff   = setdiff)
        
        
        monOPLabel <<- switch(input$operation,
                              intersect = paste("Intersection of ",  paste(input$variables, collapse = " & "), collapse = ""), 
                              union = paste("Union of ",  paste(input$variables, collapse = " & "), collapse = ""),
                              setdiff =paste("In ", input$variables[1], " but Not in any of ", paste(input$variables[2:length(input$variables)], collapse = " & "), collapse ="")
        )
        
        
        
        titre <- paste(monOPLabel)
        tb <- data.frame(Reduce(monOp , excel[, input$variables]))
        colnames(tb) <- titre
        
        
        
        output$resOp <- renderTable(  na.omit(tb) , rownames = T)
        show("resOp", anim =TRUE,animType = "fade")
      }else{return()}
      
      
    })
    
    
    hide("resOp")
    
    
    observeEvent(
      input$showHideCalcul, {
        
        toggle("resOp")
        
      })#observeEvent
    
    ######################################################################################
    
    #-- output relatif au diag de Venn
    
    
    
    
    
    
    tst2 <- as.list(excel) #transformer le fichier Excel en list
    
    tst3 <- lapply(tst2 , function(tst2) tst2[!is.na(tst2)])#?liminer les les NAs
    
    output$tableExcel    <- renderTable(excel)
    hide("tableExcel")
    
    
    #-- couleurs des cercles du diagramme de venn
    coul   <- colors()[grep("green|red|yellow|blue", colors())]
    coul_N <- sample(coul,length(tst3))
    
    output$vennImg       <-
      renderPlot(
        grid.draw(venn.diagram(tst3 ,filename = NULL, fill = coul_N , col = "transparent" ,
                               alpha = input$alph , print.mode = input$numMode , cat.col = coul_N,
                               cat.cex = input$cat.cex , cat.dist = input$cat.dist,
                               margin =0.1
        )
        )
      )
    
    
    hide("vennImg")
    
    observeEvent(
      input$showHideData, {
        
        toggle("tableExcel")
      })#observeEvent
    
    observeEvent(
      input$showHideVenn , {
        toggle("vennImg")
      })#observeEvent
    
    observeEvent(
      input$ChangeColors, {
        coul_N <<- sample(coul,length(tst3))
        
        output$vennImg       <- renderPlot(
          grid.draw(venn.diagram(tst3 ,filename = NULL, fill = coul_N , col = "transparent" ,
                                 alpha = input$alph , print.mode = input$numMode , cat.col = coul_N,
                                 cat.cex = input$cat.cex , cat.dist = input$cat.dist,
                                 margin =0.1
          )
          )#grid.draw
        )# renderPlot
      })#observeEvent input$ChangeColors, {
    
    
    
    observeEvent(
      input$colByHand, {
        # coul_N <- NULL
        coul_N <- input$colByHand
        if(length(coul_N)!=ncol(excel)){return()}else{
          
          output$vennImg       <- renderPlot(
            grid.draw(venn.diagram(tst3 ,filename = NULL, fill = coul_N , col = "transparent" ,
                                   alpha = input$alph , print.mode = input$numMode , cat.col = coul_N,
                                   cat.cex = input$cat.cex , cat.dist = input$cat.dist,
                                   margin =0.1
            )
            )#grid.draw
          )# renderPlot
        }
      })#observeEvent input$ChangeColors, { 
    
    
    #-- Cr?ation du pdf ? t?l?charger
    
    output$download_PDF <- downloadHandler(
      
      #-- Nom du fichier de sortie
      filename = paste0(monDocName,"_", Sys.Date(), ".pdf")
      ,
      #--createdTempFilePath est un chemin temporaire cr?er par la fonction 'downloadHandler' pour stocker les ?l?ments zipp?s.
      #Exemple : "C:\\Users\\oallatif\\AppData\\Local\\Temp\\RtmpEvac1y\\file3f4458725b.zip"
      content = function(createdTempFilePath) {
        
        pdf(file = createdTempFilePath, paper = 'a4r' , h=0,w=0)
        grid.draw(venn.diagram(tst3 ,filename = NULL, fill = coul_N , col = "transparent" ,
                               alpha = input$alph , print.mode = input$numMode , cat.col = coul_N,
                               cat.cex = input$cat.cex , cat.dist = input$cat.dist ,margin =0.1
        )
        )
        dev.off()
        
      }
      ,
      contentType = "image/pdf"
    )#downloadHandler
    
  })#observe (le 1er 'observe' dans la fonction 'server')
  
  
  
  ################################################################################################### 
  ################################################################################################### 
  #################################      DEMO    ################################################### 
  ################################################################################################### 
  ################################################################################################### 
  
  observeEvent(
    input$demo , {
      
      
      if(exists("excel")){
        if(!is.null(excel)){shinyjs::info("You are currently working with your own dataset, 
              Press the button 'Reset to start new analysis' 
              before uploading a Demo dataset.")
          return()
        }}
      
      excelDemo <<-   data.frame(Groupe1= c(sample(LETTERS,10, rep = F), rep(NA,0)),
                                 Groupe2= c(sample(LETTERS,6, rep = F), rep(NA,4)),
                                 Groupe3= c(sample(LETTERS,8, rep = F), rep(NA,2)),
                                 Groupe4= c(sample(LETTERS,9, rep = F), rep(NA,1))
      )
      
      
      
      
      # dataDemo <- data.frame(Groupe1= sample(LETTERS,10, rep = F),Groupe2= sample(LETTERS,10, rep = F),
      #            Groupe3= sample(LETTERS,10, rep = F),Groupe4= sample(LETTERS,10, rep = F),Groupe5= sample(LETTERS,10, rep = F))
      
      #--Message d'erreur en cas de fichier avec plus que 5 colonnes.
      if(ncol(excelDemo)>5){
        showModal(
          modalDialog(
            title =
              "Problem with groups number",
            paste("In a Venn diagram, you can not have more than 5 groups !! The file you uploaded has actually",
                  ncol(excelDemo), "groups. Please, close this message window and upload another file."
            ),
            footer = modalButton("OK"),
            size = "l"
          )
        )
        return()
      }
      #--output relatif ? la table des donn?es
      
      
      #-- output relatif au diag de Venn
      
      
      ######################################################################################
      
      updateSelectInput(session , "variables", choices = colnames(excelDemo) , selected = NULL) 
      
      
      observeEvent(input$calcul,{
        if(length(input$variables)>0){
          
          
          monOp <- switch(input$operation,
                          intersect = intersect , 
                          union     = union, 
                          setdiff   = setdiff)
          
          
          monOPLabel <- switch(input$operation,
                               intersect = paste("Intersection of ",  paste(input$variables, collapse = " & "), collapse = ""), 
                               union = paste("Union of ",  paste(input$variables, collapse = " & "), collapse = ""),
                               setdiff =paste("In ", input$variables[1], " but Not in any of ", paste(input$variables[2:length(input$variables)], collapse = " & "), collapse ="")
          )
          
          
          
          titre <- paste(monOPLabel)
          tb <- data.frame(Reduce(monOp , excelDemo[, input$variables]))
          colnames(tb) <- titre
          
          
          
          output$resOp <- renderTable(  na.omit(tb) , rownames = T)
          show("resOp", anim =TRUE,animType = "fade")
        }else{return()}
        
        
      })
      
      
      hide("resOp")
      
      
      observeEvent(
        input$showHideCalcul, {
          
          toggle("resOp")
          
        })#observeEvent
      
      ######################################################################################
      
      tst2 <- as.list(excelDemo) #transformer le fichier excelDemo en list
      
      tst3 <- lapply(tst2 , function(tst2) tst2[!is.na(tst2)])#?liminer les les NAs
      
      output$tableexcelDemo    <- renderTable(excelDemo)
      hide("tableexcelDemo")
      
      
      #-- couleurs des cercles du diagramme de venn
      coul   <- colors()[grep("green|red|yellow|blue", colors())]
      coul_N <- sample(coul,length(tst3))
      
      output$vennImg       <-
        renderPlot(
          grid.draw(venn.diagram(tst3 ,filename = NULL, fill = coul_N , col = "transparent" ,
                                 alpha = input$alph , print.mode = input$numMode , cat.col = coul_N,
                                 cat.cex = input$cat.cex , cat.dist = input$cat.dist,
                                 margin =0.1
          )
          )
        )
      
      hide("vennImg")
      
      
      observeEvent(
        input$showHideData, {
          
          toggle("tableexcelDemo")
        })#observeEvent
      
      observeEvent(
        input$showHideVenn , {
          toggle("vennImg")
        })#observeEvent
      
      observeEvent(
        input$ChangeColors, {
          coul_N <<- sample(coul,length(tst3))
          
          output$vennImg       <- renderPlot(
            grid.draw(venn.diagram(tst3 ,filename = NULL, fill = coul_N , col = "transparent" ,
                                   alpha = input$alph , print.mode = input$numMode , cat.col = coul_N,
                                   cat.cex = input$cat.cex , cat.dist = input$cat.dist,
                                   margin =0.1
            )
            )#grid.draw
          )# renderPlot
        })#observeEvent
      
      
      observeEvent(
        input$colByHand, {
          # coul_N <- NULL
          coul_N <- input$colByHand
          if(length(coul_N)!=ncol(excelDemo)){return()}else{
            
            output$vennImg       <- renderPlot(
              grid.draw(venn.diagram(tst3 ,filename = NULL, fill = coul_N , col = "transparent" ,
                                     alpha = input$alph , print.mode = input$numMode , cat.col = coul_N,
                                     cat.cex = input$cat.cex , cat.dist = input$cat.dist,
                                     margin =0.1
              )
              )#grid.draw
            )# renderPlot
          }
        })#observeEvent input$ChangeColors, { 
      
      
      #-- Cr?ation du pdf ? t?l?charger
      
      output$download_PDF <- downloadHandler(
        
        #-- Nom du fichier de sortie
        filename = paste0("Demo_", Sys.Date(), ".pdf")
        ,
        #--createdTempFilePath est un chemin temporaire cr?er par la fonction 'downloadHandler' pour stocker les ?l?ments zipp?s.
        #Exemple : "C:\\Users\\oallatif\\AppData\\Local\\Temp\\RtmpEvac1y\\file3f4458725b.zip"
        content = function(createdTempFilePath) {
          
          pdf(file = createdTempFilePath, paper = 'a4r' , h=0,w=0)
          grid.draw(venn.diagram(tst3 ,filename = NULL, fill = coul_N , col = "transparent" ,
                                 alpha = input$alph , print.mode = input$numMode , cat.col = coul_N,
                                 cat.cex = input$cat.cex , cat.dist = input$cat.dist ,margin =0.1
          )
          )
          dev.off()
          
        }
        ,
        contentType = "image/pdf"
      )#downloadHandler
      
    }) #observeEvent(input$demo 
  
}#server
