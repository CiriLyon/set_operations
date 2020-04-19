ui =
  
  library(VennDiagram)
library(shinythemes)
library(gplots)
library(shiny)
library(shinyjs)
library(openxlsx)
library(V8)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"



fluidPage(#img(src="logo_ciri.png", height = 180, width = 800),
 
  
          title = "Set operations & Venn Diagrams GUI",
          theme = shinytheme("slate") ,
          useShinyjs()                     , # Include shinyjs in the UI
          extendShinyjs(text = jsResetCode),

          
          fluidRow(
            #div(id = "televersement" ,
            sidebarLayout(position ="left",
                          
                          #_____________________________________________________________________________________            
                          sidebarPanel(width = 10,
                            
                            h3("This graphical interface is designed to perform mathematical operations - Intersection, union & difference - between
                                the categorical variables of a data table. It also allows to lay out Venn diagrams." , style="color: white; background-color: black; font-size: 100%;border-color: dodgerblue")
                            
                            
                          ), # sidebarPanel(
                          
                          
                          #_____________________________________________________________________________________ 
                          mainPanel(
                             column(width = 12 , align = "center" , offset = 4 
                           
                             )#column(width = 8
                          )#mainPanel 
                          #_____________________________________________________________________________________ 
                          
            )#sidebarLayout
            # )#div
          ),#fluidRow,
          
          
br(), br() ,
          
          fluidRow(
            #div(id = "televersement" ,
            sidebarLayout(position ="left",
                          
                          #_____________________________________________________________________________________            
                          sidebarPanel(
                            
                            
                            h6("Upload your Excel file or Press 'Demo dataset' button to create a dataset internally then proceed with the analyzes") ,
                            tags$div(title="You can already use this software without uploading data. You will use an internal file intended for demonstration, Press 'Demo dataset' if so.",
                                     fileInput("boutonUpload" , "Data must be in the first sheet of your Excel file", accept = ".xlsx"
                                     )
                                     
                            ),
                            
                            hr(),
                            tags$div(title="Press 'Demo dataset' button to create a dataset internally.",
                                     actionButton('demo'   , 'Demo dataset'   ,
                                                  style="color: green; background-color: #337ab7; border-color: magenta"       )
                            ),
                            hr(),
                            tags$div(title="You need to refresh the application before re-loading a new dataset.",
                                     actionButton('resetData', 'Reset to start new analysis', icon("refresh"),
                                                  style="color: gold; background-color: #337ab7; border-color: dodgerblue")
                            )
                            
                            
                          ), # sidebarPanel(
                          
                          
                          #_____________________________________________________________________________________ 
                          mainPanel(
                            column(width = 8 , align = "right" , offset = 4 ,
                                   
                                   textOutput("fpath"),       #Affichage de texte.
                                   tableOutput("tableexcelDemo"), #Affichage de tables.
                                   tableOutput("tableExcel"), #Affichage de tables.
                                   plotOutput("vennImg") ,     #Affichage d'un plot.
                                   tableOutput("resOp") #resultat de l'operation ensembliste.
                            )#column(width = 8
                          )#mainPanel 
                          #_____________________________________________________________________________________ 
                          
            )#sidebarLayout
            # )#div
          ),#fluidRow,
          
          
          
          
          
          
          
          absolutePanel(
            
            
            fluidRow(
              
              column(width = 4,align = "left", offset = 1,
                     titlePanel( h1('Venn Diagrams', style = "color:mediumslateblue ; font-size:200% ; align = right")) ,
                     #-- Display 'counting ...'
                     
                     
                     hr(),
                     actionButton('showHideData'   , 'Show/Hide data'          ),
                     
                     hr(),
                     actionButton('showHideVenn'   , 'Show/Hide Venn diagram'  ),
                     hr(),
                     tags$div(title="Choose the density of your digram colors",
                              sliderInput("alph", 'Diagram Transparency Level', min =0 , max = 1,value=0.5, step = 0.01)
                     ),
                     hr(),
                     actionButton('ChangeColors', 'Change Diagram Colors Randomly'),
                     
                     hr(),
                     tags$div(title="Colors become active once you choose exactly as many colors as columns in the current dataset.",
                              selectInput("colByHand", "Select Colors By Hand", choices = sort(colors()) , selected = NULL , multiple =TRUE)
                     ),
                     
                     hr(),
                     tags$div(title="The size of the categorie name.",
                              sliderInput("cat.cex", 'Category name size', min =0.5 , max = 3,value=1, step = 0.1)
                     ),
                     
                     hr(),
                     tags$div(title="The distance of the category name from the edge of the circle.",
                              sliderInput("cat.dist", 'Category name dist', min =-1 , max = 1,value=-1, step = 0.01)
                     ),
                     
                     
                     hr(),
                     selectInput("numMode", "Display numbers", choices = c('raw', 'percent', 'FALSE'), selected = 'raw'),
                     
                     hr(),
                     downloadButton('download_PDF', 'Download the current diagram in pdf format')
              ),
              
              
              
              
              
              #--  les operations ensemblistes
              column(width = 4,align = "right", offset = 1,      
                     
                     titlePanel( h1('Ensemblist Operations', style = "color:mediumslateblue ; font-size:200% ; align = center")) ,
                     hr(),
                     
                     radioButtons('operation', 'Operation type',choices =c("Intersection" = "intersect", 
                                                                           "Union" = "union", 
                                                                           "Difference"= "setdiff"), selected =NULL ,inline = TRUE),
                     hr(),
                     selectInput("variables", "Select colmuns", choices = NULL , selected = NULL , multiple =TRUE, width = "200%", selectize = T) ,
                     
                     hr(),
                     tags$div(title="Launch the calculation.",
                              actionButton("calcul", 'Calculate', icon("fas fa-calculator") , style="color: indianred; background-color: #337ab7; border-color: magenta; font-size: 150%"  )
                     ),
                     
                     hr(),
                     actionButton('showHideCalcul'   , 'Show/Hide Calculation result'          ), 
                     
                     #--  fin des operations ensemblistes
                     
                     
                     
                     
                     conditionalPanel(
                       condition="$('html').hasClass('shiny-busy')",
                       tags$div(style="color:#FF00FF ; background-color:powderblue ; font-family:courier", paste( "Busy ..."))
                     )
                     
              )
            ), #fluidRow(
            top = 700, left = 0, right = NULL, bottom = NULL,
            width = 600, height = 200, draggable = F, fixed = F,
            cursor = c("auto", "move", "default", "inherit")
            ,
            br(), br(), br(), br(), br(), br(),
            h6("By Omran Allatif  - ", a(href="http://ciri.inserm.fr/service-bioinformatique-et-biostatistique-bibs/",
                           "CIRI-BIBS Service", style = "background-color:purple ; color:white; font-size:80%;" ),
               style = "margin-left:10%;" )#h6
          )# absolutePanel
)# fluidPage


# absolutePanel(
# fluidPage(    fluidRow(
#   sidebarLayout(
#     sidebarPanel(width = 8,
#                  h6("By the", a(href="http://ciri.inserm.fr/service-bioinformatique-et-biostatistique-bibs/",
#                                 "BIBS Service", style = "background-color:purple ; color:white" ))
#     ),
#
#     mainPanel(" "))
# )))
