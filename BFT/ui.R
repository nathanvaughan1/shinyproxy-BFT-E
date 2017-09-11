
library(ncdf4)
# display 10 rows initially
linksfile <- read.csv('https://goo.gl/lcuqZO')

fluidPage(
    # We'll add some custom CSS styling -- totally optional
    includeCSS("www/styling1.css"),
    
    # And custom JavaScript -- just to send a message when a user hits "enter"
    # and automatically scroll the chat window for us. Totally optional.
    includeScript("www/sendOnEnter.js"),
      div(
        # Setup custom Bootstrap elements here to define a new layout
        class = "container-fluid", 
        div(class = "row-fluid",
            # Set the page title
            tags$head(tags$title("East/Med Bluefin tuna stock assessment")),
            
             img(src="bluefin-tuna.png", height = 150, width = 380),
            tags$style(".span12 {background-color: black;}"),
            # Create the header
            div(class="span6", style="padding: 10px 0px;",
                h1("Retrospective anlysis outputs of the Bluefin tuna east stock assessment")),
                # h1("Chat members "),
                # h4("East/Med Bluefin tuna stock assessment"),
                h6(format(Sys.Date(), format="%B %d %Y"))
            ), div(class="span6", id=as.character(Sys.info()["nodename"])
            )),       
  
    navbarPage("Bluefin Tuna - east",
               tabPanel("Table",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "Run",
                                        label = "Choose the run",
                                        choices  = paste(linksfile$runNumb_retros,' SEED:',linksfile$seedNumb_retros,sep=''),
                                        selected = paste(linksfile$runNumb_retros,' SEED:',linksfile$seedNumb_retros,sep='')[1],
                                        multiple = FALSE
                            )
                            # downloadButton('downloadData', 'Download')
                            
                          ),
                          mainPanel(
                            tabsetPanel(
                              id = 'dataset',
                              tabPanel('Main data',     DT::dataTableOutput('tab1')),
                              tabPanel('Residuals in catch',        DT::dataTableOutput('tab2')),
                              tabPanel('Fishing mortality by age',      DT::dataTableOutput('tab3')),
                              tabPanel('Obj',       DT::dataTableOutput('tab4'))
                            )
                          )
                        )
               ) ,
               tabPanel("ncdump",
                       
                          mainPanel(
                            verbatimTextOutput("ncdump")
                          )
                        
                       
               ))       

)

# 
# 
# # navbarPage(
# # title = 'Retrospective anlysis outputs of the Bluefin tuna east stock assessment',
# shinyUI(pageWithSidebar(
#   # We'll add some custom CSS styling -- totally optional
#   includeCSS("www/styling1.css"),
#   
#   # And custom JavaScript -- just to send a message when a user hits "enter"
#   # and automatically scroll the chat window for us. Totally optional.
#   includeScript("www/sendOnEnter.js"),
#   
#   div(
#     # Setup custom Bootstrap elements here to define a new layout
#     class = "container-fluid", 
#     div(class = "row-fluid",
#         # Set the page title
#         tags$head(tags$title("East/Med Bluefin tuna stock assessment")),
#         div(class = "container-fluid",
#             sidebarPanel(
#               selectInput(inputId = "Run",
#                           label = "Choose the run",
#                           choices  = paste(linksfile$runNumb_retros,' SEED:',linksfile$seedNumb_retros,sep=''),
#                           selected = paste(linksfile$runNumb_retros,' SEED:',linksfile$seedNumb_retros,sep='')[1],
#                           multiple = FALSE
#               )
#               
#             ),
#          # img(src="bluefin-tuna.png", height = 150, width = 380),
#         tags$style(".span12 {background-color: black;}"),
#         # Create the header
#         div(class="span6", style="padding: 10px 0px;",
#             h1("Retrospective anlysis outputs of the Bluefin tuna east stock assessment")),
#             # h1("Chat members "),
#             # h4("East/Med Bluefin tuna stock assessment"),
#             h6(format(Sys.Date(), format="%B %d %Y"))
#         ), div(class="span6", id=as.character(Sys.info()["nodename"])
#               
#         ),
#        
#     
#     
# 
# #      div(class = "container-fluid",
# #   # fil <- read.csv('https://goo.gl/w4tD15')
# #   
# # 
# # 
# navbarPage(
#   
#   tabPanel('Main data',     DT::dataTableOutput('tab1')),
#   tabPanel('Residuals in catch',        DT::dataTableOutput('tab2')),
#   tabPanel('Fishing mortality by age',      DT::dataTableOutput('tab3')),
#   tabPanel('Obj',       DT::dataTableOutput('tab4'))
#   # tabPanel('main',  DT::dataTableOutput('tab5'))
# )
# # )
# 
# )
# )
# )
# )