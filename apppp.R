library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(rpivotTable)
library(readxl)
library(shinyWidgets)
library(rvest)
library(htmlwidgets) # to use saveWidget function


gm= tags$h3(strong("Good Morning",style="color:#116bac"))
ga= tags$h3(strong("Good Afternoon",style="color:#116bac"))
ge= tags$h3(strong("Good Evening",style="color:#116bac"))

#===========
## FUNCTIONS
#===========
## SIMPLE GREETING
good_time <- function(){
  
  ## LOAD PACKAGE
  require(lubridate, quietly = T)
  
  ## ISOLATE currHour
  currhour = hour(now())
  
  ## RUN LOGIC
  if(currhour < 12){
    return(gm)
  } 
  else if(currhour >= 12 & currhour < 17){
    return(ga)
  }
  else if( currhour >= 17){
    return(ge)  
  }
}

js <- "
function filter(node){
  return (node.tagName !== 'i');
}
function exportPlot(filename){
  var plot = document.getElementsByClassName('pvtRendererArea');
  domtoimage.toPng(plot[0], {filter: filter, bgcolor: 'white'})
    .then(function (dataUrl) {
      var link = document.createElement('a');
      link.download = filename;
      link.href = dataUrl;
      link.click();
    });
}
Shiny.addCustomMessageHandler('export', exportPlot);
"


## STARTING LOGGED VALUE; LET'S CHANGE THAT!
Logged = FALSE;


#====
# UI
#====
## make login screen
ui1 <- function(){
  
  tagList(tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:#116bac}')),
          div(id="container",align="center",
              div(id = "login",
                  # make login panel
                  wellPanel(id="well",style = "overflow-y: ;width:100%;height:100%",
                            br(),
                            HTML(paste0('
                                        <h2>
                                        Hello, ', good_time(),
                                        '</h2>',
                                        '<h3>
                                        <br>You are in Admin page.</br>
                                        </h3>')
                            ),
                            br(),
                            br(),
                            tags$div(textInput("userName", "Username",width = "100%"),align="left"),
                            br(),
                            tags$div(passwordInput("passwd", "Password",width = "100%"),align="left"),
                            br(),
                            # button
                            tags$div(actionButton("Login", "Log in"),align="center"),
                            # login error message
                            tags$div(uiOutput("message"),align="center")
                            )
                  
                  )
          ),
          # css for container
          tags$style(type = "text/css", 
                     "#container{
                     display: flex;
                     justify-content: center;
                     margin-top: 150px;
}"),
        # css for login well panel
        tags$style(type="text/css", "
                   #login,{
                   font-size:14px; 
                   width: 360px;}"),
        # well panel
        tags$style(type="text/css",
                   "#well{
                   padding: 50px;
                   background: white;
                   border: 1px;
                   box-shadow: ;}"),
        # welcome text css
        tags$style(type = 'text/css',
                   "h2, h3{
                   color: #525252;}"),
        # input fields
        tags$style(type="text/css",
                   "#userName, #passwd{
                   box-shadow: none;
                   outline:none;
                   border: none;
                   padding-left: 0;
                   border-bottom: 2px solid #116bac;
                   border-radius: 0;
                   }
                   #userName:focus, #passwd:focus{
                   box-shadow: 0px 10px 10px -5px lightgray;
                   }"),
        # button css
        tags$style(type='text/css',
                   "#Login{
                   outline: none;
                   margin-left: 0px;
                   width: 100px;
                   font-size: 12pt;
                   background: transparent;
                   border: 2px solid #116bac;
                   color: #116bac;
                   border-radius: 10px;
                   transition: 0.8s ease-in-out;
                   }
                   #Login:hover{
                   background: #116bac;
                   color: white;}"),
        # error box - fadeOut animation
        tags$style(type="text/css",
                   "@-webkit-keyframes fadeOut {
                   from {
                   opacity: 1;
                   }
                   to {
                   opacity: 0;
                   }
                   }
                   @keyframes fadeOut {
                   from {
                   opacity: 1;
                   }
                   to {
                   opacity: 0;
                   }
                   }"),
        tags$style(type="text/css",
                   "#error-box{
                   margin-top: 20px;
                   margin-left: 0px;
                   padding: 5px 10px 5px 10px;
                   text-align: center;
                   width: 325px;
                   color: white;
                   background: #ef3b2c;
                   border: 1px solid #ef3b2c;
                   border-radius: 5px;
                   -webkit-animation: fadeOut;
                   animation: fadeOut;
                   opacity: 0;
                   animation-duration: 15s;}")
        )
  }

#=========
# PRINT UI
#=========
ui = (uiOutput("page"))

#========
# SERVER
#========

server = shinyServer(function(input, output,session){
  options(shiny.maxRequestSize=50*1024^2)
  users <- data.frame(User=c("summary"),Password=c("statistics"))
  ## BEGIN BUILD LOG IN SCREEN
  USER <- reactiveValues(Logged = Logged)
  
  ## ERROR CHECKING
  observeEvent(input$Login,{
    
    ## output error message
    output$message <- renderUI({
      if(!is.null(input$Login)){
        my_username <- length(users$User[grep(pattern = input$userName, x = users$User)])
        my_password <- length(users$User[grep(pattern = input$passwd, x = users$Password)])
        if(input$Login > 0){
          if(my_username < 1 ||  my_password < 1){
            HTML("<div id='error-box'>
                 Sorry, that's not the right username or password. Please, 
                 try again. If you continue to have problems,
                 <a href='###'>
                 <u>Contact Us..</u></a>
                 </div>")
          }
          }
          }
          })
    
    ## CHECK INPUT
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(users$User == Username)
          Id.password <- which(users$Password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username %in% Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
          })
  
  ## Make UI
  observe({
    # What to do when logged = F
    if (USER$Logged == FALSE) {
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    
    # Render UI when logged = T
    if (USER$Logged == TRUE) 
    {
      ## get the current user's authorization level 
      user_log <- toupper(input$userName)
      
      # if admin ("input.SELECT == 1 || input.FED == 2" )
      if(user_log == "SUMMARY" ){
        output$page <- renderUI({
          ###################################################### ADMIN UI PAGE ###################################################################################################################
          fluidPage(
            
            tags$head(
              tags$script(src = "dom-to-image.min.js"),
              tags$script(HTML(js))
            ),
            
            theme = shinytheme("simplex"),
            tagList(
              useShinyjs(),
            tags$style(HTML("
                             
            
                            .navbar  {
                            background-color:white; }
                            
                            .navbar .navbar-nav {float: left; 
                            margin-top: 32px;
                            color: #; 
                            font-size: 20px; 
                            background-color: #; }
                            
                            .navbar.navbar-default.navbar-static-top{ 
                            color: #; 
                            font-size: 23px; 
                            background-color: # ;}
                            
                            .navbar .navbar-header {
                            float: left;
                           
                            background-color: # ;}
                            
                            .navbar-default .navbar-brand { color: #054b94; 
                            font-size: 28px; 
                            margin-bottom:32px;
                            background-color: # ;} 
                            
                            ")),
            
            
            tags$head(HTML("<title>XYZ</title> <link rel='icon' type='image/gif/png' href='log.png'>")),
            
            navbarPage(id="tabs",
            
            title = tags$div(img(src="","", style="color:white;font-weight:200%;margin-top: -5px;margin-left: 30px;", height = 60)),position = "fixed-top",
            selected = tags$div(bsButton("dummy0",strong("Upload"),style = "danger",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),inverse = F,
            
            tabPanel(title = tags$div(bsButton("dummy0",strong("Upload"),style = "danger",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                     
                     fluidPage(
                       
                       tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}
                                  #modal1 .modal-dialog { width: 1800px;}
                                  #modal1 .modal-content  {background-color:#;}"), 
                       
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"
                       ),
                       tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                            overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                       tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                            overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                       
                       
                       br(),
                       br(),
                       br(),
                       
                       column(6,
                              
                              br(),br(),br(),br(),br(),br(),br(),
                              tags$div(id = 'logo1',img(src="https://i.ya-webdesign.com/images/statistics-vector-data-chart-4.png",height='100%',width='100%'),align="center")
                       ),
                       
                       br(),
                       br(),
                       
                       column(6,
                              
                              
                              bootstrapPage(
                                            
                                            br(),
                                            tags$div(id = 'logo2',img(src="img3.png",height='40%',width='40%'),align="center"),br(),
                                            
                                            tags$div(h4(strong(em("Statistics")),style="color:#2e5cb8;font-size:200%"),align="center"),
                                            
                                            
                                            
                                            br(),
                                              uiOutput('fileupload'), tags$div(h4(strong(em("OR")),style="color:black;font-size:170%"),align="center"),br(),
                                            tags$div(bsButton("dbase",h4(strong(em("Data Base..?")),style="color:white;font-size:100%"),style = "primary",size = "large"),align="center"),br(),
                                              #uiOutput("bss"),br(),
                                              uiOutput('checkbox'),
                                              uiOutput("button"),
                                              uiOutput("helptext"),
                                            br(),
                                            br(),
                                            bsPopover(id = "dummy000",title = "Note:",content = "XXX",placement = "right"),
                                            #bsPopover(id="check",title = "",content = "Note: I accept the XYZ Terms & Conditions.. Show the Analyse button",placement = "right"),
                                            tags$div(bsButton("reset", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                            
                                            
                                            #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                            br(),
                                            
                                            # tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                            #          tags$tbody("Need Help ?"),
                                            #          tags$a(href = "XYZ", "Contact Us...", target="_blank")
                                            # ),
                                            tags$div(actionLink("reset2",""),align="center"),
                                            br()
                              )
                       )
                       
                       
                       
                       )),
            
            
            
            tabPanel(value = "mytab2",
              
              title = tags$div(bsButton("dummy",strong("Summay Statistics"),style = "primary",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
              br(),br(),br(),br(),br(),br(),
              # tags$div(selectInput('select1', "Select One...",choices = c("Table", "Image"),selected = "Image"), align = 'center'),
              tags$div(radioButtons('select1', 'Select One...', choices = c('Image', 'Table'), selected = 'Image', inline = TRUE), align = 'center', style = 'color:green'),
              uiOutput('download1'),
              # uiOutput('download2'),
              tags$div(rpivotTableOutput("test",width = '100%'),align = 'center')
                          
              ),
            
            tabPanel(
              title = tags$a(href="javascript:history.go(0)",tags$div(bsButton("ss22",strong("Logout"),style = "success",size="small"),
                                                                      style="color:white;margin-top: -12px;font-weight:100%;",align="center"),style="color:white;margin-top: -32px;")
            )
            
            
          

           
            


            
            )
            )
                  )
          
          #########################################################################################################################################################################
          
          
          
      })
    }
      
   
        
      # if standard user
      else{
        output$page <- renderUI({
       
          
        })
      }
    }
    })
  
  
  
  ####################################################### server #############################################################################################
  
     
    
    
  observeEvent(input$reset,{
    reset(id = "file")
  })
  
    output[["fileupload"]] <- renderUI({
      input$reset
      tags$div(fileInput("file",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"),accept=c('.csv','.txt')),align="center")
      
    })
    

    
    
    output[["checkbox"]] <- renderUI({
      input$reset
      tags$div(checkboxInput("check",tags$a(href = "", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
      
    })
    
    output[["button"]] <- renderUI({
      if (is.null(input$file)) return()
        tags$div(bsButton("analyse",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
                 style="color:white;font-weight:100%;",align="center")
      
    })
    
    observeEvent(input$dbase, {
      sendSweetAlert(
        session = session,
        title = "Enter the database details..!",btn_labels = c("Connect..!", 'Cancel..?'),
        text = tags$div( tags$div(textInput("servername:",width = "80%",label = h3(strong(em("Server Name:")),style="text-align:center;color:#2e5cb8;font-size:120%")),align="left"),
                         tags$div(textInput("dbname",width = "80%",label =  h3(strong(em("Database Name:")),style="text-align:center;color:#2e5cb8;font-size:120%")),align="left"),
                         tags$div(textInput("port",width = "80%",label =  h3(strong(em("Port:")),style="text-align:center;color:#2e5cb8;font-size:120%")),align="left"),
                         tags$div(textInput("data",width = "80%",label =  h3(strong(em("Data Name:")),style="text-align:center;color:#2e5cb8;font-size:120%")),align="left")
                         #bsButton("dbsubmit","Connect..!",style = "primary",size = "large")))
                         ))
    })
    
    
    
  
  
  
  

  
  
  
  ############################################# Data ###############################################################################  
  
  data <-reactive({
    file1 <- input$file
    if(is.null(file1)) {return(NULL)}
    data <- read.csv(file1$datapath)
    #data=data.frame(readxl::read_excel("ega.xlsx"))
    data=data.frame(data)
    data
    
  })
  
  observeEvent(input$analyse, {
    confirmSweetAlert(
      session = session,
      inputId = "confirmation",
      type = "warning",
      title = "Are you sure the data was uploaded ?",
      tags$div(strong(h3("If upload Done then go to the Summary Statistics tab for output..",style="color:red;")),align="center"),
      btn_labels = c("Nope", "Yep"),
      danger_mode = TRUE
    )
  })
  
  session_store <- reactiveValues()
  
  pivottab = reactive({
    
    Pivot = rpivotTable(
      data(),
      onRefresh = 
        htmlwidgets::JS("function(config) { 
                           Shiny.onInputChange('myData', document.getElementById('test').innerHTML); 
                        }")
    )
    
    Pivot
    
  })
  
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){

      output$test <- renderRpivotTable({
        pivottab()
      })

    }
  })
  
  
  summarydf <- eventReactive(input$myData,{
    input$myData %>% 
      read_html %>% 
      html_table(fill = TRUE) %>% 
      # Turns out there are two tables in an rpivotTable, we want the second
      .[[2]]
    
  })
  
  
  table1 = reactive({
    summarydf()
  })
  

  # output[['download1']] = renderUI({
  #   
  #   if(dim(table1())[1] != 0){
  #     
  #       downloadButton('downloadData1', 'Download Data...')
  #     
  #   }else{
  #     
  #       downloadButton('downloadData2', 'Download Widget')
  #       # bsButton("downloadData2", "Download Plot")
  #     
  # 
  #   }
  #   
  # })

  # output$downloadData1 <- downloadHandler(
  #   filename = function() {
  #     paste("dataset-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(table1(), file)
  #   })
  
  # output$downloadData2 <- downloadHandler(
  #   filename = function() {
  #     paste("Summary - Statistics", Sys.Date(), ".png", sep = "")
  #   },
  #   content = function(file) {
  #     png(file)
  #     #plot(x = data()$vehicle_claim, y = data()$total_claim_amount)
  #     pivottab()
  #     dev.off()
  #     # saveWidget(pivottab(), file, selfcontained = TRUE)
  #     
  #   })
  

  output[['download1']] = renderUI({
    
    if(input$select1 == 'Image'){
      
      #downloadButton('downloadData1', 'Download Data...')
      bsButton("downloadData2", "Download Image", icon = icon('bar-chart-o'))
      
    }else{
      
      downloadButton('downloadData1', 'Download Data...', icon = icon('table'))
      # bsButton("downloadData2", "Download Plot")
      
      
    }
    
  })
  
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table1(), file)
    })
  
  observeEvent(input[["downloadData2"]], {
    session$sendCustomMessage("export", "plot.png")
  })  
  
  
  
  
  
  
  
  }) # END SHINYAPP

#======
# RUN
#======
shinyApp(ui = ui, server = server)

#save(app,file = "app.rda")
