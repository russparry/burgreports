library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(rsconnect)
source("readin2.R")


BurgSpec$Date <- as.Date(BurgSpec$Date)

fair <- subset(BurgSpecH, PitchCall == "InPlay")

fair$direct <- fair$Bearing + 90

fair$x <- fair$Distance * (-cos(fair$direct*pi/180))

fair$y <- fair$Distance * (sin(fair$direct*pi/180))

bases <- data.frame(xl=c(0,90/sqrt(2),0,-90/sqrt(2),0),
                    yl=c(0,90/sqrt(2), 2 * 90/sqrt(2), 90/sqrt(2), 0))


ui <- fluidPage(
  HTML('
       <img src="sburger.png", height="185px", width="200px",
            style="display: block; margin-left: auto; 
                 margin-right: auto;"/>'),
  navbarPage(
    HTML('<img src="sburger.png", height="18px", width="20px",
            style="display: block; margin-left: auto; 
                 margin-right: auto;"/>'),
    tabPanel("Pitcher: Single Game",
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(BurgSpec$Pitcher))),
                 
                 selectInput(inputId = "DateInput", label = "Select Game(s)", choices = ""),
                 
                 HTML('<img src="hburger.png", height="185px", width="200px", style="display: block; margin-left: auto; margin-right: auto;"/>')
                 
                 
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Outing Summary", br(), dataTableOutput("pitcher_sum")),
                   tabPanel("Zone Report", br(),
                            
                            sidebarLayout(
                              sidebarPanel(
                                pickerInput(inputId = "PitchInput", label = "Select Pitch", 
                                            choices = ""),
                                
                                pickerInput(inputId = "HandInput", label = "Batter Side", 
                                            choices = list("All", "Right", "Left"),
                                            selected = "All"),
                                
                                selectInput(inputId = "ResInput", label = "Pitch Result", 
                                            choices = "")
                                
                              ),
                              mainPanel(
                                plotOutput("strike_zone")
                              )
                              
                            )
                            
                            ),
                   tabPanel("Pitch Spin Rates", br(), plotOutput("boxplot2"))
                 )
                 #h2("."),
                 #tabsetPanel(
                 # tabPanel("Pitch Uage", br(), selectInput(inputId = "ZoneInput", label = "Select Pitch", choices = sort(unique(BurgSpec$TaggedPitchType))), plotOutput("barhart"))
                 
                 #)
               )
               
             )   
      
    ),
    tabPanel("Hitters", sidebarLayout(
      
      sidebarPanel(
        
        selectInput(inputId = "BatterInput", label = "Select Batter", choices = sort(unique(BurgSpecH$Batter))),
        
        HTML('<img src="hburger.png", height="185px", width="200px", style="display: block; margin-left: auto; margin-right: auto;"/>')
        
        
        
      ),
      mainPanel(
        tabsetPanel(
          
          tabPanel("Zone Report", br(),
                   
                   sidebarLayout(
                     sidebarPanel(
                       pickerInput(inputId = "PitchhInput", label = "Select Pitch", 
                                   choices = ""),
                       
                       selectInput(inputId = "ReshInput", label = "Pitch Result", 
                                   choices = "")
                       
                     ),
                     mainPanel(
                       plotOutput("strike_zoneH")
                     )
                     
                   )
                   
          ),
          tabPanel("Spray Chart", br(),
                   sidebarLayout(
                     sidebarPanel(
                       
                       pickerInput(inputId = "HitInput", label = "Hit Result",
                                   choices = "")
                     ),
                     mainPanel(
                       plotOutput("spray_chart")
                     )
                   )
                   
          )
        )
        
      )
      
    )   
             
             )
    
  )
    
    
)




server <- function(input, output, session){
  
  observeEvent(input$PitcherInput, 
               updateSelectInput(session, inputId = "DateInput", label = "Select Game", 
                                 choices = sort(unique((BurgSpec$Date[BurgSpec$Pitcher == input$PitcherInput])))))
  observeEvent(input$PitcherInput, 
               updatePickerInput(session, inputId = "PitchInput", label = "Select Pitch",
                                 choices = sort(c(unique((BurgSpec$TaggedPitchType[BurgSpec$Pitcher == input$PitcherInput])), "All"))))
  observeEvent(input$PitcherInput, 
               updateSelectInput(session, inputId = "ResInput", label = "Select Result", 
                                 choices = sort(c(unique((BurgSpec$PitchCall[BurgSpec$Pitcher == input$PitcherInput])), "All"))))
  

  observeEvent(input$BatterInput, 
               updatePickerInput(session, inputId = "PitchhInput", label = "Select Pitch",
                                 choices = sort(c(unique((BurgSpecH$TaggedPitchType[BurgSpecH$Batter == input$BatterInput])), "All"))))
  observeEvent(input$BatterInput, 
               updateSelectInput(session, inputId = "ReshInput", label = "Select Result", 
                                 choices = sort(c(unique((BurgSpecH$PitchCall[BurgSpecH$Batter == input$BatterInput])), "All"))))
  
  observeEvent(input$BatterInput, 
               updatePickerInput(session, inputId = "HitInput", label = "Select Hit Result", 
                                 choices = sort(c(unique((BurgSpecH$PlayResult[BurgSpecH$Batter == input$BatterInput])), "All" ))))
  
  
  
  
  output$pitcher_sum <- renderDataTable({
    table <- BurgSpec %>%
      filter(Pitcher == input$PitcherInput, Date == input$DateInput) %>%
      
      group_by('Pitch' = TaggedPitchType) %>%
      
      summarize('No.' = n(),
                'Max Velo' = round(max(RelSpeed, na.rm=TRUE),1),
                'Avg Velo' = round(mean(RelSpeed, na.rm=TRUE),1),
                'Avg Spin' = round(mean(SpinRate, na.rm=TRUE),0),
                'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "InPlay"))/n(),3)*100,
                'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                    sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "InPlay")),3)*100
                )
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
    
  })  
  
  
  
  output$strike_zone <- renderPlot({
    dataFilter <- reactive({
      if(input$HandInput == "All" && input$PitchInput == "All" && input$ResInput == "All"){
        BurgSpec %>%
          filter(Pitcher == input$PitcherInput, Date == input$DateInput)
      }
      else if(input$HandInput == "All" && input$PitchInput == "All" && input$ResInput != "All"){
        BurgSpec %>%
        filter(Pitcher == input$PitcherInput, Date == input$DateInput, 
          PitchCall == input$ResInput)
      }
      else if(input$HandInput == "All" && input$PitchInput != "All" && input$ResInput == "All"){
        BurgSpec %>%
        filter(Pitcher == input$PitcherInput, Date == input$DateInput, 
               TaggedPitchType == input$PitchInput)
      }
      else if(input$HandInput != "All" && input$PitchInput == "All" && input$ResInput == "All"){
        BurgSpec %>%
        filter(Pitcher == input$PitcherInput, Date == input$DateInput, 
               BatterSide == input$HandInput)
      }
      else if(input$HandInput == "All" && input$PitchInput != "All" && input$ResInput != "All"){
        BurgSpec %>%
        filter(Pitcher == input$PitcherInput, Date == input$DateInput, 
               PitchCall == input$ResInput, TaggedPitchType == input$PitchInput)
      }
      else if(input$HandInput != "All" && input$PitchInput == "All" && input$ResInput != "All"){
        BurgSpec %>%
        filter(Pitcher == input$PitcherInput, Date == input$DateInput, 
               PitchCall == input$ResInput, BatterSide == input$HandInput)
      }
      else if(input$HandInput != "All" && input$PitchInput != "All" && input$ResInput == "All"){
        BurgSpec %>%
        filter(Pitcher == input$PitcherInput, Date == input$DateInput, 
               BatterSide == input$HandInput, TaggedPitchType == input$PitchInput)
      }
      else{
        BurgSpec %>%
          filter(Pitcher == input$PitcherInput, Date == input$DateInput, 
                 BatterSide == input$HandInput, 
                 TaggedPitchType == input$PitchInput, PitchCall == input$ResInput)
      }
      
    })
    
    
    ggplot(data = (if(nrow(dataFilter()) == 0)return() else(dataFilter())),
             aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      xlim(-3,3) + ylim(0,5) + labs(color= "", title = "Pitch Location") +
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(size = 3, na.rm = TRUE) +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
  
  }, width = 350, height = 450)
  
  
  

    
    output$boxplot2 <- renderPlot({
      
      dataFilter <- reactive ({
        BurgSpec %>% 
          filter(Pitcher == input$PitcherInput, Date == input$DateInput)
        
      })
      ggplot(dataFilter(), aes(x = reorder(TaggedPitchType, -SpinRate), y = SpinRate, fill = TaggedPitchType)) + geom_boxplot(width = 0.5) +
        labs(x = "Pitch", y = "SpinRate", title = "SpinRate Distribution") + theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
      
    }, width = 800, height = 450)
    
    


output$strike_zoneH <- renderPlot({
  dataFilter <- reactive({
    if(input$PitchhInput == "All" && input$ReshInput == "All"){
      BurgSpecH %>%
        filter(Batter == input$BatterInput)
    }
    else if(input$PitchhInput == "All" && input$ReshInput != "All"){
      BurgSpecH %>%
        filter(Batter == input$BatterInput,
               PitchCall == input$ReshInput)
    }
    else if(input$PitchhInput != "All" && input$ReshInput == "All"){
      BurgSpecH %>%
        filter(Batter == input$BatterInput, 
               TaggedPitchType == input$PitchhInput)
    }
    else if(input$PitchhInput == "All" && input$ReshInput == "All"){
      BurgSpecH %>%
        filter(Batter == input$BatterInput)
    }
    else if(input$PitchhInput != "All" && input$ReshInput != "All"){
      BurgSpecH %>%
        filter(Batter == input$BatterInput, 
               PitchCall == input$ReshInput, TaggedPitchType == input$PitchhInput)
    }
    else if(input$PitchhInput == "All" && input$ReshInput != "All"){
      BurgSpecH %>%
        filter(Batter == input$BatterInput,
               PitchCall == input$ReshInput)
    }
    else if(input$PitchInput != "All" && input$ResInput == "All"){
      BurgSpecH %>%
        filter(Batter == input$BatterInput, TaggedPitchType == input$PitchhInput)
    }
    else{
      BurgSpecH %>%
        filter(Batter == input$BatterInput, 
               TaggedPitchType == input$PitchhInput, PitchCall == input$ReshInput)
    }
    
  })
  
  
  ggplot(data = (if(nrow(dataFilter()) == 0)return() else(dataFilter())),
         aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    xlim(-3,3) + ylim(0,5) + labs(color= "", title = "Pitch Location") +
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
    geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(size = 3, na.rm = TRUE) +
    theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
  
}, width = 350, height = 450)


output$spray_chart <- renderPlot({
  
  dataFilter <- reactive({
    if(input$HitInput == "All"){
      fair %>%
        filter(Batter == input$BatterInput)
    }
    else{
      fair %>%  
        filter(Batter == input$BatterInput, PlayResult == input$HitInput)
    }
    
  })
  
  
  ggplot(data = dataFilter(), aes(x = x, y = y, color = PlayResult)) + 
    xlim(-350,350) + 
    ylim(0,500) + 
    labs(color= "") +
    geom_point(size = 3, na.rm = TRUE) +
    geom_segment(x=0,xend=300,y=0,yend=300, color = "black") +  
    geom_segment(x=0,xend=-300,y=0,yend=300, color = "black") + 
    geom_segment(x=300,xend=175,y=300,yend=375, color = "black") +
    geom_segment(x=175,xend=50,y=375,yend=400, color = "black") + 
    geom_segment(x=50,xend=-50,y=400,yend=400, color = "black") + 
    geom_segment(x=-50,xend=-175,y=400,yend=375, color = "black") + 
    geom_segment(x=-175,xend=-300,y=375,yend=300, color = "black") +
    labs(y = "Distance")
    
  
}, width = 700, height = 500)



}


shinyApp(ui = ui, server = server)
