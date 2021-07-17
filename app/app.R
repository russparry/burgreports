library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
source("readin2.R")


BurgSpec$Date <- as.Date(BurgSpec$Date)

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
    tabPanel("All Tracked Games",
             
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
    
    
}

shinyApp(ui = ui, server = server)

