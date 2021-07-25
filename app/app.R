library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
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
                 margin-right: auto;"/>
        
       '),
  navbarPage(
    HTML('
    Burger Reportz </title> 
         '),
    
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
                
               )
               
             )   
      
    ),
    tabPanel("Hitters Individual", sidebarLayout(
      
      sidebarPanel(
        
        selectInput(inputId = "BatterInput", label = "Select Batter", choices = sort(unique(BurgSpecH$Batter))),
        
        HTML('<img src="fries.png", height="185px", width="200px", style="display: block; margin-left: auto; margin-right: auto;"/>')
        
        
        
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
                   
          ),
          tabPanel("Hitter Summary", br(), dataTableOutput("hitter_sum"), br(), dataTableOutput("hitter_sum2")),
          tabPanel("Contact Points", br(), 
                   sidebarLayout(
                     sidebarPanel(
                       "Filter by:",
                       selectInput(inputId = "ConPitchInput", label = "Pitch",
                                   choices = ""),
                       selectInput(inputId = "ConAngleInput", label = "Launch Angle",
                                   choices = list("All", "<-10", "-10-0", "0-10", "10-20", "20-30", ">30"))
                     ),
                     mainPanel(
                       plotOutput("contact_pts")
                     )
                   )
                   
                   )
          
        )
        
        
      )
      
    )   
             
             ),
    tabPanel("Hitter Ranks min 100 Pitches tracked", br(), 
             "Top 10 showed. Click column header to see specific ranking. Metrics assume consistent strike zone (20 inches wide, 1.5 to 3.5 ft high)", 
             dataTableOutput("hitter_rank")),
    tabPanel("Hitter Ranks min 20 Pitches tracked", br(), 
             "Top 10 showed. Click column header to see specific ranking. Metrics assume consistent strike zone (20 inches wide, 1.5 to 3.5 ft high)", 
             dataTableOutput("hitter_rank2")),
    
    tabPanel("Pitcher Release Points", br(), 
             sidebarLayout(
              sidebarPanel(
                
                selectInput(inputId = "ReleInput", label = "Pitcher",
                            choices = sort(unique(BurgSpec$Pitcher)))
              ),
              mainPanel(
                plotOutput("rel_plot")
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
  
  observeEvent(input$BatterInput,
               updateSelectInput(session, inputId = "ConPitchInput", 
                                 choices = sort(c(unique((BurgSpecH$TaggedPitchType[BurgSpecH$Batter == input$BatterInput])), "All"   ))))
  
  #observeEvent(input$BatterInput,
   #            updateSelectInput(session, inputId = "ConAngleInput", 
    #                             choices = sort(c(unique((BurgSpecH$LA[BurgSpecH$Batter == input$BatterInput])), "All"   ))))
  
  
  
  ##  Pitcher Sum Table
  
  
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
  
  ## Pitcher Strike Zone plot
  
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
  
  
  
##Spin rate distribution
  
    output$boxplot2 <- renderPlot({
      
      dataFilter <- reactive ({
        BurgSpec %>% 
          filter(Pitcher == input$PitcherInput, Date == input$DateInput)
        
      })
      ggplot(dataFilter(), aes(x = reorder(TaggedPitchType, -SpinRate), y = SpinRate, fill = TaggedPitchType)) + geom_boxplot(width = 0.5) +
        labs(x = "Pitch", y = "SpinRate", title = "SpinRate Distribution") + theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
      
    }, width = 800, height = 450)
    
    

## Hitter Zone Plot    
    

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


## Hitter Spray Chart


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


##Hitter Summary Table

output$hitter_sum <- renderDataTable({
  table <- BurgSpecH %>%
    filter(Batter == input$BatterInput) %>%
    
    
    summarize('Pitches Tracked' = n(),
              'Max Exit Velo' = round(max(ExitSpeed, na.rm=TRUE),1),
              'Avg Exit Velo' = round(mean(ExitSpeed, na.rm=TRUE),1),
              'Median Exit Velo' = round(median(ExitSpeed, na.rm=TRUE),1),
              'Max Distance' = round(max(Distance, na.rm=TRUE),0),
              
              'Ball %' = round(sum(BurgSpecH$isBall[BurgSpecH$Batter==input$BatterInput], na.rm=T)/
                                 n()
                               ,3)*100,
              'Balls thrown' = sum(BurgSpecH$isBall[BurgSpecH$Batter==input$BatterInput], na.rm=T),
              
              'chase rate (all)' = 
                round(sum(BurgSpecH$isChase[BurgSpecH$Batter==input$BatterInput], na.rm=T)/
                        n()
                      ,3)*100,
              'chase rate (balls)' = 
                round(sum(BurgSpecH$isChase[BurgSpecH$Batter==input$BatterInput], na.rm=T)/
                        sum(BurgSpecH$isBall[BurgSpecH$Batter==input$BatterInput], na.rm=T)
                      ,3)*100,
              
              'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                  sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "InPlay")),3)*100,
              
              'Barrel rate' = round(sum(BurgSpecH$isBarrel[BurgSpecH$Batter==input$BatterInput], na.rm=T)/
                                      sum(BurgSpecH$isInPlay[BurgSpecH$Batter==input$BatterInput], na.rm=T)  
                                    ,3)*100
    )
  
  tableFilter <- reactive({table})
  datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
    formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  
}) 



output$hitter_rank <- renderDataTable({
  table <- BurgSpecH %>%
    
    
    
    group_by('Batter' = Batter) %>%
    
    filter(n() > 100) %>%
    
    summarize(
      'Pitches Tracked' =n(),
      'Strike %' = round( (n() - sum(BurgSpecH$isBall[BurgSpecH$Batter==Batter], na.rm=T) )/n() ,3)*100,
      'Ball %' = round(sum(BurgSpecH$isBall[BurgSpecH$Batter==Batter], na.rm=T) / n() , 3)*100,
      'Max Exit Velo' = round(max(ExitSpeed, na.rm=TRUE),1),
      'Avg Exit Velo' = round(mean(ExitSpeed, na.rm=TRUE),1),
      'Median Exit Velo' = round(median(ExitSpeed, na.rm=TRUE),1),
      'Max Distance' = round(max(Distance, na.rm=TRUE),0),
      
      
      'chase rate (all)' = 
        round(sum(BurgSpecH$isChase[BurgSpecH$Batter==Batter], na.rm=T)/
                n()
              ,3)*100,
      'chase rate (balls)' = 
        round(sum(BurgSpecH$isChase[BurgSpecH$Batter==Batter], na.rm=T)/
                sum(BurgSpecH$isBall[BurgSpecH$Batter==Batter], na.rm=T)
              ,3)*100,
      
      'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                          sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "InPlay")),3)*100,
      
      'Barrel rate' = round(sum(BurgSpecH$isBarrel[BurgSpecH$Batter==Batter], na.rm=T)/
                              sum(BurgSpecH$isInPlay[BurgSpecH$Batter==Batter], na.rm=T)  
                            ,3)*100
    )
  
  tableFilter <- reactive({table})
  datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
    formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  
})

output$hitter_rank2 <- renderDataTable({
  table <- BurgSpecH %>%
    
    
    
    group_by('Batter' = Batter) %>%
    
    filter(n() > 20) %>%
    
    summarize(
      'Pitches Tracked' =n(),
      'Strike %' = round( (n() - sum(BurgSpecH$isBall[BurgSpecH$Batter==Batter], na.rm=T) )/n() ,3)*100,
      'Ball %' = round(sum(BurgSpecH$isBall[BurgSpecH$Batter==Batter], na.rm=T) / n() , 3)*100,
      'Max Exit Velo' = round(max(ExitSpeed, na.rm=TRUE),1),
      'Avg Exit Velo' = round(mean(ExitSpeed, na.rm=TRUE),1),
      'Median Exit Velo' = round(median(ExitSpeed, na.rm=TRUE),1),
      'Max Distance' = round(max(Distance, na.rm=TRUE),0),
      
      
      'chase rate (all)' = 
        round(sum(BurgSpecH$isChase[BurgSpecH$Batter==Batter], na.rm=T)/
                n()
              ,3)*100,
      'chase rate (balls)' = 
        round(sum(BurgSpecH$isChase[BurgSpecH$Batter==Batter], na.rm=T)/
                sum(BurgSpecH$isBall[BurgSpecH$Batter==Batter], na.rm=T)
              ,3)*100,
      
      'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                          sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "InPlay")),3)*100,
      
      'Barrel rate' = round(sum(BurgSpecH$isBarrel[BurgSpecH$Batter==Batter], na.rm=T)/
                              sum(BurgSpecH$isInPlay[BurgSpecH$Batter==Batter], na.rm=T)  
                            ,3)*100
    )
  
  tableFilter <- reactive({table})
  datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
    formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  
})




output$rel_plot <- renderPlot({
  dataFilter <- reactive({
    BurgSpec %>%
       filter(Pitcher == input$ReleInput, TaggedPitchType != "Undefined")
    
  })
  
  
  ggplot(data = (if(nrow(dataFilter()) == 0)return() else(dataFilter())),
         aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
    xlim(-4,4) + ylim(0,8) + labs(color= "", title = "Release Point") +
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
    geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(size = 2, na.rm = TRUE) +
    theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
  
}, width = 350, height = 450)



output$contact_pts <- renderPlot({
  dataFilter <- reactive({
    
    if(input$ConPitchInput == "All" & input$ConAngleInput == "All"){
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay")
      
    } 
    else if(input$ConPitchInput == "All" & input$ConAngleInput == "<-10") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", Angle < -10)
      
    } 
    else if(input$ConPitchInput == "All" & input$ConAngleInput == "-10-0") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", -10 < Angle, Angle < 0)
    }
    
    else if(input$ConPitchInput == "All" & input$ConAngleInput == "0-10") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", 0 < Angle, Angle < 10)
    }
    
    else if(input$ConPitchInput == "All" & input$ConAngleInput == "10-20") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", 10 < Angle, Angle < 20)
    }
    
    else if(input$ConPitchInput == "All" & input$ConAngleInput == "20-30") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", 20 < Angle, Angle < 30)
    }
    
    else if(input$ConPitchInput == "All" & input$ConAngleInput == ">30") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", 30 < Angle)
    }
    
    else if(input$ConPitchInput != "All" & input$ConAngleInput == ">30") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", 30 < Angle, TaggedPitchType == input$ConPitchInput)
    }
    else if(input$ConPitchInput != "All" & input$ConAngleInput == "20-30") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", 20 < Angle, Angle < 30, TaggedPitchType == input$ConPitchInput)
    }
    else if(input$ConPitchInput != "All" & input$ConAngleInput == "10-20") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", 10 < Angle, Angle < 20, TaggedPitchType == input$ConPitchInput)
    }
    else if(input$ConPitchInput != "All" & input$ConAngleInput == "0-10") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", 0 < Angle, Angle < 10, TaggedPitchType == input$ConPitchInput)
    }
    else if(input$ConPitchInput != "All" & input$ConAngleInput == "-10-0") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", -10 < Angle, Andgle < 0, TaggedPitchType == input$ConPitchInput)
    }
    else if(input$ConPitchInput != "All" & input$ConAngleInput == "<-10") {
      BurgSpecH %>%
        filter(Batter == input$BatterInput, PitchCall == "InPlay", Angle < -10, TaggedPitchType == input$ConPitchInput)
    }
    
  })
  
  
  ggplot(data = (if(nrow(dataFilter()) == 0)return() else(dataFilter())),
         aes(x = ContactPositionZ, y = ContactPositionX, color = TaggedPitchType)) +
    xlim(-4,4) + ylim(-4,4) + labs(color= "", title = "Contact Point") +
     geom_segment(aes(x = -.708, y = .708, xend = .708, yend = .708), size = 1, color = "black") + 
     geom_segment(aes(x = -.708, y = .708, xend = -.708, yend = 0), size = 1, color = "black") + 
     geom_segment(aes(x = 0.708, y = .708, xend = .708, yend = 0), size = 1, color = "black") + 
     geom_segment(aes(x = 0, y = -.708, xend = 0.708, yend = 0), size = 1, color = "black") + 
     geom_segment(aes(x = 0, y = -0.708, xend = -0.708, yend = 0), size = 1, color = "black") +
    geom_point(size = 4, na.rm = TRUE) +
    theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
  
}, width = 500, height = 500)

}


shinyApp(ui = ui, server = server)

