################################
#### Load the libraries required
################################
library(shiny)
library(sp)
library(leaflet)
library(lubridate)
library(ggplot2)


###############################################
#### Load the spatial data object and data sets
###############################################
#### Spatial data
load(file="PDspace1.Rdata")
load(file="PDspace2.Rdata")
PDfinal <- rbind(PDfinal1, PDfinal2)

#### Page A
#ASE1 <- read.csv(file="SE1estimates.csv")
Acovid <- read.csv(file="covid19estimates.csv")


#### Page B
Bcases <- read.csv(file="postestweekly.csv")
Bdeaths <- read.csv(file="deathsweekly.csv")
Bcovid <- read.csv(file="covid19weekly.csv")
BSE1 <- read.csv(file="SE1weekly.csv")
Bcode <- read.csv(file="HBcodes.csv")


#### Page C
Cpep <- read.csv(file="pep.csv")



###################
#### User interface
###################
ui <- fluidPage(title = "Covid",
                tabsetPanel(
                  #########
                  #### Home
                  #########
                  tabPanel(title = "Home",
                           h2(tags$b("Visualisation app for the NHS 24 Covid-19 data.", style="color:royalblue")),
                           tags$br(),
                           tags$h3("The app provides the following visualisations."),
                           tags$br(),
                           tags$h3(
                             tags$h3(tags$b("(A) - ", style="color:royalblue"), "A map displaying the estimated proportions of NHS 24 calls classified as Covid-19."),
                             tags$br(),
                             tags$h3(tags$b("(B) - ", style="color:royalblue"), "Time series plots comparing the temporal trends in the NHS 24 calls, proportions of positive tests (cases) and deaths by Health Board."),
                             tags$br(),
                             tags$h3(tags$b("(C) - ", style="color:royalblue"), "A map displaying the latest posterior excedence probabilties (PEP) for NHS 24 calls classified as Covid-19.")
                           )

                           
                           
                  ),
                  
                  
                    ###################
                    #### Risk estimates
                    ###################
                    tabPanel(title = "(A) - Estimated proportions",
                             h2(tags$b("Estimated proportions of calls to NHS 24 being due to Covid-19.", style="color:royalblue")),
                             tags$br(),
                             sliderInput(inputId="Atime1", label="Week Beginning", min=ymd("2020-03-02"), max=ymd("2020-07-27"), value=ymd("2020-03-02"), step = 7, animate=FALSE, width='80%'),
                             leafletOutput("Amap1", height = 600, width='80%')
                    ),
                    
                    
                    
                    ########################
                    #### Comparison to cases
                    ########################
                    tabPanel(title = "(B) Comparing NHS 24 calls, proportions of positive tests (cases) and deaths",
                             h2(tags$b("Comparison of the temporal trends in the Covid-19 and SE1 telehealth activity with the proportions of positive Covid-19 tests (cases) and Covid-19 related deaths.", style="color:royalblue")),
                             tags$h3("The vertical scale is a measure of propensity, and is the value of the original variable divided by its maximum over the time period."),
                             tags$br(),
                             selectInput(inputId="BHB", label="Select a Health Board", 
                                        choices=c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife", "Forth Valley", 
                                                  "Greater Glasgow and Clyde",  "Grampian", "Highland", "Lanarkshire", "Lothian", "Tayside")),
                             plotOutput("Bplot", height=600)
                             
                    ),
                    
                    
                    
                    
                    #######################################
                    #### Posterior exceedance probabilities
                    #######################################
                    tabPanel(title = "(C) Posterior exceedance probabilities",
                             h2(tags$b("Posterior probabilities that the estimated proportions of calls to NHS 24 classified as Covid-19 are greater than a given threshold.", style="color:royalblue")),
                             tags$h3("The map relates to the week beginning 27th July 2020."),
                             tags$br(),
                             selectInput(inputId="Cpeplab", label="Select the statistic to visualise", 
                                         choices=c("Absolute PEP (0.2)", "Relative PEP (Scottish average)")),
                             tags$br(),
                             leafletOutput("Cmap1", height = 600, width='80%')

                 )
                )
)




################
#### Server page
################
server <- function(input, output, session) {
  #######################################
  #### page A - Spatial maps of estimates
  #######################################
  #### Specify the min and max scales for the map
  Amin <- min(Acovid[ ,-1])
  Amax <- max(Acovid[ ,-1])
  
  
  #### Specify the dates in the table
  Acolnames.temp <- colnames(Acovid)[-1]
  Acolnames <- gsub("\\.", replacement="-", x=substr(Acolnames.temp, 2, 11))
  
  
  #### Specify the variables to be mapped
  Adate1 <- reactive({as.character(input$Atime1)})
  Acolumn1 <- reactive({which(Adate1()==Acolnames) + 1})
  AvariableCovid <- reactive({Acovid[ ,Acolumn1()]})
  
  #Adate2 <- reactive({as.character(input$Atime2)})
  #Acolumn2 <- reactive({which(Adate2()==Acolnames) + 1})
  #AvariableSE1 <- reactive({ASE1[ ,Acolumn2()]})
  
  
  #### Create data frames of the two variables
  ADF1 <- reactive({data.frame(Postcode=Acovid[ ,1], var=AvariableCovid())})
  #ADF2 <- reactive({data.frame(Postcode=ASE1[ ,1], var=AvariableSE1())})
  
  
  #### Create the spatialpolygondataframes
  APDfinal1 <- reactive({merge(x=PDfinal, y=ADF1(), by.x="District", by.y="Postcode", all.x=FALSE)})
  #APDfinal2 <- reactive({merge(x=PDfinal, y=ADF2(), by.x="District", by.y="Postcode", all.x=FALSE)})
  
  #output$test <- renderPrint({c(Amin, APDfinal1()@data$var, Amax)})  
  

  #### Draw the maps
  ## Covid-19
  mapdata1 <- reactive({
      colours <- colorNumeric(palette = "YlOrRd", domain = c(Amin, APDfinal1()@data$var, Amax), reverse=FALSE)
      leaflet(data=APDfinal1()) %>% 
      addTiles() %>% 
      addPolygons(data=APDfinal1(), fillColor = ~colours(APDfinal1()@data$var), color="", weight=1, fillOpacity = 0.7) %>%
      addLegend(pal = colours, values = c(Amin, APDfinal1()@data$var, Amax), opacity = 1, title="Proportion") %>%
      addScaleBar(position="bottomleft")
      })
    output$Amap1 <- leaflet::renderLeaflet({mapdata1()})
    
  ## SE1
  #mapdata2 <- reactive({
  #    colours <- colorNumeric(palette = "YlOrRd", domain = c(Amin, APDfinal2()@data$var, Amax), reverse=FALSE)
  #    leaflet(data=APDfinal1()) %>% 
  #      addTiles() %>% 
  #      addPolygons(data=APDfinal1(), fillColor = ~colours(APDfinal2()@data$var), color="", weight=1, fillOpacity = 0.7) %>%
  #      addLegend(pal = colours, values = c(Amin, APDfinal2()@data$var, Amax), opacity = 1, title="Proportion") %>%
  #      addScaleBar(position="bottomleft")
  #  })
  #  output$Amap2 <- leaflet::renderLeaflet({mapdata2()})
    
    
    
    ###############################
    #### page B - Time series plots
    ###############################
    #### Specify the HB
    BHBname <- reactive({as.character(input$BHB)})
    BHBcode <- reactive({Bcode$code[Bcode$name==BHBname()]})
    
    
    #### Create the new data set
    Bdata <- reactive({
                      temp.case <- Bcases[ ,colnames(Bcases)==as.character(BHBcode())]  
                      temp.deaths <- Bdeaths[ ,colnames(Bdeaths)==as.character(BHBcode())]  
                      temp.covid <- Bcovid[ ,colnames(Bcovid)==as.character(BHBcode())]  
                      temp.SE1 <- BSE1[ ,colnames(BSE1)==as.character(BHBcode())]
                      n.week <- length(temp.SE1)
                      temp.df <- data.frame(Week=ymd(rep(Bdeaths$Week,4)), Propensity=c(temp.case, temp.deaths, temp.covid, temp.SE1), Event=c(rep("Positive tests", n.week), rep("Deaths", n.week), rep("NHS 24 Covid-19", n.week), rep("NHS 24 SE1" , n.week)))
    return(temp.df)  
    })
    
    
    
    #### Create the plot
    timeplotB <- reactive({ggplot(Bdata(), aes(x=Week, y=Propensity, group=Event)) +
      geom_line(aes(color=Event)) +
      scale_colour_discrete(name  ="Event type") +
      scale_x_date(date_labels = "%d-%m-%Y", breaks = as.Date(c("2020-03-02", "2020-04-06", "2020-05-04", "2020-06-01", "2020-07-06", "2020-07-27"))) + 
      scale_y_continuous(name="Covid-19 propensity") + 
      theme(text=element_text(size=16), plot.title=element_text(size=16, face="bold"))}) 
    output$Bplot <- renderPlot(timeplotB())
    
    
    
    #################################    
    #### page C - Maps of differences
    #################################
    #### Create the spatialpolygondataframe
    CPDfinal <- reactive({merge(x=PDfinal, y=Cpep, by.x="District", by.y="Post.Code", all.x=FALSE)})
    
    
    #### Specify the min and max scales for the map
    Cminmax <- reactive({
                      if(input$Cpeplab == "Absolute PEP (0.2)")
                      {
                      Cmintemp1 <- min(Cpep$absolute)
                      Cmaxtemp1 <- max(Cpep$absolute) 
                      }else
                      {
                      Cmintemp1 <- min(Cpep$relative)
                      Cmaxtemp1 <- max(Cpep$relative) 
                      }
                      return(c(Cmintemp1, Cmaxtemp1))
    })

    #### Create the variables
    Cvarcovid <- reactive({
                          if(input$Cpeplab == "Absolute PEP (0.2)")
                          {
                          Ctemp1 <- CPDfinal()@data$absolute   
                          }else
                          {
                          Ctemp1 <- CPDfinal()@data$relative  
                          }
                          return(Ctemp1)  
    })
    
    #CvarSE1 <- reactive({
    #  if(input$Cdiff == "Estimated difference")
    #  {
    #    Ctemp2 <- CPDfinal()@data$SE1est   
    #  }else
    #  {
    #    Ctemp2 <- CPDfinal()@data$SE1probinc 
    #  }
    #  return(Ctemp2)  
    #})
    #output$Ctest <- renderPrint(CvarSE1())
    
    #### Create the maps
    ## Covid-19
    Cmapdata1 <- reactive({
      colours <- colorNumeric(palette = "YlOrRd", domain = c(Cminmax(), Cvarcovid()), reverse=FALSE)
      leaflet(data=CPDfinal()) %>% 
        addTiles() %>% 
        addPolygons(data=CPDfinal(), fillColor = ~colours(Cvarcovid()), color="", weight=1, fillOpacity = 0.7) %>%
        addLegend(pal = colours, values = c(Cminmax(), Cvarcovid()), opacity = 1, title="") %>%
        addScaleBar(position="bottomleft")
    })
    output$Cmap1 <- leaflet::renderLeaflet({Cmapdata1()})
    
    ## SE1
    #Cmapdata2 <- reactive({
    #  colours <- colorNumeric(palette = "YlOrRd", domain = c(Cminmax(), CvarSE1()), reverse=FALSE)
    #  leaflet(data=CPDfinal()) %>% 
    #    addTiles() %>% 
    #    addPolygons(data=CPDfinal(), fillColor = ~colours(CvarSE1()), color="", weight=1, fillOpacity = 0.7) %>%
    #    addLegend(pal = colours, values = c(Cminmax(), CvarSE1()), opacity = 1, title="") %>%
    #    addScaleBar(position="bottomleft")
    #})
    #output$Cmap2 <- leaflet::renderLeaflet({Cmapdata2()})
  }



########################
#### Run the application 
########################
shinyApp(ui = ui, server = server)
