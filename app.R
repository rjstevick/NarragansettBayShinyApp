################################################################################
#
# Shiny app for BIO594
# Rebecca Stevick
# Updated 2/2019
# Data from Narragansett Bay Oyster Survey - Aug 2017 and Ashley Hamilton
# Contact: rstevick@my.uri.edu
# 
# To upload: rsconnect::deployApp("~/Documents/URI/Research/Overview_NBayStations_ShinyApp/NBay_ShinyApp")
################################################################################



## Load packages -------------------------

library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(slickR)
library(gridExtra)
library(png)
library(grid)
library(reshape2)
library(dichromat)
library(shinythemes)
library(shinycssloaders)
library(shinyalert)
library(shinyBS)



## Read in data and calculate means ------------------------------------------

data<-read_excel("TNC_PJ_AllSamples_Metadata.xlsx", sheet="ALLDATA")
Stationmeans<- data %>%
  group_by(Station, StationName, Lat, Long) %>%
  summarise(meanT=mean(Temp_avg),meanS=mean(Salinity_avg),meanpH=mean(pH_avg),
                          meanChl=mean(Chl_ugL_avg),meanDO=mean(DO_mgL_avg),meanNH4=mean(ammonium_uM),
                          meanDermo=mean(DermoConc),meanPO4=mean(phosphate_uM),
                          meanNO2=mean(nitrite_uM),meanNO3=mean(nitrate_uM))
Stationmeans$meanIndex <- c("Low","None","High","Medium","Low","None","None")

# Define UI for application ---------------------------------------------------
ui <- dashboardPage(

   # Application title
   dashboardHeader(title="Choose an environmental parameter",
                   titleWidth = 400),
   
   # Sidebar with an input
   dashboardSidebar(width=400, collapsed = TRUE,
                    
     # add some styling for the padding and the pop up
     div(style="padding-left:20px",     
         
         # Prompt for selection
         h3("Select an environmental feature to show on the map and in the graph below:"),
         h6("These measurements are daily averages from the week of sampling in August 2017."),

         # Vertical layout with radiobuttons and a plot
         fluidRow(fluid=TRUE,width=400,
              # Define rediobutton choices and outputs
              radioButtons(inputId = "param",
                         label=NULL,
                         choiceNames = list(
                           "Temperature","Salinity","pH","Chlorophyll-a","Dermo Disease Levels",
                           "Dissolved Oxygen","Ammonium","Phosphate","Nitrite","Nitrate"),
                         choiceValues = list(
                           "meanT", "meanS", "meanpH", "meanChl","meanDermo","meanDO",
                           "meanNH4","meanPO4","meanNO2","meanNO3")),
              br(),
              # Plot environ param based on selection
              column(h4("How does the Bay change from North to South?"),
              plotOutput("plot", width = 370),
              br(),
              h4("Learn more about how we measure parameters in the ocean at this website:", 
                 a(href="https://coast.noaa.gov/estuaries/science-data/","https://coast.noaa.gov/estuaries/science-data/")), width=12)

              )
         )
     ),
      
      # Show the map in the main panel
      dashboardBody(
        
         div(style="padding-left:10px",   
             
             tags$head(tags$style("#modal{font-size: 16px;
                                         }"
             )
             ),
             
         
         # Prompt for map
         h2("Click on a station to learn more about its oysters!"),
         h4("Oysters were collected from 7 different stations throughout Narragansett Bay shown on the map below."),
         h4("To show a different environmental parameter on the map, click on the menu above."),
         leafletOutput("map"),
         bsPopover("map", title="To show a different environmental parameter on the map, click on the menu above.",
           trigger="hover", placement="top"),
         br(),
         # Row layout for slideshow and plots
         
         fluidRow(
             # Slideshow of photos from selected station
             popify(
                 column(verticalLayout(htmlOutput("title"),
                                       tags$head(tags$style("#title{font-size: 16px;
                                                                    text-align: justify;
                                                              text-justify: inter-word;
                                                             }")),
                                       br(),
                                       slickROutput("photos"),
                                       br(),
                                       h3("  ")), width=5), 
                 title="Click on the arrows to scroll through photos of this station and the oysters.", 
                 trigger="hover", placement="top"),
             # Plot of oyster data from selected station
             column(plotOutput("oysterplot"),
                    h4("Each dot on this graph is the value of a single oyster. The red dotted line shows the 
                       average value (mass, width, or length). The background photo is of an oyster from this Station.
                       How does the average oyster size change between stations?
                       Click on another point on the map to find out!", align="justify"),
                    h4("To find out more about why oysters are important, visit this website:", a(href="https://www.chesapeakebay.net/issues/oysters","https://www.chesapeakebay.net/issues/oysters")),
                    h3(" "),
                    width=5),
             column(plotOutput("diseaseplot"), 
                    h4("Dermo disease is a parasitic infection that does not affect humans. We measured the amount of 
                       the parasite in each oyster to find the infection level. High infections are red and no 
                       infections are grey. How does the infection level change between stations?", align="justify"),
                    h4("To learn more about Dermo, visit this link: ", a(href="https://www.vims.edu/_docs/oysters/oyster-diseases-CB.pdf","https://www.vims.edu/_docs/oysters/oyster-diseases-CB.pdf")), 
                    h3(" "),
                    width=2),
             br()
         ),
         fluidRow(
           column(verticalLayout(
             br(),
             imageOutput("logos", inline = TRUE),
             h5("Developed by", a(href="https://mobile.twitter.com/rjstevick","Rebecca Stevick (URI-GSO)"), align="center"),
             h5("Collaborators: Marta Gomez-Chiarri, Anton Post, Stephanie Spada, Ashley Hamilton, Serena Moseman-Valtierra, Roxanna Smolowitz", align="center"),
             h5("This work is supported by funding from the National Science Foundation Graduate Research Fellowship Program, Rhode Island Science and Technology Advisory Council,
                the University of Rhode Island Coastal Institute, The Nature Conservancy of RI - TNC Global Marine Team, 
                and the Blount Family Shellfish Restoration Foundation. This material is based upon work conducted at a Rhode Island NSF EPSCoR research facility, 
                the Genomics and Sequencing Center, supported in part by the NSF EPSCoR Cooperative Agreement #OIA-1655221.", align="center")),
             width=8, offset=2)
         )
      ),
   
   fluidPage(
   # Set up shinyalert
   useShinyalert()
   )
   
))



# Define server logic -----------------------------------
server <- function(input, output, session) {
  
    ## Shiny Alert welcome -----------------------------------------  
    
   # Show a modal when the app opens
   shinyalert(title="Welcome to Narragansett Bay!", 
              text="Oysters are important organisms in coastal environments. They provide habitat for fish and eels, protect shorelines, and can filter up to 36 Olympic swimming pools of water every day! We are exploring the health of oyster communities at 7 stations in Narragansett Bay and how they are affected by the environment.
              
              To start, click on one of the colored points on the map.",
              confirmButtonText="Let's go!",
              confirmButtonCol="#008B8B", animation=TRUE,timer=20000)
    
    
   ## Make a reactive value to store click position ---------------------------------------------
   click_data <- reactiveValues(clickedMarker=NULL)
  
   
   ## Plot map and add markers based on input----------------------------------------------------
   output$map <- renderLeaflet({
     
     # render the plot only when input$param is not empty
     req(input$param)
     
     # define variables based on radiobutton input (input$param)
     if(input$param=="meanT"){x<-Stationmeans$meanT; un<-" \u00B0F"; t<-"Mean <br> Temperature (\u00B0F)"}
     if(input$param=="meanS"){x<-Stationmeans$meanS; un<-" ppt"; t<-"Mean Salinity (ppt)"}
     if(input$param=="meanpH"){x<-Stationmeans$meanpH; un<-""; t<-"Mean pH"}
     if(input$param=="meanChl"){x<-Stationmeans$meanChl; un<-" \U003BCg/L"; t<-"Mean Chlorophyll-a <br> concentration (\U003BCg/L)"}
     if(input$param=="meanDermo"){x<-Stationmeans$meanDermo; un<-" cells"; t<-"Mean Dermo cell <br> concentration (cells/oyster)"}
     if(input$param=="meanDO"){x<-Stationmeans$meanDO; un<-" mg/L"; t<-"Mean Dissolved <br> Oxygen (mg/L)"}
     if(input$param=="meanNH4"){x<-Stationmeans$meanNH4; un<-" \U003BCM"; t<-"Mean Ammonium (\U003BCM)"}
     if(input$param=="meanPO4"){x<-Stationmeans$meanPO4; un<-" \U003BCM"; t<-"Mean Phosphate (\U003BCM)"}
     if(input$param=="meanNO2"){x<-Stationmeans$meanNO2; un<-" \U003BCM"; t<-"Mean Nitrite (\U003BCM)"}
     if(input$param=="meanNO3"){x<-Stationmeans$meanNO3; un<-" \U003BCM"; t<-"Mean Nitrate (\U003BCM)"}
     
     # define palette based on input$param --> now x
     # red=warm/high, blue=cold/low
     pal <- colorNumeric(palette = colorschemes$DarkRedtoBlue.18, domain = x)
     # define reverse palette based on input$param --> now x
     # this will be used in the legend to get higher values on the top of the scale. 
     palr <- colorNumeric(palette = colorschemes$DarkRedtoBlue.18, domain = x, reverse=TRUE)
     
     # plot the leaflet map based on mean data
     leaflet(Stationmeans) %>% 
       
       # define map style - Hydda is a nice blue/white background
       addProviderTiles(providers$Hydda) %>%
       
       # define map view if needed, will go to default if not
           # setView(lng = -71.5, lat = 41.6, zoom = 9.4) %>%
       
       # add markers at Station locations
       addCircleMarkers(Stationmeans$Long, Stationmeans$Lat,
                        # hover popup shows the value of input$param
                        label = Stationmeans$StationName, 
                        # when user clicks, popup showing station name
                        popup = paste(t,"=", format(round(x,2))),
                        # color based on input$param, using the defined pal
                        color = ~pal(x), stroke = TRUE, opacity=100, radius=8) %>%
       
       # add legend in bottom right corner
       # use reverse palette and reverse values using transform
       addLegend("bottomright", pal = palr, values = x,
                 # add title of legend, from variable t
                 title = t, opacity = 1, 
                 # add units for input$param, from variable un
                 labFormat = labelFormat(suffix=un, digits=2, 
                 transform = function(x) sort(x, decreasing = TRUE))
       )
   })
   
   
   ## Add plot of selected parameter based on radiobuttons input ------------------------------------
   output$plot <- renderPlot({
     
     # render the plot only when input$param is not empty
     req(input$param)
     
     # define variables based on radiobutton input (input$param)
     if(input$param=="meanT"){x<-Stationmeans$meanT; t<-"Mean Temperature (\u00B0F)"; 
     subt<-"Temperature is warmest in the \nshallow coves."}
     if(input$param=="meanS"){x<-Stationmeans$meanS; t<-"Mean Salinity (ppt)"; 
     subt<-"Salinity generally increases down the \nBay, except in the Narrow River \nwhere it is less saline."}
     if(input$param=="meanpH"){x<-Stationmeans$meanpH; t<-"Mean pH"; 
     subt<-"pH increases down the Bay."}
     if(input$param=="meanChl"){x<-Stationmeans$meanChl; t<-"Mean Chlorophyll-a \nconcentration (\U003BCg/L)"; 
     subt<-"Chlorophyll-a is a measurement of \nhow much photosynthesis is \nhappening. It is highest at Stations \n2 and 5."}
     if(input$param=="meanDermo"){x<-Stationmeans$meanDermo; t<-"Mean Dermo cell \nconcentration (cells/oyster)"; 
     subt<-"Dermo disease infections are highest \nat Bissel Cove. You can also see the \ninfection level per station in the graph \nbelow the map."}
     if(input$param=="meanDO"){x<-Stationmeans$meanDO; t<-"Mean Dissolved Oxygen \n(mg/L)"; 
     subt<-"Dissolved oxygen in the water \nincreases down the Bay."}
     if(input$param=="meanNH4"){x<-Stationmeans$meanNH4; t<-"Mean Ammonium (\U003BCM)"; 
     subt<-"Ammonium is a dissolved nitrogen \ncompound. It is highest at Stations \n3 and 5."}
     if(input$param=="meanPO4"){x<-Stationmeans$meanPO4; t<-"Mean Phosphate (\U003BCM)"; 
     subt<-"Phosphate concentrations are very \nhigh in the Northern stations and \ndecrease down the Bay."}
     if(input$param=="meanNO2"){x<-Stationmeans$meanNO2; t<-"Mean Nitrite (\U003BCM)"; 
     subt<-"Nitrite decreases down the Bay."}
     if(input$param=="meanNO3"){x<-Stationmeans$meanNO3; t<-"Mean Nitrate (\U003BCM)"; 
     subt<-"Nitrate decreases down the Bay."}

     # Make new variable with wrapped x-labels
     Stationmeans$newNames = str_wrap(Stationmeans$StationName, width = 20)
     # Reorder stations so that they plot North to South
     Stationmeans$newNames <- factor(Stationmeans$newNames, levels = Stationmeans$newNames)

     # Plot stations vs. parameter selected
     theme_set(theme_classic())
     ggplot(Stationmeans, aes(newNames, x, fill=x))+ 
         geom_bar(stat="identity",color="grey")+
         # color bars based on palette used in map
         scale_fill_gradientn(colours= colorschemes$DarkRedtoBlue.18)+
         # change size of text
         theme(axis.text.x=element_text(size = 14, colour = "gray25"), 
               axis.text.y=element_text(size = 14, colour = "gray25"),
               legend.position = "none",
  #             plot.margin = unit(c(0.5, 1, 1, 1), "cm"),
               plot.title = element_text(size = 17, face = "bold"),
               plot.subtitle = element_text(size=14))+
         scale_y_continuous(position="right")+
         scale_x_discrete(limits = rev(levels(Stationmeans$newNames)))+
         labs(y=NULL, x=NULL) + ggtitle(label=t, subtitle=subt)+
         coord_flip()
   }, execOnResize = TRUE)


   
   ## Store the user click -----------------------------------------------------------------------
   observeEvent(input$map_marker_click,{
     # observe click only if input$map_marker_click is not empty
     req(input$map_marker_click)
     
     #make new variable in click_data with current clicker marker
     click_data$clickedMarker <- input$map_marker_click
     
     # get Station name
     longgg<-click_data$clickedMarker$lng
     locdata<-filter(data, Long==longgg)
     
     # Show a modal when the button is pressed
     shinyalert(paste("You've selected ", locdata$StationName[1]),
                "Below the map, you will find a slideshow of photos from this station, the average length, width, and mass of the oysters collected, and the Dermo disease infections at this station. Read the captions below the graphs to learn more.", 
                confirmButtonCol="#008B8B")
   })
   
   
   ## Make plots of oyster data from a station based on clicked point -----------------------------
   output$oysterplot<-renderPlot({
     
     # render the plot only when input$map_marker_click is not empty
     req(input$map_marker_click)
     
     # filter out data based on click location longitude
     longgg<-click_data$clickedMarker$lng
     locdata0<-filter(data, Long==longgg)
     # store location name
     location<- locdata0$StationName[1]
     # define title of plot a
     title<- paste("Length, width, and mass of oysters")
     
     # remove first 6 columns of data from the location's data
     locdata <- locdata0[ -c(1:6)]
     # remove columns with no data, so it doesn't error out when plotting
     newloc<- locdata
     #[!sapply(locdata, function(x) all(is.na(x)))]
     
     #calculate means to plot red red 
     newloc2_means<-melt(summarise_all(newloc, mean))
     colnames(newloc2_means)<-c("param","value")
     #remove all but width,length,mass
     newloc2_means<-newloc2_means[c(4:6),]
     
     # if loop to remove means for Point Judith samples - no width/length/mass data was collected
     #if(click_data$clickedMarker$lng=="-71.5"){newloc2_means<-as.data.frame(matrix(data=NA,nrow=3,ncol=2));
     #          colnames(newloc2_means)<-c("param","value");
     #          newloc2_means[,1]<-c("Mass_g","Width_mm","Length_mm");
     #          newloc2_means[,2]<-c(-100,-100,-100)}
     
     # gather columns from mass to length
     # then make new variable to facet out length, width, mass in plot a
     newloc2<- gather(locdata0, param, value, "Mass_g":"Length_mm")
     newloc2$param<-factor(newloc2$param, levels=c("Length_mm", "Width_mm", "Mass_g"))
     
     # Define photo to use as background
     
     if(click_data$clickedMarker$lng=="-71.390574"){pic<-readPNG("Oyster/Slide1.PNG")} #PVD
     if(click_data$clickedMarker$lng=="-71.445185"){pic<-readPNG("Oyster/Slide2.PNG")} #GB
     if(click_data$clickedMarker$lng=="-71.431168"){pic<-readPNG("Oyster/Slide3.PNG")} #BIS
     if(click_data$clickedMarker$lng=="-71.452911"){pic<-readPNG("Oyster/Slide4.PNG")} #NAR
     if(click_data$clickedMarker$lng=="-71.689007"){pic<-readPNG("Oyster/Slide6.PNG")} #NIN
     if(click_data$clickedMarker$lng=="-71.5"){pic<-readPNG("Oyster/Slide6.PNG")} # PJN
     if(click_data$clickedMarker$lng=="-71.51"){pic<-readPNG("Oyster/Slide6.PNG")} # PJS
     # read in image and store as "g"
     g <- rasterGrob(pic, interpolate=TRUE) 
     number_ticks <- function(n) {function(limits) pretty(limits, n)}
     
     
     # plot length, width, and mass of the oysters
     ggplot(newloc2, aes(x=OysterNumber, y=value))+ 
       annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
       geom_jitter(size=3)+
       # add mean values
                #geom_point(data=newloc2_means, mapping=aes(x = 15, y = value), col="red", size=3)+
       geom_abline(data=newloc2_means, mapping=aes( intercept = value, slope=0), col="red", 
                   size=1.5, lty="twodash")+
       # add arrow pointing out mean value
       geom_curve(data=newloc2_means,mapping=aes(x = 15, y = value-40, xend = 10, yend = value), 
                    colour = "red", size=1, curvature = 0.2,
                    arrow = arrow(length = unit(0.1, "npc"),type="open"))+ 
       # add label for mean value
       geom_label(data=newloc2_means,mapping=aes(x = 15, y = value-44, 
                 label = paste("Average \n", c(Mass_g="mass (g)", Width_mm="width (mm)", Length_mm="length (mm)"))), 
                 hjust = 0.5,vjust = 0.5, 
                 lineheight = 0.8,colour = "red", 
                 fill = alpha("white",0.4),label.size = NA,
                 size = 4) +
       facet_wrap(~param, scales="free", strip.position= c("left"),
                  labeller=as_labeller(c(Mass_g="Mass (g)", Width_mm="Width (mm)", Length_mm="Length (mm)")))+
       ggtitle(title) +
       theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),
             axis.text.y = element_text(size=12),axis.ticks.x = element_blank(),
             strip.background = element_blank(), strip.placement = "outside",
             strip.text.y = element_text(size = 14, colour = "gray25"),
             plot.title = element_text(size = 17, face = "bold"),
             plot.background = element_rect(fill = "gray95"),
             plot.caption = element_text(size=17, hjust=0))+
       scale_y_continuous(limits=c(0,370),breaks=number_ticks(10))
     
   })
     
    
   ## Make plots of disease data from a station based on clicked point -----------------------------
   output$diseaseplot<-renderPlot({
     
     # render the plot only when input$map_marker_click is not empty
     req(input$map_marker_click)
     
     # filter out data based on click location longitude
     longgg<-click_data$clickedMarker$lng
     locdata0<-filter(data, Long==longgg)
     # store location name
     location<- locdata0$StationName[1]
     # define title of plot a
     title<- paste("Length, width, and mass of oysters")
     
     # remove first 6 columns of data from the location's data
     locdata <- locdata0[ -c(1:6)]
     # remove columns with no data, so it doesn't error out when plotting
     newloc<- locdata
     #[!sapply(locdata, function(x) all(is.na(x)))]
     
     #calculate means to plot red red 
     newloc2_means<-melt(summarise_all(newloc, mean))
     colnames(newloc2_means)<-c("param","value")
     #remove all but width,length,mass
     newloc2_means<-newloc2_means[c(4:6),]
     
     # if loop to remove means for Point Judith samples - no width/length/mass data was collected
     #if(click_data$clickedMarker$lng=="-71.5"){newloc2_means<-as.data.frame(matrix(data=NA,nrow=3,ncol=2));
     #          colnames(newloc2_means)<-c("param","value");
     #          newloc2_means[,1]<-c("Mass_g","Width_mm","Length_mm");
     #          newloc2_means[,2]<-c(-100,-100,-100)}
     
     # gather columns from mass to length
     # then make new variable to facet out length, width, mass in plot a
     newloc2<- gather(locdata0, param, value, "Mass_g":"Length_mm")
     newloc2$param<-factor(newloc2$param, levels=c("Length_mm", "Width_mm", "Mass_g"))
     
     # plot percent dermo by disease group
     ggplot(newloc, aes(x=DateCollected, fill=DermoIndex))+geom_bar(position="fill")+
       ggtitle("Percent Dermo \nprevalence")+ 
       #ylab("Percent Dermo by group (%)")+
       theme(axis.text.x = element_blank(),axis.title.x = element_blank(),
             axis.title.y = element_blank(),axis.ticks.x = element_blank(),
             axis.text.y = element_text(size=12),
             plot.title = element_text(size = 17, face = "bold"), legend.position = "top",
             legend.title = element_blank(), legend.text = element_text(size=14, colour="gray25"),
             legend.background = element_rect(fill="gray90"),
             plot.background = element_rect(fill = "gray95"),
             plot.caption = element_text(size=17, hjust=0)) +
       scale_fill_manual(values=c(High="red",Medium="orange",Low="yellow", None="dark grey"))+
       guides(fill=guide_legend(nrow=2,byrow=TRUE))+
       scale_y_continuous(labels = scales::percent)

     
   }, execOnResize = TRUE)
   
   
   ## Add title of user-selected site above slide show -------------------------------------------
   output$title<-renderText({

     # render the title only when input$map_marker_click is not empty
     req(input$map_marker_click)

     
     if(click_data$clickedMarker$lng=="-71.390574"){
       caption<-"<b> 1. Providence River - Bold Point Park. </b> This station is a closed shellfishing area where we see the most impact by humans. 
       There are high levels of nutrients in the water, low oxygen, and moderate levels of disease. These oysters are older and bigger than any of the other stations. "} #PVD
     if(click_data$clickedMarker$lng=="-71.445185"){
       caption<-"<b> 2. Greenwich Bay - Goddard State Park. </b> This station is a closed shellishing area that has high Chlorophyll-a and no Dermo disease."} #GB
     if(click_data$clickedMarker$lng=="-71.431168"){
       caption<-"<b> 3. Bissel Cove. </b> This station has the highest levels of Dermo disease and ammonium. These oysters are the heaviest in the Bay."} #BIS
     if(click_data$clickedMarker$lng=="-71.452911"){
       caption<-"<b> 4. Narrow River. </b> This station has the warmest temperatures and lowest salinity. These oysters are small and look like rocks!"} #NAR
     if(click_data$clickedMarker$lng=="-71.5"){
       caption<-"<b> 5. Point Judith Pond - Billington Cove. </b> This station has the highest salinity and Chlorophyll-a. These oysters are from a shellfish farm and are not infected with Dermo disease."} # PJN
     if(click_data$clickedMarker$lng=="-71.51"){
       caption<-"<b> 6. Point Judith Pond - Bluff Hill Cove. </b> This station has the lowest temperature and high salinity. These oysters are from a shellfish farm and are not infected with Dermo disease."} # PJS
     if(click_data$clickedMarker$lng=="-71.689007"){
       caption<-"<b> 7. Ninigret Pond. </b> This station has the highest levels of dissolved oxygen, pH, and very little nutrients. 
       These oysters are from a shellfish farm and are about 2 years old."} #NIN

     paste(caption)
     
   })
   
   
   ## Add slideshow of photos from user-selected site --------------------------------------------
   output$photos<-renderSlickR({

     # render the slideshow only when input$map_marker_click is not empty
     req(input$map_marker_click)
     
     # define new variable pic based on longitude selected
     if(click_data$clickedMarker$lng=="-71.390574"){pic<-as.character("01_PVD/")} #PVD
     if(click_data$clickedMarker$lng=="-71.445185"){pic<-as.character("02_GreenwichBay/")} #GB
     if(click_data$clickedMarker$lng=="-71.431168"){pic<-as.character("03_BisselCove/")} #BIS
     if(click_data$clickedMarker$lng=="-71.452911"){pic<-as.character("04_NarrowRiver/")} #NAR
     if(click_data$clickedMarker$lng=="-71.689007"){pic<-as.character("05_Ninigret/")} #NIN
     if(click_data$clickedMarker$lng=="-71.5"){pic<-as.character("06_PointJudith/")} # PJ
     if(click_data$clickedMarker$lng=="-71.51"){pic<-as.character("06_PointJudith/")} # PJS
     
     # get all file/photo names from pic folder
     imgs <- list.files(pic, full.names = TRUE)
     # output a slideshow with all photos in folder from location
     slickR(imgs, height=NULL)
     
   })
   
   
   ## Add funding logos --------------------------
   output$logos <- renderImage({
     # When input$n is 1, filename is ./images/image1.jpeg
     filename <- "logosline.png"
     
     # Return a list containing the filename
     list(src = filename, width = "100%")
     
   }, deleteFile = FALSE)
   
}


# Run the application! ----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

