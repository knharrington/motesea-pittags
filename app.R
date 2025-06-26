##############################  GLOBAL  ########################################
library(tidyverse)
library(data.table)
library(plyr)
library(bslib)
library(ggsvg)

#########PIT TAG ANTENNA DATA#########
#Get the file names of all the raw txt files saved in the folder of data to be imported (change path as necessary)

ORMR.files = list.files(path=paste0("data"), pattern="*.txt", full.names=T)

##### FOR MULTIREADER DATA#####  
#Use a loop to create a raw dataframe for each file in the folder to be imported and create columns in the dataframe to specify creek, date, and Antenna
filenames = as.vector(NA) #create a dummy vector used in the loop

for (i in 1:length(ORMR.files)) {
  file_name = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=6, end=-5)
  filenames[[i]] = file_name
  file_df = read.table(ORMR.files[i], header=F, fill=T, col.names = paste0("V", seq_len(16)))
  file_df$System = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=6, end=7)
  file_df$ReadDate = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=9, end=15)
  file_df$Antenna = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=17, end=-5)
  assign(file_name, file_df, envir = .GlobalEnv)
}

#Identify the PIT tag data in each dataframe, merge them, and relabeled columns appropriately
dflist =as.list(NA) #creates a dummy list that the loop below can fill with all the dataframes
PITlist = as.list(NA) #creates a dummy list to fill with the PIT tag data within the list of dataframes
Errorlist = as.list(NA)

for (i in 1:length(filenames)) {
  dflist[[i]] = get(filenames[i])
  PITlist[[i]] = dflist[[i]][which(dflist[[i]]$V1=='S'),]
  Errorlist[[i]] = dflist[[i]][which(dflist[[i]]$V1=='E'),]
}

ORMR.raw = droplevels(rbindlist(PITlist, fill=T)) #Combine the list of dataframes into one dataframe

ORMR.raw = within(ORMR.raw, rm(V11, V12, V13, V14, V15, V16)) #delete empty columns
ORMR.raw = plyr::rename(ORMR.raw, c("V1"="Code", "V2"="Date", "V3"="Time", "V4"="Time_Reference", "V5"="Duration", "V6"="Tag_Type", "V7"="Loop", "V8"="Tag_ID",
                                    "V9"="Site_Code", "V10"="Effective_Amps")) #rename the columns
###NOTE that Number consecutive detections seems to be missing (between 9 and 10)
ORMR.raw$Loop = paste(ORMR.raw$Antenna, "-", ORMR.raw$Loop)

##############################  UI  ############################################

card1 <- card(
  div(style = "text-align: center;", h3(strong("Today's Experiment"))),
  h4(em("Objective")),
  "Determine fish habitat preference by comparing behavior in various environments",
  br(), br(),
  div(style = "text-align: center;", h3(strong("About the Habitat"))),
  h5("Species"),
  h5("Common Name")
  
)

card2 <- card(
  card_body(
    max_height=225,
    div(style = "text-align: center;", h3(em(strong("Which habitat does this fish prefer?")))),
    plotOutput("prefer")
  ),
  card_body(
    
  )
)

card3 <- card(
  div(style = "text-align: center;", h3(strong("About the Fish"))),
  h5("Species"),
  h5("Common Name"),
  h5("Age"),
  h5("Fork Length"),
  h5("Weight"),
  br(),
  h4(em("Fun Fact")),
  "Snook can do cool things."
)

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  div(style = "text-align: center;", h1(strong("Fisheries Ecology & Enhancement"))),
  div(style = "text-align: center;", h2("Habitat Choice Experiment")),
  
  layout_column_wrap(
    width=NULL, style=bslib::css(grid_template_columns="1fr 2fr 1fr"),
    card1, card2, card3
  )
  
) # end ui

##############################  SERVER  ########################################

server <- function(input, output) {
  
  snook_svg <- paste(readLines("www/snook.svg"))
  
  test_df <- data.table(x=3, y=1)
  
  output$prefer <- renderPlot({
    ggplot(test_df) +
      geom_point(aes(x=1,y=1), color="transparent") +
      geom_point(aes(x=10,y=1), color="transparent") +
      
      geom_point_svg(aes(x,y), svg=snook_svg, size=60) +
      geom_segment(aes(x=x, xend=x, y=-0.2, yend=0.2), size=1.5, color="black") +
      
      geom_segment(aes(x=0.5, xend=10.5, y=0, yend=0), color = "black", size=1.5) +
      geom_segment(aes(x = 10.2, xend = 10.5, y = 0, yend = 0),
        arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
        size = 1.5, color = "black") +
      geom_segment(aes(x = 0.8, xend = 0.5, y = 0, yend = 0),
        arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
        size = 1.5,color = "black") +
      
      scale_x_continuous(expand=c(0,0), breaks=seq(0,11,1), limits=c(0,11))+
      scale_y_continuous(expand=c(0,0),limits=c(-0.25,2)) +
      
      theme_void() #+
      # theme(#axis.line.x = element_line(color="black", linewidth=1.5, arrow=arrow(ends="both")),
      #       #axis.ticks.x = element_line(color="blue", linewidth=1.5),
      #       #axis.ticks.length.x = unit(0.3,"cm"),
      #       #plot.margin = margin(10,10,10,10)#,
      #       #plot.background = element_rect(fill="lightblue")
      #       )
  })
  
} # end server

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)