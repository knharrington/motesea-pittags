##############################  GLOBAL  ########################################
library(plyr)
library(tidyverse)
library(data.table)
library(ggsvg)

library(shiny)
library(bslib)
library(showtext)
library(thematic)

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

# current color is fill:#007bc2 which is primary blue
snook_svg <- paste(readLines("www/snook.svg"), collapse = "\n")

# ui facts
{
  hab_sp = "Rhizophora mangle"
  hab_cn = "Red Mangrove"
  fish_sp = "Centropomus undecimalis"
  fish_cn = "Common Snook"
  fish_age = "11 months"
  fish_fl = "10 inches"
  fish_w = "0.7 pounds"
  fun_fact = "Snook are popular game and food fish."
}

##############################  UI  ############################################

card1 <- card(
  div(style = "text-align: center;", h3(strong("Today's Experiment"))),
  h4(em("Objective")),
  "Determine fish habitat preference by comparing behavior in various environments",
  br(), br(),
  div(style = "text-align: center;", h3(strong("About the Habitat"))),
  h5(strong("Species:"), em(hab_sp)),
  h5(strong("Common Name: "), hab_cn),
  img(src="RedMangrove.jpg")
  
)

card2 <- card(
  card_body(
    max_height=225,
    div(style = "text-align: center;", h3(strong("Which habitat does this fish prefer?"))),
    plotOutput("prefer")
  ),
  card_body(
    div(style = "text-align: center;", h3(strong("Time Spent Occupying Each Habitat"))),
    plotOutput("time")
  )
)

card3 <- card(
  card_body(
    div(style = "text-align: center;", h3(strong("About the Fish"))),
    img(src="CommonSnook.jpg"),
    h5(strong("Species:"), em(fish_sp)),
    h5(strong("Common Name: "), fish_cn),
    h5(strong("Age: "), fish_age),
    h5(strong("Fork Length: "), fish_fl),
    h5(strong("Weight: "), fish_w)#,
    #h4(em("Fun Fact")), fun_fact
  )
)

thematic::thematic_shiny(font="auto")

ui <- page_fillable(
  #theme = bs_theme(version = 5), #bootswatch = "flatly"),
  #input_dark_mode(),
  
  div(style = "text-align: center;", h1(strong("Fisheries Ecology & Enhancement"))),
  div(style = "text-align: center;", h2("Habitat Choice Experiment")),
  
  layout_column_wrap(
    width=NULL, style=bslib::css(grid_template_columns="1fr 2fr 1fr"),
    card1, card2, card3
  )
  
) # end ui

##############################  SERVER  ########################################

server <- function(input, output) {
  #bs_themer()
  
  test_df <- data.table(x=3, y=1.25, svg=snook_svg)
  
  output$prefer <- renderPlot({
    ggplot(test_df) +
      geom_point(aes(x=1,y=1), color="transparent") +
      geom_point(aes(x=10,y=1), color="transparent") +
      
      geom_point_svg(aes(x,y, svg=svg), size=60) +
      geom_segment(aes(x=x, xend=x, y=-0.2, yend=0.2), linewidth=1.5) +
      
      geom_segment(aes(x=0.5, xend=10.5, y=0, yend=0), linewidth=1.5) +
      geom_segment(aes(x = 10.2, xend = 10.5, y = 0, yend = 0),
        arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
        linewidth = 1.5) +
      geom_segment(aes(x = 0.8, xend = 0.5, y = 0, yend = 0),
        arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
        linewidth = 1.5) +
      
      scale_x_continuous(expand=c(0,0), breaks=seq(0,11,1), limits=c(0,11))+
      scale_y_continuous(expand=c(0,0),limits=c(-1,2)) +
      
      geom_text(aes(x=1, y=-0.6, label="Real Mangrove"), size=5) +
      geom_text(aes(x=10, y=-0.6, label="Replica Mangrove"), size=5) +
      coord_cartesian(clip = "off") + 
      
      theme(plot.margin = margin(10,10,10,10),
            axis.line = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())
  })
  
  time_comp <- data.table(
    x = c("Day", "Night"),
    y = seq(1,10,.5),
    Habitat = c("Replica Mangrove", "Real Mangrove", "Real Mangrove", "Real Mangrove","Replica Mangrove")
  )
  
  output$time <- renderPlot({
    ggplot(time_comp) +
      geom_boxplot(aes(x=x, y=y, color=Habitat), fill=NA, linewidth=1.5) +
      
      xlab("Time of Day") +
      ylab("min/hr") +
      
      theme(
             axis.line=element_line(linewidth=1.5),
             axis.ticks=element_line(linewidth=1.5),
             axis.text=element_text(size=14),
             axis.title = element_text(size=18),
             legend.title = element_text(size=18),
             legend.text = element_text(size=14),
             legend.position = "bottom",
             panel.background= element_blank()
      )
  })
  
} # end server

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)