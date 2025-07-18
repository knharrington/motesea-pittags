##############################  GLOBAL  ########################################
library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(ggsvg)
library(glue)

library(shiny)
library(bslib)
library(showtext)
library(thematic)

`%nin%` = Negate(`%in%`)

mangrove_green = "#007b41"
seagrass_green = "#85b034"
snook_yellow = "#ebcc00"
otter_brown = "#6d5849"
shark_gray = "#63666a"
manatee_gray = "#b1b3b3"

hab_colors <- c(mangrove_green, manatee_gray)

habitat_a = "Red Mangrove"
habitat_b = "Replica Mangrove"

# ui facts
{
  hab_cn = habitat_a
  hab_sp = "Rhizophora mangle"
  hab_sp2 = habitat_b
  hab_cn2 = "Plastic"
  fish_sp = "Centropomus undecimalis"
  fish_cn = "Common Snook"
  fish_age = "11 months"
  fish_fl = "10 inches"
  fish_w = "0.7 pounds"
  fun_fact = "Snook are popular game and food fish."
}

# current color is fill:#ebcc00 which is snook yellow
snook_svg <- paste(readLines("www/snook.svg"), collapse = "\n")

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

ORMR.raw$Date_Time <- ymd_hms(paste(ORMR.raw$Date, ORMR.raw$Time))
ORMR.raw$Date <- as.POSIXct(ORMR.raw$Date, format="%Y-%m-%d")

# subset for date
ORMR.raw <- subset(ORMR.raw, Date >= "2023-10-01" & Date <= "2023-10-07")
# current_date <- Sys.Date()
# start_date <- current_date - 7
# ORMR.raw <- subset(ORMR.raw, Date >= start_date & Date <= current_date)

data <- as.data.table(ORMR.raw) %>%
  mutate(
    Bin_Loop = case_when(
      Loop %in% c("Nursery3 - 0822_1530095941184861", "Nursery3 - A1", "Nursery3 - A2") ~ "A",
      TRUE ~ "B"),
    Duration_Sec = period_to_seconds(hms(Duration)),
    Duration_Min = Duration_Sec / 60,
    Hour = hour(Date_Time),
    Day_Night = case_when(
      Hour >= 7 & Hour < 19 ~ "Day",
      TRUE ~ "Night"
    ),
    Habitat = case_when(
      Bin_Loop == "A" ~ habitat_a,
      TRUE ~ habitat_b
    )
  ) 

# calculate proportion of time spent in each habitat
min_per_hour <- data %>%
  group_by(Bin_Loop, Date, Hour) %>%
  summarise(
    Total_Min_Detected = sum(Duration_Min, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Date_Time_Hour = ymd_h(paste(Date, Hour)),
    Day_Night = case_when(
      Hour >= 7 & Hour < 19 ~ "Day",
      TRUE ~ "Night"
    ),
    Habitat = case_when(
      Bin_Loop == "A" ~ habitat_a,
      TRUE ~ habitat_b
    )
  ) %>% filter(Total_Min_Detected != 0)

# which habitat is winning, display that color
hab_a_data <- min_per_hour[min_per_hour$Bin_Loop == "A",]
hab_b_data <- min_per_hour[min_per_hour$Bin_Loop == "B",]
hab_a_mean <- mean(hab_a_data$Total_Min_Detected)
hab_b_mean <- mean(hab_b_data$Total_Min_Detected)
conc_color <- ifelse(hab_a_mean > hab_b_mean, "#007b41", "#63666a")
conc_text <- ifelse(hab_a_mean > hab_b_mean, habitat_a, habitat_b)

# # Create full grid of all combinations of Bin_Loop × Date × Hour (0–23)
# full_grid <- expand.grid(
#   Bin_Loop = unique(data$Bin_Loop),
#   Date = unique(data$Date),
#   Hour = 0:23
# )

# # Join and fill in missing combinations with 0
# min_per_hour <- full_grid %>%
#   left_join(summary_by_hour, by = c("Bin_Loop", "Date", "Hour")) %>%
#   mutate(
#     Total_Min_Detected = replace_na(Total_Min_Detected, 0),
#     Day_Night = case_when(
#       Hour >= 7 & Hour < 19 ~ "Day",
#       TRUE ~ "Night"
#     ),
#     Habitat = case_when(
#       Bin_Loop == "A" ~ habitat_a,
#       TRUE ~ habitat_b
#     )
#   )

##############################  UI  ############################################

card1 <- card(
  div(style = "text-align: center;", h3(strong("Current Experiment"))),
  p(strong("Objective:"), "Determine fish habitat preference by comparing behavior in various environments"),
  br(), #br(),
  div(style = "text-align: center;", h3(strong("About the Habitats"))),
  p(strong("Species:"), em(hab_sp)),
  p(strong("Common Name: "), hab_cn),
  div(style = "text-align: center", img(src="RedMangroveWater.jpg", width="300px")),
  br(),
  p(strong("Type:"), hab_sp2),
  p(strong("Material: "), hab_cn2),
  div(style = "text-align: center", img(src="RedMangrove.jpg", width="300px"))
)

card2 <- card(
  card_body(
    max_height=225,
    div(style = "text-align: center;", h3(strong("Where was the fish last detected?"))),
    plotOutput("prefer")
  ),
  card_body(
    div(style = "text-align: center;", h3(strong("Time Spent Occupying Each Habitat"))),
    plotOutput("box_time"),
    plotOutput("line_time")
  )
)

card3 <- card(
  #card_body(
    #max_height=540,
    div(style = "text-align: center;", h3(strong("About the Fish"))),
    div(style = "text-align: center", img(src="CommonSnook.jpg", width="300px")),
    p(strong("Species:"), em(fish_sp)),
    p(strong("Common Name: "), fish_cn),
    p(strong("Age: "), fish_age),
    p(strong("Fork Length: "), fish_fl),
    p(strong("Weight: "), fish_w)#,
    #h4(em("Fun Fact")), fun_fact
  #)
)

card4 <- card(
  card_body_fill = TRUE,
  style = glue("background-color: {conc_color}; color: white;"),
  card_body(
    class = "text-center",
    h3(strong("Conclusions So Far"))
  ),
  card_body(
    class = "text-center",
    h4(textOutput("hab_pref"))
  )
)

thematic::thematic_shiny(font="auto")

# theme <- bs_theme(
#   version = 5,
#   bg = "#023f88",
#   fg = "rgb(255, 255, 255)",
#   bootswatch = "superhero"
#   #primary = "#0054a6",
#   #secondary = "#00aae7"
# )

ui <- page_fillable(
  theme = bs_theme(version=5, bootswatch = "superhero"), #theme,
  #input_dark_mode(),
  
  div(style = "text-align: center;", h1(strong("Fisheries Ecology & Enhancement:"), "Habitat Choice Experiment")),
  #div(style = "text-align: center;", h2("Habitat Choice Experiment")),
  
  layout_column_wrap(
    width=NULL, style=bslib::css(grid_template_columns="1fr 2fr 1fr"),
    card1, card2, layout_column_wrap(
      width = 1/1, heights_equal = "row",
      card3,
      card4
    )
  )
  
) # end ui

##############################  SERVER  ########################################

server <- function(input, output) {
  #bs_themer()
  
  last_detection <- last(data$Habitat)
  if (last_detection == habitat_a){
    x = 2
  } else if (last_detection == habitat_b) {
    x = 8
  }
  detect_df <- data.table(x=x, y=1.25, svg=snook_svg)
  
  output$prefer <- renderPlot({
    ggplot(detect_df) +
      geom_point(aes(x=1,y=1), color="transparent") +
      geom_point(aes(x=10,y=1), color="transparent") +
      
      geom_point_svg(aes(x,y, svg=svg), size=60) +
      geom_segment(aes(x=x, xend=x, y=-0.2, yend=0.2), linewidth=1.5) +
      
      geom_segment(aes(x=0.5, xend=10.5, y=0, yend=0), linewidth=1.5) +
      # geom_segment(aes(x = 10.2, xend = 10.5, y = 0, yend = 0),
      #   arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
      #   linewidth = 1.5) +
      # geom_segment(aes(x = 0.8, xend = 0.5, y = 0, yend = 0),
      #   arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
      #   linewidth = 1.5) +
      
      scale_x_continuous(expand=c(0,0), breaks=seq(0,11,1), limits=c(0,11))+
      scale_y_continuous(expand=c(0,0),limits=c(-1,2)) +
      
      geom_text(aes(x=1, y=-0.6, label=habitat_a), size=5) +
      geom_text(aes(x=10, y=-0.6, label=habitat_b), size=5) +
      coord_cartesian(clip = "off") + 
      
      theme(plot.margin = margin(10,10,10,10),
            axis.line = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())
  })
  
  output$box_time <- renderPlot({
    ggplot(min_per_hour) +
      geom_boxplot(aes(x=Day_Night, y=Total_Min_Detected, color=Habitat), fill=NA, linewidth=1.5) +
      
      scale_color_manual(values = hab_colors) +
      
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
  
  output$line_time <- renderPlot({
    ggplot(min_per_hour) +
      geom_line(aes(x=Date_Time_Hour, y=Total_Min_Detected, color=Habitat), linewidth=1.5) +
      scale_color_manual(values = hab_colors) +
      
      scale_x_datetime(date_breaks = "12 hour", date_labels = "%b %d %H") +
      ylab("min/hr") +
      
      theme(
        axis.line=element_line(linewidth=1.5),
        axis.ticks=element_line(linewidth=1.5),
        axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 30, vjust=.9, hjust=0.9),
        axis.title = element_text(size=18),
        axis.title.x=element_blank(),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14),
        legend.position = "bottom",
        panel.background= element_blank()
      )
  })
  
  output$hab_pref <- renderText({
    paste0("This fish prefers the ", tolower(conc_text), " habitat.")
  })
  
} # end server

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)