# TO DO:
#   - make shiny check files for new info every X seconds
#   - fix date/time subsetting to reflect the real dataset
#   - update UI info and images to reflect current experiment

##############################  GLOBAL  ########################################
library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)
#library(ggsvg)
library(ggimage)
library(glue)
library(shiny)
library(bslib)
library(showtext)
library(thematic)

`%nin%` = Negate(`%in%`)

# habitats in experiment; change as necessary
habitat_a = "Red Mangrove"
habitat_b = "Replica Mangrove"

# UI information to display; change as necessary (imgs kept in www folder)
{
  # Habitat A
  hab_a_name = "Rhizophora mangle"
  hab_a_img <- img(src="RedMangroveWater.jpg", width="300px")
  # Habitat B
  hab_b_name = "Plastic"
  hab_b_img <- img(src="Replica_Mangrove.jpg", width="280px")
  # Fish
  fish_sp = "Centropomus undecimalis"
  fish_cn = "Common Snook"
  fish_age = "11 months"
  fish_fl = "10 inches"
  fish_w = "0.7 pounds"
  fish_img <- img(src="CommonSnook.jpg", width="400px")
  detection_image <- "www/snook_yellow.png"
  #snook_svg <- paste(readLines("www/snook-old.svg"), collapse = "\n") # fill:#ebcc00 (snook yellow)
}

# mote branded color choices
abyssal_blue = "#003041"
gulf_teal = "#00798c"
turquoise_bay = "#00ae9d"
estuary_green = "#638a63"
mangrove_green = "#007b41"
seagrass_green = "#85b034"
snook_yellow = "#ebcc00"
sandbar_beige = "#c6b8a6"
otter_brown = "#6d5849"
shark_gray = "#63666a"
manatee_gray = "#b1b3b3"

# colors to assign to each habitat; change as necessary
hab_colors <- c(mangrove_green, manatee_gray)

##############################  UI  ############################################

# column 1: about the habitats
card1 <- card(
  div(style = "text-align: center;", h3(strong("Current Experiment"))),
  p(strong("Objective:"), "Determine fish habitat preference by comparing behavior in various environments"),
  br(), #br(),
  div(style = "text-align: center;", h3(strong("About the Habitats"))),
  p(strong("Common Name: "), habitat_a),
  p(strong("Species:"), em(hab_a_name)),
  div(style = "text-align: center", hab_a_img),
  br(),
  p(strong("Type:"), habitat_b),
  p(strong("Material: "), hab_b_name),
  div(style = "text-align: center", hab_b_img)
)

# column 2: all plots
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

# column 3: about the fish
card3 <- card(
  #card_body(
    #max_height=540,
    div(style = "text-align: center;", h3(strong("About the Fish"))),
    div(style = "text-align: center", fish_img),
    p(strong("Common Name: "), fish_cn),
    p(strong("Species:"), em(fish_sp)),
    p(strong("Age: "), fish_age),
    p(strong("Fork Length: "), fish_fl),
    p(strong("Weight: "), fish_w)
  #)
)

# automatically adjust ggplot themes
thematic::thematic_shiny(font="auto")

ui <- page_fillable(
  theme = bs_theme(version=5, bootswatch = "superhero", bg=abyssal_blue, fg="#ffffff"), #theme,
  #input_dark_mode(),
  
  div(style = "text-align: center;", h1(strong("Fisheries Ecology & Enhancement:"), "Habitat Choice Experiment")),
  #div(style = "text-align: center;", h2("Habitat Choice Experiment")),
  
  layout_column_wrap(
    width=NULL, style=bslib::css(grid_template_columns="1fr 2fr 1fr"),
    card1, card2, layout_column_wrap(
      width = 1/1, heights_equal = "row",
      card3,
      as_fill_carrier(uiOutput("conclusion_card"))
    )
  )
  
) # end ui

##############################  SERVER  ########################################

server <- function(input, output, session) {
  
  # Reactive data pipeline that refreshes every 10 seconds
  dataset <- reactive({
    invalidateLater(10000, session)
    
    # Get the file names of all the raw txt files saved in the folder of data to be imported (change path as necessary)
    ORMR.files = list.files(path=paste0("data"), pattern="*.txt", full.names=T)
    
    ##### FOR MULTIREADER DATA #####  
    # Use a loop to create a raw dataframe for each file in the folder to be imported and create columns in the dataframe to specify creek, date, and Antenna
    filenames = as.vector(NA) # create a dummy vector used in the loop
    
    for (i in 1:length(ORMR.files)) {
      file_name = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=6, end=-5)
      filenames[[i]] = file_name
      file_df = read.table(ORMR.files[i], header=F, fill=T, col.names = paste0("V", seq_len(16)))
      file_df$System = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=6, end=7)
      file_df$ReadDate = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=9, end=15)
      file_df$Antenna = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=17, end=-5)
      assign(file_name, file_df, envir = .GlobalEnv)
    }
    
    # Identify the PIT tag data in each dataframe, merge them, and relabeled columns appropriately
    dflist =as.list(NA) # creates a dummy list that the loop below can fill with all the dataframes
    PITlist = as.list(NA) # creates a dummy list to fill with the PIT tag data within the list of dataframes
    Errorlist = as.list(NA)
    
    for (i in 1:length(filenames)) {
      dflist[[i]] = get(filenames[i])
      PITlist[[i]] = dflist[[i]][which(dflist[[i]]$V1=='S'),]
      Errorlist[[i]] = dflist[[i]][which(dflist[[i]]$V1=='E'),]
    }
    
    ORMR.raw = droplevels(rbindlist(PITlist, fill=T)) # Combine the list of dataframes into one dataframe
    
    ORMR.raw = within(ORMR.raw, rm(V11, V12, V13, V14, V15, V16)) # delete empty columns
    ORMR.raw = plyr::rename(ORMR.raw, c("V1"="Code", "V2"="Date", "V3"="Time", "V4"="Time_Reference", "V5"="Duration", "V6"="Tag_Type", "V7"="Loop", "V8"="Tag_ID",
                                        "V9"="Site_Code", "V10"="Effective_Amps")) # rename the columns
    # NOTE that Number consecutive detections seems to be missing (between 9 and 10)
    ORMR.raw$Loop = paste(ORMR.raw$Antenna, "-", ORMR.raw$Loop)
    
    ORMR.raw$Date_Time <- ymd_hms(paste(ORMR.raw$Date, ORMR.raw$Time))
    ORMR.raw$Date <- as.POSIXct(ORMR.raw$Date, format="%Y-%m-%d")
    
    # subset for date
    ORMR.raw <- subset(ORMR.raw, Date >= "2023-10-01" & Date <= "2023-10-07")
    # current_date <- Sys.Date()
    # start_date <- current_date - 7
    # ORMR.raw <- subset(ORMR.raw, Date >= start_date & Date <= current_date)
    
    # Preprocess
    data <- as.data.table(ORMR.raw) %>%
      mutate(
        Bin_Loop = case_when(
          Loop %in% c("Nursery3 - 0822_1530095941184861", "Nursery3 - A1", "Nursery3 - A2") ~ "A",
          TRUE ~ "B"),
        Duration_Sec = period_to_seconds(hms(Duration)),
        Duration_Min = Duration_Sec / 60,
        Hour = hour(Date_Time),
        Day_Night = case_when(Hour >= 7 & Hour < 19 ~ "Day", TRUE ~ "Night"),
        Habitat = case_when(Bin_Loop == "A" ~ habitat_a, TRUE ~ habitat_b)
      )
    
    # Summarize per hour
    min_per_hour <- data %>%
      group_by(Bin_Loop, Date, Hour) %>%
      summarise(Total_Min_Detected = sum(Duration_Min, na.rm = TRUE), .groups = 'drop') %>%
      mutate(
        Date_Time_Hour = ymd_h(paste(Date, Hour)),
        Day_Night = case_when(Hour >= 7 & Hour < 19 ~ "Day", TRUE ~ "Night"),
        Habitat = case_when(Bin_Loop == "A" ~ habitat_a, TRUE ~ habitat_b)
      ) %>%
      filter(Total_Min_Detected != 0)
    
    # Last detection location
    last_detection <- tail(data$Habitat, 1)
    detect_x <- if (last_detection == habitat_a) 2 else 8.5
    detect_df <- data.table(x = detect_x, y = 1.25)
    
    list(raw = data, hourly = min_per_hour, detect = detect_df, last_hab = last_detection)
  })
  
  # ------------------- PLOTS -------------------
  
  # Fish location "slider": where was the fish last detected
  output$prefer <- renderPlot({
    detect_df <- dataset()$detect
    last_detection <- dataset()$last_hab
    
    ggplot(detect_df) +
      geom_point(aes(x=1,y=1), color="transparent") +
      geom_point(aes(x=10,y=1), color="transparent") +
      
      geom_image(aes(x,y, image=detection_image), size=2) +
      
      geom_segment(aes(x=x, xend=x, y=-0.2, yend=0.2), linewidth=1.5) +
      geom_segment(aes(x=0.5, xend=10.5, y=0, yend=0), linewidth=1.5) +
      
      scale_x_continuous(expand=c(0,0), breaks=seq(0,11,1), limits=c(0,11))+
      scale_y_continuous(expand=c(0,0),limits=c(-1,2)) +
      
      geom_text(aes(x=1, y=-0.6, label=habitat_a), size=5) +
      geom_text(aes(x=10, y=-0.6, label=habitat_b), size=5) +
      coord_cartesian(clip = "off") + 
      
      theme(
        plot.margin = margin(10,10,10,10),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()
      )
  })
  
  # Boxplot: time spent occupying each habitat
  output$box_time <- renderPlot({
    df <- dataset()$hourly
    ggplot(df) +
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
  
  # Line chart: time spent occupying each habitat
  output$line_time <- renderPlot({
    df <- dataset()$hourly
    ggplot(df) +
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
  
  # conclusions text & color card (reactive)
  output$conclusion_card <- renderUI({
    df <- dataset()$hourly
    
    hab_a_mean <- mean(df$Total_Min_Detected[df$Bin_Loop=="A"], na.rm=TRUE)
    hab_b_mean <- mean(df$Total_Min_Detected[df$Bin_Loop=="B"], na.rm=TRUE)
    
    conc_color <- ifelse(hab_a_mean > hab_b_mean, hab_colors[[1]], hab_colors[[2]])
    conc_text <- ifelse(hab_a_mean > hab_b_mean, habitat_a, habitat_b)
    
    card(
    card_body_fill = TRUE,
      style = glue::glue("background-color: {conc_color}; color: white;"),
      card_body(
        class = "text-center",
        h3(strong("Conclusions So Far"))
      ),
      card_body(
        class = "text-center",
        h4(paste0("This fish prefers the ", tolower(conc_text), " habitat."))
      )
    )
  })
  
}

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)