# TO DO:
#   - should change naming conventions for the files so that they are ordered automatically by date (19Sep25 --> 2025-09-19)
# NOTES:
#   - 

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

############################## FUNCTIONS #######################################
# function to do the opposite of %in%
`%nin%` = Negate(`%in%`)

# function to read the last line of a file (requires git bash)
last_line_unix <- function(filepath) {
  #system(paste("tail -n 1", filepath), intern = TRUE)
  system2("tail", c("-n 1", filepath), stdout=TRUE)
}

# silences dplyr::summarise messages
#options(dplyr.summarise.inform = FALSE)

############################## OPTIONS #########################################

# habitats in experiment; change as necessary
habitat_a = "Replica Mangrove"
habitat_b = "Red Mangrove"

# UI information to display; change as necessary (imgs kept in www folder)
{
  # Habitat A
  hab_a_name = "Plastic"
  hab_a_img <- img(src="Replica_Mangrove.jpg", width="225px")
  # Habitat B
  hab_b_name = "Rhizophora mangle"
  hab_b_img <- img(src="RedMangroveWater.jpg", width="225px")
  # Fish
  tag_number = "900_209000193086"
  fish_sp = "Centropomus undecimalis"
  fish_cn = "Common Snook"
  fish_age = "11 months"
  fish_fl = "163 mm"
  fish_w = "385 g"
  fish_img <- img(src="CommonSnook.jpg", width="350px")
  detection_image <- "www/snook_yellow.png"
  #snook_svg <- paste(readLines("www/snook-old.svg"), collapse = "\n") # fill:#ebcc00 (snook yellow)
}

# data visualizations to display; change as necessary (options: box_time, hab_trans, line_time)
# Note: you may have to adjust titles or title placement
{
  title_1 = "Number of Times Fish Switched Habitats"
  viz_1 = plotOutput("hab_trans")
  title_2 = "Time Spent Occupying Each Habitat"
  viz_2 = plotOutput("line_time")
}

# mote branded color choices
{
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
}

# colors to assign to each habitat; change as necessary (habitat A, habitat B)
# FYI: this is not automating correctly in ggplot -> change downstream
hab_colors <- c(manatee_gray, turquoise_bay)

##############################  UI  ############################################

# column 1: about the habitats
card1 <- card(
  div(style = "text-align: center;", h3(strong("Current Experiment"))),
  p(strong("Premise:"), "Mote's stock enhancement research uses mangrove replicates to condition
    the snook they will stock to use protective habitat in the wild. Here we will test habitat designs
    and configurations to encourage behaviors that maximize habitat use."),
  p(strong("Question:"), "Will our snook use artificial habitats as much as real mangroves?"),
  #br(), #br(),
  div(style = "text-align: center;", h3(strong("About the Habitats"))),
  p(strong("Common Name: "), habitat_b),
  p(strong("Species:"), em(hab_b_name)),
  div(style = "text-align: center", hab_b_img),
  #br(),
  p(strong("Type:"), habitat_a),
  p(strong("Material: "), hab_a_name),
  div(style = "text-align: center", hab_a_img)
)

# column 2: all plots
card2 <- card(
  card_body(
    max_height=225,
    div(style = "text-align: center;", h4(strong("Where was the fish last detected?"))),
    plotOutput("detect")
  ),
  card_body(
    div(style = "text-align: center;", h4(strong(title_1))),
    viz_1,
    div(style = "text-align: center;", h4(strong(title_2))),
    viz_2
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
    p(strong("Weight: "), fish_w),
    p(strong("Tag ID: "), tag_number)
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
  
  # ------------------- DATA -------------------
  
  # Reactive data pipeline that refreshes every 10 seconds
  dataset <- reactive({
    invalidateLater(10000, session)
    
    # Get the file names of all the raw txt files saved in the folder of data to be imported (change path as necessary)
    ORMR.files = list.files(path=paste0("data"), pattern="*.txt", full.names=T)
    
    ##### FOR MULTIREADER DATA #####  
    # Use a loop to create a raw dataframe for each file in the folder to be imported and create columns in the dataframe to specify location, date, and Antenna
    filenames = as.vector(NA) # create a dummy vector used in the loop
    
    for (i in 1:length(ORMR.files)) {
      file_name = str_sub(str_extract(ORMR.files[i], "data/[[:graph:]]+"),start=6, end=-5)
      filenames[[i]] = file_name
      
      # # make temporary files so as not to lose original data
      #file_clean <- tempfile()
      file_small <- tempfile()
      
      # remove bad characters and filter out "I" detections (does this before reading the file) REQUIRES GIT BASH 
      #system2("tr", c("-d", "'\\000'"), stdin = ORMR.files[i], stdout = file_clean) # calls Unix command-line tool to translate/delete null bytes
      system2("grep", c("--text", "-v", "I", ORMR.files[i]), stdout = file_small)

      file_df <- read.table(file_small, header = FALSE, fill = TRUE, col.names = paste0("V", seq_len(16)))
      #file_df = read.table(ORMR.files[i], header=F, fill=T, col.names = paste0("V", seq_len(16))) # this one will throw warnings for NULL bytes
      
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
    
    ORMR.raw$Date <- as.POSIXct(ORMR.raw$Date, format="%Y-%m-%d")
    #ORMR.raw$Time <- hms(ORMR.raw$Time)
    ORMR.raw$Duration <- as.numeric(hms(ORMR.raw$Duration))
    ORMR.raw$Date_Time <- ymd_hms(paste(ORMR.raw$Date, ORMR.raw$Time))
    
    # subset for date
    #ORMR.raw <- subset(ORMR.raw, Date >= "2023-10-01" & Date <= "2023-10-07")
    current_date <- Sys.Date()
    start_date <- current_date - 11  #NEEDS TO BE 7, but setting to 10 for now
    ORMR.raw <- subset(ORMR.raw, Date >= start_date & Date <= current_date)
    
    # Preprocess
    data <- as.data.table(ORMR.raw) %>%
      filter(Tag_ID %in% tag_number) %>%
      mutate(
        Bin_Loop = case_when(
          Loop %in% c("Habitat - A1") ~ "A",
          TRUE ~ "B"),
        #Duration_Sec = period_to_seconds(hms(Duration)),
        Duration_Sec = Duration,
        Duration_Min = Duration_Sec / 60,
        Hour = hour(Date_Time),
        Day_Night = case_when(Hour >= 7 & Hour < 19 ~ "Day", TRUE ~ "Night"),
        Habitat = case_when(Bin_Loop == "A" ~ habitat_a, TRUE ~ habitat_b),
        Date = 
          case_when(
            Date %in% c(as.POSIXct("2025-09-23", format="%Y-%m-%d"), as.POSIXct("2025-09-24", format="%Y-%m-%d"), as.POSIXct("2025-09-25", format="%Y-%m-%d")) ~ Date + (5 * 24 * 60 * 60),
            TRUE ~ Date)
      )
    
    #Trim data back to 7 days now that Date data is fixes
    new_start_date <- current_date - 7  #NEEDS TO BE 7, but setting to 10 for now
    data <- subset(data, Date >= new_start_date & Date <= current_date)
    
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
    
    # sum total habitat transitions per hour
    hab_trans <- data %>%
      mutate(Hab_Trans = if_else(Habitat == lag(Habitat), 0, 1),
             Hab_Trans = replace_na(Hab_Trans, 0)) %>%
      group_by(Date, Hour) %>%
      summarise(Tot_Hab_Trans = sum(Hab_Trans, na.rm=TRUE), .groups = 'drop') %>%
      mutate(Date_Time_Hour = ymd_h(paste(Date, Hour)))
    
    # Last detection location
    # last_detection <- tail(data$Habitat, 1)
    last <- last_line_unix(ORMR.files[[length(ORMR.files)]]) # needs git bash to work
    fields <- str_split(last, "\\s+", simplify = TRUE)
    last_df <- as.data.table(as.list(fields))
    last_detection <- last_df$V7
    detect_x <- if (last_detection == "A1") 2 else 8.5
    detect_df <- data.table(x = detect_x, y = 1.25)
    
    list(raw = data, hourly = min_per_hour, transitions = hab_trans, detect = detect_df, last_hab = last_detection)
  })
  
  # ------------------- PLOTS -------------------
  
  # Fish location "slider": where was the fish last detected
  output$detect <- renderPlot({
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
    cols <- c("Replica Mangrove" = "#b1b3b3", "Red Mangrove" = "#00ae9d")
    ggplot(df) +
      geom_boxplot(aes(x=Day_Night, y=Total_Min_Detected, color=Habitat), fill=NA, linewidth=1.5) +
      scale_color_manual(values = cols) +
      
      xlab("Time of Day") +
      ylab("min/hr") +
      
      theme(
        axis.line=element_line(linewidth=1.5),
        axis.ticks=element_line(linewidth=1.5),
        axis.text=element_text(size=14),
        axis.title = element_text(size=16),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14),
        legend.position = "bottom",
        panel.background= element_blank()
      )
  })
  
  # Line chart: number of habitat transitions in each hour
  output$hab_trans <- renderPlot({
    df <- dataset()$transitions
    ggplot(df) +
      #geom_point(aes(x=Date_Time_Hour, y=Tot_Hab_Trans), color=snook_yellow) +
      geom_line(aes(x=Date_Time_Hour, y=Tot_Hab_Trans), color=snook_yellow, linewidth=1.5) +
      
      scale_x_datetime(date_breaks = "8 hour", date_labels = "%b %d %H") +
      scale_y_continuous(
        limits = c(0,(max(df$Tot_Hab_Trans))), 
        labels = scales::label_number(accuracy = 1),
        breaks = seq(0,(max(df$Tot_Hab_Trans)),10)) +
      ylab("# of Transitions") +
      
      theme(
        axis.line=element_line(linewidth=1.5),
        axis.ticks=element_line(linewidth=1.5),
        axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 30, vjust=.9, hjust=0.9),
        axis.title = element_text(size=16),
        axis.title.x=element_blank(),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14),
        legend.position = "bottom",
        panel.background= element_blank()
      )
  })
  
  # Line chart: time spent occupying each habitat
  output$line_time <- renderPlot({
    df <- dataset()$hourly
    #cols <- c(habitat_a = hab_colors[[1]], habitat_b = hab_colors[[2]])
    #cols <- c(habitat_a = manatee_gray, habitat_b = mangrove_green)
    cols <- c("Replica Mangrove" = "#b1b3b3", "Red Mangrove" = "#00ae9d")
    ggplot(df) +
      #geom_point(aes(x=Date_Time_Hour, y=Total_Min_Detected, color=Habitat)) +
      geom_line(aes(x=Date_Time_Hour, y=Total_Min_Detected, color=Habitat), linewidth=1.5) +
      scale_color_manual(values = cols) +
      
      scale_x_datetime(date_breaks = "8 hour", date_labels = "%b %d %H") +
      scale_y_continuous(
        limits = c(0,(max(df$Total_Min_Detected))), 
        labels = scales::label_number(accuracy = 1),
        breaks = seq(0,(max(df$Total_Min_Detected)),10)) +
      ylab("min/hr") +
      
      theme(
        axis.line=element_line(linewidth=1.5),
        axis.ticks=element_line(linewidth=1.5),
        axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 30, vjust=.9, hjust=0.9),
        axis.title = element_text(size=16),
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
    df <- df[df$Date >= Sys.Date(),]
    
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
        h4(paste0("Today the tagged fish prefers the ", tolower(conc_text), " habitat."))
      )
    )
  })
  
}

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)