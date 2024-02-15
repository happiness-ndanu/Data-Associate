library(shiny)
library(shinyauthr)
library(sf)
library(tmap)
library(leaflet)
library(summarytools)
library(tidyverse)
library(glue)
library(patchwork)
library(readxl)
library(here)
library(janitor)
library(graphics)
library(paletteer)
library(dplyr)
library(lubridate)
library(plotly)
library(ggrepel)
library(DT)
library(hrbrthemes)
library(kableExtra)
library(gganimate)
library(viridis)
library(viridisLite)
library(hrbrthemes)
library(viridis)
library(plotly)
library(heatmaply)

here()
#load data 
HCWDashboard <- readRDS("HCWDashboard.rds") %>%  
  clean_names()

nrow(HCWDashboard)
#Check data type
sapply(HCWDashboard,class)

HCWDashboard$date <- as.Date(HCWDashboard$date)
HCWDashboard$team <- as.character(HCWDashboard$team)
HCWDashboard$subcounty <- as.character(HCWDashboard$subcounty)
HCWDashboard$sex <- as.character(HCWDashboard$sex)
HCWDashboard$age <- as.numeric(HCWDashboard$age)
HCWDashboard$cadre <- as.character(HCWDashboard$cadre)
HCWDashboard$risk_level <- as.character(HCWDashboard$risk_level)
sapply(HCWDashboard,class)


#Lookout for missing values
colSums(is.na(HCWDashboard))

#drop missing values

clean_HCWDashboard <- HCWDashboard %>% drop_na()



#confirm missing values in data
colSums(is.na(clean_HCWDashboard))


#since there are no duplicate rows in this case, let's do EDA

#----------------------------------2------------------------------------------------------

#daily vaccine counts
daily_vacc <- clean_HCWDashboard %>% 
  group_by(date) %>% 
  summarize(DailyVaccinations=n())

#cumulative vaccination
cummltv_vacc <- daily_vacc %>% 
  mutate(CumulativeVaccination=cumsum(DailyVaccinations)) %>% 
  print()

# Plotting
p <- ggplot(cummltv_vacc, aes(x = date)) +
  geom_bar(aes(y = DailyVaccinations), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_line(aes(y = CumulativeVaccination, group = 1), color = "red") +
  geom_point(aes(y = CumulativeVaccination), color = "red") +
  ylab("Vaccination Count") +
  xlab("Date of Vaccination") +
  ggtitle("Daily and Cumulative Vaccination Count in Kakamega County") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),  
        legend.position = "right") +  
  scale_fill_identity(name = "Legend", labels = "Daily Vaccination") +
  scale_colour_identity(name = "Legend", labels = "Cumulative Vaccination")

# Convert ggplot to plotly for interactivity
ggplotly(p, tooltip = c("x", "y"), showlegend = TRUE)


#-------------------------------------------------3------------------------------------

#gender 
# Filter the dataset to include only female HCWs
female_HCW <- clean_HCWDashboard %>%
  group_by(subcounty,sex) %>%
  filter(sex == "Female") %>%
  summarize(total_females = n()) 




#-------------------------------------------------4---------------------------------------


# Generate the summary data
risk_cadre <- clean_HCWDashboard %>%
  select(risk_level, cadre) %>%
  group_by(cadre, risk_level) %>%
  summarise(count = n()) %>%
  arrange(cadre, desc(count)) %>%
  group_by(cadre) %>%
  slice_max(order_by = count, n = 5) %>%
  ungroup() %>%
  complete(risk_level, cadre, fill = list(count = 0))  # Fill missing combinations with count = 0

# Define color palette using topo.colors()
colors <- topo.colors(3) 

# Create the heatmap
hrc <- ggplot(risk_cadre, aes(x = risk_level, y = cadre, fill = count)) +
  geom_tile() +
  scale_fill_gradientn(colors = colors, values = c(0, 0.5, 1),
                       breaks = c(0, 5, max(risk_cadre$count))) +
  labs(title = "HeatMap Showing Distribution of Cadre - Risk Level Count",
       
       x = "Risk Level",
       y = "Cadre") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),  
        axis.title.y = element_text(size = 12))
 
# Convert ggplot object to plotly object
heatmap_hrc <- ggplotly(hrc)

# Display the interactive plot
heatmap_hrc





#---------------------------------------------5----------------------------------------------




# Summarize the number of vaccinated HCWs by professional cadre
cadre_summary <- clean_HCWDashboard %>%
  group_by(cadre) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>% 
  mutate(Percentage = round((Count / sum(Count)) * 100, 2))


#----------------------------------------------7---------------------------------------------

#LET'S LOOK INTO AGE DISTRIBUTION



# Mean age
mean_age <- mean(clean_HCWDashboard$age, na.rm = TRUE)

# Median age
median_age <- median(clean_HCWDashboard$age, na.rm = TRUE)

# Mode age (using the table() function)
mode_age <- as.numeric(names(sort(-table(clean_HCWDashboard$age))[1]))

# Calculate the age range
age_range <- range(clean_HCWDashboard$age, na.rm = TRUE)

# Create a box plot
ggplot(clean_HCWDashboard, aes(y = age)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age",
       y = "Age") +
  theme_minimal()

#we have outliers in the age column (age of as low as 0 and 3) such outliers are best removed

# Drop rows with age below 18
clean_HCWDashboard <- filter(clean_HCWDashboard, age >= 18)


#distribution function
#generating the show distribution function

show_distribution <- function(var_data, binwidth) {
  
  # Get summary statistics by first extracting values from the column
  min_val <- min(pull(var_data))
  max_val <- max(pull(var_data))
  mean_val <- mean(pull(var_data))
  med_val <- median(pull(var_data))
  mod_val <- statip::mfv(pull(var_data))
  
  # Print the stats
  stats <- glue::glue(
    "Minimum: {format(round(min_val, 2), nsmall = 2)}
   Mean: {format(round(mean_val, 2), nsmall = 2)}
   Median: {format(round(med_val, 2), nsmall = 2)}
   Mode: {format(round(mod_val, 2), nsmall = 2)}
   Maximum: {format(round(max_val, 2), nsmall = 2)}"
  )
  
  theme_set(theme_light())
  # Plot the histogram
  hist_gram <- ggplot(var_data) +
    geom_histogram(aes(x = pull(var_data)), binwidth = binwidth,
                   fill = "chocolate", alpha = 0.9, boundary = 0.4) +
    
    # Add lines for the statistics
    geom_vline(xintercept = min_val, color = "gray33",
               linetype = "dashed", size = 1.3) +
    geom_vline(xintercept = mean_val, color = "cyan",
               linetype = "dashed", size = 1.3) +
    geom_vline(xintercept = med_val, color = "red",
               linetype = "dashed", size = 1.3) +
    geom_vline(xintercept = mod_val, color = "yellow",
               linetype = "dashed", size = 1.3) +
    geom_vline(xintercept = max_val, color = "gray33",
               linetype = "dashed", size = 1.3) +
    
    # Add titles and labels
    ggtitle("Age Distribution of Vaccinated HCWs") +
    xlab("") +
    ylab("Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Plot the box plot
  bx_plt <- ggplot(data = var_data) +
    geom_boxplot(mapping = aes(x = pull(var_data), y = 1),
                 fill = "blue", color = "gray23", alpha = 0.7) +
    
    # Add titles and labels
    ggtitle("Age Distribution of Vaccinated HCWs on a Box Plot") +
    xlab("Age Range") +
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # To return multiple outputs, use a `list`
  return(
    
    list(stats,
         hist_gram / bx_plt)) 
}

#plot for distribution of age
age_vacc<- clean_HCWDashboard %>%
  select(age)
show_distribution(var_data = age_vacc, binwidth = 10)



#-----------------------------------------7---------------------------------------------

#LOOK INTO SUBCOUNTY AGAINST SEX
# Create a table of counts of Sex by Subcounty
count_ss <- table(clean_HCWDashboard$subcounty, clean_HCWDashboard$sex)

# Convert count_ss to a dataframe
count_ss_df <- as.data.frame(count_ss)
colnames(count_ss_df) <- c("Subcounty", "Sex", "Count")

# Create a bar plot
p <- ggplot(count_ss_df, aes(x = Subcounty,  y = Count,fill = Sex, text = paste("Count:", Count))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Gender Count by Sub-counties",
       x = "Sub-counties",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert ggplot object to plotly object
p <- ggplotly(p, tooltip = "text")
p

#-----------------------------------------8----------------------------------------------
#-----------------------------------MAPPING--------------------------------------------------

#read in the kenyan shapefiles

subcountySHP <- read_sf("currentshapefiles/Sub-Counties.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)

#View(subcountySHP %>% st_drop_geometry())

#inspect a few rows
print(subcountySHP[6:9], n = 3)

#inspect column names
colnames(subcountySHP)

#inspect the class shape files
class(subcountySHP)

#Look at the variable data types
glimpse(subcountySHP)

#View the geometry column
subcountySHP_geometry <- st_geometry(subcountySHP)
# View one geometry entry
subcountySHP_geometry[[1]]

#Geometry columns have their own class
class(subcountySHP) #sfc, the list-column with the geometries for each feature
#> [1] "sfc_MULTIPOLYGON" "sfc"

class(subcountySHP[[1]]) #sfg, the feature geometry of an individual simple feature
#> [1] "XY"           "MULTIPOLYGON" "sfg"

### This line is not necessary since the shapefile is already in the WGS 84 projection.

subcountySHP <- st_transform(subcountySHP, crs = 4326)

### Inspect the co-ordinate reference system
st_crs(subcountySHP)
# Coordinate Reference System:
#   User input: EPSG:4326 
# wkt:
#   GEOGCRS["WGS 84",
#           ENSEMBLE["World Geodetic System 1984 ensemble",
#                    MEMBER["World Geodetic System 1984 (Transit)"],
#                    MEMBER["World Geodetic System 1984 (G730)"],
#                    MEMBER["World Geodetic System 1984 (G873)"],
#                    MEMBER["World Geodetic System 1984 (G1150)"],
#                    MEMBER["World Geodetic System 1984 (G1674)"],
#                    MEMBER["World Geodetic System 1984 (G1762)"],
#                    MEMBER["World Geodetic System 1984 (G2139)"],
#                    ELLIPSOID["WGS 84",6378137,298.257223563,
#                              LENGTHUNIT["metre",1]],
#                    ENSEMBLEACCURACY[2.0]],
#           PRIMEM["Greenwich",0,
#                  ANGLEUNIT["degree",0.0174532925199433]],
#           CS[ellipsoidal,2],
#           AXIS["geodetic latitude (Lat)",north,
#                ORDER[1],
#                ANGLEUNIT["degree",0.0174532925199433]],
#           AXIS["geodetic longitude (Lon)",east,
#                ORDER[2],
#                ANGLEUNIT["degree",0.0174532925199433]],
#           USAGE[
#             SCOPE["Horizontal component of 3D system."],
#             AREA["World."],
#             BBOX[-90,-180,90,180]],
#           ID["EPSG",4326]]

# #Load the data that we are going to map
#View(clean_HCWDashboard)

#Clean the data, so that the sub-counties match those in the shapefile.

### Inspect the sub-county names of the disability data
subcounties_df <- unique(clean_HCWDashboard$subcounty)

### Inspect the sub-county names of the shape file
subcounties_KenyaSHP <- subcountySHP %>% 
  st_drop_geometry() %>% 
  select(NAME_2) %>% 
  pull() %>%
  unique()
#glimpse(subcounties_KenyaSHP)

### Convert the clean_HCWDashboard subcounty names to title case
HCWsubcounties_df <- clean_HCWDashboard %>% 
  ungroup() %>% 
  mutate(sub_county = tools::toTitleCase(tolower(subcounty)))

### Inspect the subcounty names  again 
HCWsubcounties2_df <- unique(HCWsubcounties_df$subcounty)

### Inspect the subcounty names that are different in each of the datasets
unique(HCWsubcounties_df$subcounty)[which(!unique(HCWsubcounties_df$subcounty) %in% subcounties_KenyaSHP)]


### Clean the subcounty names so that they match in both datasets

#THERE IS NO SUBCOUNTY KNOWN AS "CHMT" SO I GOOGLED AND FOUND OUT THE OFFICES ARE IN MALAVA SUBCOUNTY. LATER REPLACED "CHMT" WITH LURAMBI
HCWsubcounties_df <- HCWsubcounties_df %>% 
  mutate(subcounty = ifelse(subcounty == "CHMT", "Lurambi", subcounty))


### Inspect the subcounty names again to ensure that they now match.
unique(HCWsubcounties_df$subcounty)[which(!unique(HCWsubcounties_df$subcounty) %in% subcounties_KenyaSHP)]

#make totals column
HCWsubcounties_dfcount <-HCWsubcounties_df%>% 
  count(subcounty, sort = TRUE)


#Join the shapefile and the data
### Rename the SUBCOUNTY variable, to match the variable name in the shapefile data
colnames(HCWsubcounties_dfcount)[1]="NAME_2"
#the above column has been renamed the same as the subcountiesSHP dataframe subcounty column 
colnames(HCWsubcounties_dfcount)[2]="subcounty_totals"


### Ensure that there are no leading or trailing spaces in the subcounty variable
subcountySHP$NAME_2 <- trimws(subcountySHP$NAME_2)
HCWsubcounties_dfcount$NAME_2 <- trimws(HCWsubcounties_dfcount$NAME_2)

### Merge the data
merged_df <- left_join(HCWsubcounties_dfcount, subcountySHP,by = "NAME_2")


### Sort the data so that the sub-county variable appears first
merged_df <- merged_df %>% 
  select(NAME_2, everything())



### Class of the merged data
class(merged_df)
### Column names
colnames(merged_df)



#Visualise the data
#plot()
#We are going to plot a base plot / map.

plot(subcountySHP$geometry, lty = 3, col = "chocolate")

#ggplot2()
map1 <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = subcounty_totals))+
  theme_void()+
  labs(title = "Distribution of HCWs Vaccination In Kakamega Sub-Counties",
       caption = "By:Happiness Ndanu")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  #scale_fill_gradient(low = "darkgreen", high = "red")
  scale_fill_viridis_c(option = "A")
map1


#9.3 tmap()
tmap_mode("plot") #Set tmap mode to static plotting or interactive viewing
merged_df <- st_sf(merged_df)

map2 <- tm_shape(merged_df) +
  tm_fill("subcounty_totals",palette="YlOrRd",
          title="Distribution of HCWs Vaccination Cases In Kakamega Sub-Counties",
          id = "NAME_2") +
  tm_borders(col = "blue",lty = 3)+
  tm_layout(legend.position = c("left", "bottom"))+
  tm_text("NAME_2", size = 0.8) +  # Add text labels for sub-county names
  tmap_mode(mode = "view")
map2

#9.4 leaflet()

## Specify the color scheme
pal <-colorBin(
  palette = "Dark2",
  domain = merged_df$subcounty_totals)

### Specify how labels will be displayed
labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  merged_df$NAME_2, merged_df$subcounty_totals
) %>% lapply(htmltools::HTML)

#Generate the graph
leaflet(merged_df) %>% 
  addTiles() %>% 
  addPolygons(color = "blue", weight = 1, dashArray = "3", fillColor = ~pal(subcounty_totals),
              highlight= highlightOptions(
                weight = 4,
                color = "blue",
                dashArray = "",
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight"="normal", padding="3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(position = c("bottomright"), pal,values = ~subcounty_totals)




#----------------------------------------------9-----------------------------------------------


# Current progress towards the vaccination target
current_progress <- clean_HCWDashboard %>% nrow(.)

# Total number of HCWs
total_hcws <- 7500
#remaining
remaining <- total_hcws- current_progress



# Define progress dataset
progress <- data.frame(
  Category = c("Progress", "Remaining"),
  Count = c(current_progress, remaining) 
)

# Calculate percentage completion
percentage_completion <- (current_progress / total_hcws) * 100


ggplot(progress, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Progress Towards Vaccination Target of 7500 HCWs",
       x = "Categories",
       y = "Number of HCWs") +
  scale_fill_manual(values = c("darkgreen", "red")) +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4, color = "white") + # Placing labels inside the bars
  annotate("text", x = 1, y = current_progress + 200, 
           label = paste0(round(percentage_completion, 2), "% completed"), size = 4) +
  geom_hline(yintercept = 7500, linetype = "dashed", color = "red") + 
  geom_text(aes(label = "Target: 7500 HCWs"), x = 1, y = 7500, vjust = -0.5, hjust = 1, color = "red", angle = 90)



#----------------------------------------------------------10------------------------------------------
# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2", "user3"),
  password = c("pass1", "pass2", "wgs84"),
  permissions = c("admin", "standard","admin"),
  name = c("User One", "User Two", "User Three")
)

#----------------------------------------------EXTRAS---------------------------------------------
  
  #list of subcounties 
  subcounties= clean_HCWDashboard %>% 
  distinct(subcounty) %>% 
  pull(subcounty)

#list of teams
teams= clean_HCWDashboard %>% 
  distinct(team) %>% 
  pull(team)

#-----------------------------U.I INTERFACE--------------------------------------

ui <- fluidPage(

  
     # add login panel UI function
     shinyauthr::loginUI(id = "login"),
     
     br(),
     br(),
     
     div( 
       id="show-page-content",
       tabsetPanel(
         tabPanel(
           title = "KAKAMEGA COUNTY", 
           br(), br(),
           HTML(
           '<center>
             <img src="map.png" width="600" height="450">
             <p style="font-style: italic; font-size: 12px;">Map showing Kakamega County and its borders</p>
             </center>'),
           br(), br(),
           HTML(paste("Kakamega County, located in the former Western Province of Kenya, shares borders with Vihiga County to the south, Siaya County to the west, and Bungoma and Trans Nzoia counties to the north. To the east, it borders Nandi and Uasin Gishu counties. The county's principal town and administrative center is Kakamega. Healthcare facilities in Kakamega include one county referral hospital (Kakamega County General Teaching & Referral Hospital), 12 sub-county hospitals, 47 health centers, 123 dispensaries, and 44 clinics. Administratively, the county is divided into twelve sub-counties, sixty county assembly wards, eighty-three locations, two hundred and fifty sub-locations, one hundred and eighty-seven Village Units, and four hundred Community Administrative Areas. In this analysis, the main focus is on the vaccination of HealthCare Workers in the various Sub-Counties.")),
           br(),
           HTML(paste("The aim of this analysis is to have a detailed Exploratory Data Analysis on the vaccinated HCWs in Kakamega County. Source: [Kakamega County Website](https://kakamega.go.ke/)")),
           br(),
           HTML(paste("NB: The plots in the analysis are interactive hence for more elaboration, please hover your cursor on the plots so as to get precise values")),
           br(),br(),
         ),
         
         tabPanel(
          title = "PLOTS",
         sidebarLayout(
           
         sidebarPanel(
           
         fluidRow(
           column=2,
         
       selectInput(
         inputId = "select_subcounty",
         label = "Select SubCounty",
         choices = subcounties,
         multiple = FALSE,
         selected = "Lurambi"
       ))),
       
    

       mainPanel(
         plotOutput(outputId = "subcounty_team"),
         br(),
         plotlyOutput(outputId = "vaccine_totals"),
         br(),
         br(),
         
         div( 
           h3("TABLE DISPLAYING NUMBER OF FEMALE HCWs"),
           h4(style = "font-style: italic;","Proportion of vaccinated female HCWs is: 0.6195302"),
         dataTableOutput("femalesvacc")),
         
         br(),
         br(),
         
         div( 
           h3("TABLE ILLUSTRATING NUMBER AND PERCENTAGE OF VACCINATED HCWs by CADRE"), 
         dataTableOutput("cadrecategory")),
         
         br(),
         br(),
         plotlyOutput(outputId = "risk_level"),
         br(),
         br(),
         
       
         )) 
        
  
     ),
         
    tabPanel(
         title = "More Viz", 
         
         mainPanel(
            
           div( 
             
           plotlyOutput(outputId = "risk_cadre"),
           h5("The plot shows distribution of the risk level count of different cadres across various subcounties. Hover your cursor on the heatmap to see the risk level count"), 
           ),
           br(),
           br(),
           
           plotlyOutput(outputId = "count_ss"),
           br(),
           br(),
           plotOutput(outputId = "age_distr"),
           br(),
           br(),
           plotOutput(outputId = "progress")
         )
       ),
     
     tabPanel(
       title = "Interactive Map",
       
       HTML(paste("<b>","VISUAL REPRESENTATION OF HWCs VACCINATION ACROSS KAKAMEGA SUB- COUNTIES", "</b>")),
       br(), br(),
       leafletOutput("mymap")
       
     ),
     
     )
       
     )%>% shinyjs::hidden(),
)
     




  


# Define server ----------------------------------------------------------------
server <- function(input, output, session) { 
  
  shiny::observe({
    shiny::req(credentials()$user_auth)
    shinyjs::show(id="show-page-content")
  })
  
  
  # call login module supplying data frame,
  #user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
   id = "logout",
     active = reactive(credentials()$user_auth)
   )

  

  
  # Render the Gender table
  output$femalesvacc <- renderDataTable({
    datatable(female_HCW, 
              options = list(pageLength=10))
  })
  
  
  
  # Render the Cadre table
  output$cadrecategory <- renderDataTable({
    datatable(cadre_summary, 
              options = list(pageLength=10))
  })
  
  
  
  
  output$mymap <- renderLeaflet( {
    
    #Generate the graph
    leaflet(merged_df) %>% 
      addTiles() %>% 
      addPolygons(color = "blue" , weight = 2, dashArray = "3", fillColor = ~pal(subcounty_totals),
                  highlight= highlightOptions(
                    weight = 4,
                    color = "black",
                    dashArray = "10",
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight"="normal", padding="3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(position = c("bottomright"), pal,values = ~subcounty_totals)
    
    
    
  })
  
  output$subcounty_team<- renderPlot({
    
    clean_HCWDashboard %>% 
      group_by(team, subcounty) %>% 
      count() %>% 
      filter(subcounty %in% input$select_subcounty) %>%
      # filter(team %in% input$select_team) %>%
      ggplot(aes(x= subcounty, y = n)) +
      geom_col(aes(fill = team),
               position = "dodge",
               show.legend = TRUE,
               alpha = 0.5)+
      geom_text_repel(aes(label = n, fill = team), 
                      position = position_dodge(width = 0.9),
                      direction = "y",
                      box.padding = 0.5,
                      point.padding = 0.3,
                      max.overlaps = Inf,
                      size = 3, 
                      color = "black") +
      theme_minimal() +
      paletteer::scale_fill_paletteer_d("ggthemes::Classic_Green_Orange_6")+
      theme(
        axis.text.x = element_text(angle = 90),
          plot.title = element_text(size = 18)  
      )+
      ylab("Number of Vaccinations Administered") +
      xlab("Sub County") +
      ggtitle("Number of Vaccinations Given Filtered by SubCounty & Teams")+
      #geom_text(aes(label = n, y =n))+
      theme(panel.grid = element_blank())
    
  })
  
  output$vaccine_totals <- renderPlotly({
    
    # Plotting
    p <- ggplot(cummltv_vacc, aes(x = date)) +
      geom_bar(aes(y = DailyVaccinations), stat = "identity", fill = "midnightblue", alpha = 0.7) +
      geom_line(aes(y = CumulativeVaccination, group = 1), color = "black") +
      geom_point(aes(y = CumulativeVaccination), color = "red") +
      ylab("Vaccination Count") +
      xlab("Date of Vaccination") +
      ggtitle("Daily and Cumulative Vaccination Count in Kakamega County") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),  # Center the title
            legend.position = "right") +  # Position legend on the right side
      scale_fill_identity(name = "Legend", labels = "Daily Vaccination") +
      scale_colour_identity(name = "Legend", labels = "Cumulative Vaccination")
    p
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p, tooltip = c("x", "y"), showlegend = TRUE)
    
  })
  

  
  
  
  
    
  output$risk_cadre <- renderPlotly({
    
   # Define color palette using topo.colors()
colors <- topo.colors(3)  # You can adjust the number of colors as needed

# Create the heatmap
hrc <- ggplot(risk_cadre, aes(x = risk_level, y = cadre, fill = count)) +
  geom_tile() +
  scale_fill_gradientn(colors = colors, values = c(0, 0.5, 1),
                       breaks = c(0, 5, max(risk_cadre$count))) +
  labs(title = "HeatMap Showing Distribution of Cadre - Risk Level Count",
       x = "Risk Level",
       y = "Cadre") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),  
        axis.title.y = element_text(size = 12))
 
# Convert ggplot object to plotly object
heatmap_hrc <- ggplotly(hrc)

# Display the interactive plot
heatmap_hrc
    
  })
  
  

    
  output$count_ss <- renderPlotly({
    
    # Convert count_ss to a dataframe
    count_ss_df <- as.data.frame(count_ss)
    colnames(count_ss_df) <- c("Subcounty", "Sex", "Count")
    
    # Create a bar plot
    p <- ggplot(count_ss_df, aes(x = Subcounty,  y = Count,fill = Sex, text = paste("Count:", Count))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Distribution of Gender Count by Sub-counties",
           x = "Sub-counties",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert ggplot object to plotly object
    p <- ggplotly(p, tooltip = "text")
    p
    
    
    
  })
  
  

  
  
  
  
    
  output$age_distr <- renderPlot({
    
    #plot for distribution of age
    age_vacc<- clean_HCWDashboard %>%
      select(age)
    show_distribution(var_data = age_vacc, binwidth = 10)
    
  })
  
  output$progress <- renderPlot({
   
    
    ggplot(progress, aes(x = Category, y = Count, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Progress Towards Vaccination Target of 7500 HCWs",
           x = "Categories",
           y = "Number of HCWs") +
      scale_fill_manual(values = c("darkgreen", "red")) +
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4, color = "white") + # Placing labels inside the bars
      annotate("text", x = 1, y = current_progress + 200, 
               label = paste0(round(percentage_completion, 2), "% completed"), size = 4) +
      geom_hline(yintercept = 7500, linetype = "dashed", color = "red") + 
      geom_text(aes(label = "Target: 7500 HCWs"), x = 1, y = 7500, vjust = -0.5, hjust = 1, color = "red", angle = 90)
  
})

}
# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
