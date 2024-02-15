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



#load data 
HCWDashboard <- readRDS("C:/Users/user/Desktop/Data Associate/HCWDashboard.rds") %>% 
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

#replace missing age with mean of age column
HCWDashboard$age <- round(HCWDashboard$age)
# Replace missing values with the rounded mean age
HCWDashboard$age[is.na(HCWDashboard$age)] <- round(mean(HCWDashboard$age, na.rm = TRUE))

#confirm missing values in age column
colSums(is.na(HCWDashboard))

#drop missing values for now in sex and risk level column
clean_HCWDashboard <- drop_na(HCWDashboard)



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
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        legend.position = "right") +  # Position legend on the right side
  scale_fill_identity(name = "Legend", labels = "Daily Vaccination") +
  scale_colour_identity(name = "Legend", labels = "Cumulative Vaccination")

# Convert ggplot to plotly for interactivity
ggplotly(p, tooltip = c("x", "y"), showlegend = TRUE)


#-------------------------------------------------3------------------------------------

#gender 
# Filter the dataset to include only female HCWs
female_HCW <- clean_HCWDashboard %>%
  filter(sex == "Female") %>% 
  mutate(total_female=nrow(.))


# Calculate the number of vaccinated female HCWs
number_female_hcw <- nrow(female_HCW) %>% 
  print()

# Calculate the proportion of vaccinated female HCWs
total_vacc_hcw <- nrow(clean_HCWDashboard)
proportion_female_hcw <- number_female_hcw / total_vacc_hcw

#pick specific columns for the dashboard
female_HCW <- female_HCW %>% 
  select(subcounty, sex, age, total_female)

# Print the results
cat("Number of vaccinated female HCWs:", number_female_hcw, "\n")
cat("Proportion of vaccinated female HCWs:", proportion_female_hcw)


#-------------------------------------------------4---------------------------------------




# Summarize the risk level distribution among vaccinated HCWs
risk_level_summary <- clean_HCWDashboard %>%
  group_by(risk_level) %>%
  summarize(Count = n())

# Visualize the risk level distribution
ggplot(risk_level_summary, aes(x = risk_level, y = Count, fill = risk_level)) +
  geom_bar(stat = "identity") +
  labs(title = "Risk Level Distribution Among Vaccinated HCWs",
       x = "Risk Level",
       y = "Count") +
  theme_minimal()


#risk level  againist cadre

# Create a horizontal bar plot
ggplot(clean_HCWDashboard, aes(x = cadre, fill = risk_level)) +
  geom_bar(position = "stack", stat = "count") +
  coord_flip() +  # Flip the coordinates to make it horizontal
  labs(title = "Distribution of Professions Against Risk Levels",
       x = "Cadre",
       y = "Risk Level Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#---------------------------------------------5----------------------------------------------




# Summarize the number of vaccinated HCWs by professional cadre
cadre_summary <- clean_HCWDashboard %>%
  group_by(cadre) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>% # Sort by count in descending order
  mutate(Percentage = (Count / sum(Count)) * 100)


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
                   fill = "midnightblue", alpha = 0.7, boundary = 0.4) +
    
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
    ggtitle("Age Distribution") +
    xlab("") +
    ylab("Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Plot the box plot
  bx_plt <- ggplot(data = var_data) +
    geom_boxplot(mapping = aes(x = pull(var_data), y = 1),
                 fill = "#E69F00", color = "gray23", alpha = 0.7) +
    
    # Add titles and labels
    ggtitle("Age Distribution on a Box Plot") +
    xlab("Age Range") +
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # To return multiple outputs, use a `list`
  return(
    
    list(stats,
         hist_gram / bx_plt)) # End of returned outputs
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

#-----------------------------------------9--------------------------------------------------

# Calculate cumulative counts and percentages
daily_vacc <- daily_vacc %>%
  mutate(CumulativeCount = cumsum(DailyVaccinations),
         CumulativePercentage = (CumulativeCount / 7500) * 100)


# Plot the progress bar chart with target line
ggplot(daily_vacc, aes(x = date, y = CumulativeCount)) +
  geom_col(fill = "skyblue", color = "black") +
  geom_hline(yintercept = 7500, color = "red", linetype = "solid") + # Add a horizontal line for the target
  geom_text(aes(label = paste0(round(CumulativePercentage), "%")), vjust = -0.5, size = 3) + # Add percentage labels inside the bars
  scale_y_continuous(labels = scales::comma) + # Format y-axis labels with commas
  labs(title = "Progress Tracking Towards Vaccination Target",
       x = "Date",
       y = "Number of Vaccinated HCWs",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "top")


#-------------------------------------------------10-----------------------------------------------

# Plot the progress tracking chart
ggplot(daily_vacc, aes(x = date)) +
  geom_line(aes(y = CumulativeCount, color = "Number of Vaccinated HCWs")) +
  geom_line(aes(y = CumulativePercentage, color = "Percentage of Target"), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~ (. / 100) * 7500, name = "Percentage of Target")) +
  labs(title = "Progress Tracking Towards Vaccination Target",
       x = "Date",
       y = "Number of Vaccinated HCWs",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "top")

