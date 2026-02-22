
library(tidyverse)
stormwaterdata<-read.csv("DATA/stormwater.csv")
stormwaterdata

#look at the data, assess if it untidy/tidy
glimpse(stormwaterdata)
head(stormwaterdata)
summary(stormwaterdata)
head(stormwaterdata)
tail(stormwaterdata)

#data is untidy. 

#stringsAsFactors=FALSE allows us to manipulate the text-> numbers later

raw_data <- read.csv("DATA/stormwater.csv", stringsAsFactors = FALSE)
raw_data

# Separate the single column into multiple columns
library(tidyverse)

stormwaterdata <- raw_data %>%
  separate(col = 1,  # Separate the first column
           into = c("id", "species", "plant.form", "outflow", "inflow", "no3"),
           sep = ",",
           convert = TRUE) %>%
  mutate(
    species = str_remove_all(species, "\""),  # Remove quotes from species
    plant.form = str_remove_all(plant.form, "\"")  # Remove quotes plant.form
  )
stormwaterdata

# Check it worked
glimpse(stormwaterdata)
head(stormwaterdata)

# 1. Summary statistics by species
stormwaterdata %>%
  group_by(species) %>%
  summarise(
    mean_no3 = mean(no3),
    sd_no3 = sd(no3),
    mean_outflow = mean(outflow),
    n = n()
  ) %>%
  arrange(desc(mean_no3))

# 2. Compare Dryland vs Wetland plants
stormwaterdata %>%
  filter(plant.form != "") %>%  # Remove Control rows
  group_by(plant.form) %>%
  summarise(
    mean_no3 = mean(no3),
    sd_no3 = sd(no3),
    mean_outflow = mean(outflow),
    n = n()
  )

# 3. Control vs Treatment comparison 

# control data
control_data <- stormwaterdata %>%
  filter(species == "Control") %>%
  summarise(
    mean_no3 = mean(no3),
    sd_no3 = sd(no3),
    mean_outflow = mean(outflow)
  )

#plant data
planted_data <- stormwaterdata %>%
  filter(species != "Control") %>%
  summarise(
    mean_no3 = mean(no3),
    sd_no3 = sd(no3),
    mean_outflow = mean(outflow)
  )

# View them
control_data
planted_data

# 4. Visualize NO3 removal by species and plant form
stormwaterdata %>%
  filter(plant.form != "") %>%
  ggplot(aes(x = species, y = no3, fill = plant.form)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Dryland" = "blue", "Wetland" = "green")) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Nitrate Removal Ability by Indigenous Plant Species",
       x = "Plant Species",
       y = "Nitrate Removed (%)",
       fill = "Plant Form")

#Nitrate Removal: Dryland vs Wetland
stormwaterdata %>%
  filter(plant.form != "") %>%
  group_by(plant.form) %>%
  summarise(mean_no3 = mean(no3)) %>%
  ggplot(aes(x = plant.form, y = mean_no3, fill = plant.form)) +
  geom_col() +
  scale_fill_manual(values = c("Dryland" = "steelblue", "Wetland" = "seagreen")) +
  theme_minimal() +
  labs(title = "Average Nitrate Removal: Dryland vs Wetland",
       x = "Plant Form",
       y = "Nitrate Removed (%)",
       fill = "Plant Form")

