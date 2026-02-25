R markdown: code for stormwaterdata
================
Amber Charman
2026-02-20

``` r
# Upload tidy verse
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.0     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## Viewing my data

I would like to see how R reads the raw data.

``` r
#load data
stormwaterdata<-read.csv("DATA/stormwater.csv")

#view the data
stormwaterdata

#see the first few lines
head(stormwaterdata)

#see the summary
summary(stormwaterdata)
```

## Cleaning my data

R is reading my data as only one column (when in fact there are 6), and
reading the numbers as characters because the data is untidy. In order
to analyse the data correctly, I need to tidy the data.

``` r
# stringsAsFactors= FALSE allows me to manipulate the "characters", so I can change them to numbers later

#load data
raw_data <- read.csv("DATA/stormwater.csv", stringsAsFactors = FALSE)
```

``` r
#Separate into multiple columns
stormwaterdata <- raw_data %>%
  separate(col = 1,  # Separate the first column
           into = c("id", "species", "plant.form", "outflow", "inflow", "no3"), #use these headings
           sep = ",", 
           convert = TRUE) %>%
  mutate(
    species = str_remove_all(species, "\""),  # Remove quotes from species
    plant.form = str_remove_all(plant.form, "\"")  # Remove quotes plant.form
  )
```

``` r
#view the tidy data
stormwaterdata

# Check R is reading the data correctly: 
#see the first 5 lines 
head(stormwaterdata)
```

``` r
# see the summary of the data
summary(stormwaterdata)
```

    ##        id           species           plant.form           outflow     
    ##  Min.   :  1.00   Length:100         Length:100         Min.   :0.300  
    ##  1st Qu.: 25.75   Class :character   Class :character   1st Qu.:0.800  
    ##  Median : 50.50   Mode  :character   Mode  :character   Median :1.450  
    ##  Mean   : 50.50                                         Mean   :1.861  
    ##  3rd Qu.: 75.25                                         3rd Qu.:2.625  
    ##  Max.   :100.00                                         Max.   :5.000  
    ##      inflow           no3       
    ##  Min.   :3.117   Min.   :13.38  
    ##  1st Qu.:3.117   1st Qu.:42.94  
    ##  Median :4.550   Median :62.39  
    ##  Mean   :4.550   Mean   :59.79  
    ##  3rd Qu.:5.983   3rd Qu.:77.54  
    ##  Max.   :5.983   Max.   :91.64

``` r
#R is providing the 5 number summary, meaning that it is treating the numbers as numbers and no longer as characters
```

## ANALYSIS: Summary Statistics by Species

``` r
# Calculate mean, standard deviation, and sample size for NO3 removal by each species
# Arranged from highest to lowest removal ability to identify best performers

library(tidyverse)
stormwaterdata %>%
  group_by(species) %>%
  summarise(
    mean_no3 = mean(no3),
    sd_no3 = sd(no3),
    mean_outflow = mean(outflow),
    n = n()
  ) %>%
  arrange(desc(mean_no3))
```

    ## # A tibble: 10 × 5
    ##    species      mean_no3 sd_no3 mean_outflow     n
    ##    <chr>           <dbl>  <dbl>        <dbl> <int>
    ##  1 Pennisetum       84.9   5.50        0.625    10
    ##  2 Stenotaphrum     81.1   7.57        0.82     10
    ##  3 Agapanthus       76.3   8.03        1.01     10
    ##  4 Zantedeschia     66.0  10.7         1.61     10
    ##  5 Ficinia          62.6   9.74        1.76     10
    ##  6 Typha            60.8  15.0         1.86     10
    ##  7 Carpobrotus      54.4  20.4         2.22     10
    ##  8 Elegia           48.2  14.5         2.5      10
    ##  9 Phragmites       36.9   9.34        2.92     10
    ## 10 Control          26.7  12.6         3.28     10

## Comparing Dryland vs Wetland Plant Performance

``` r
# Compare average NO3 removal between the two plant form types
# This shows whether plant habitat type (Dryland/Wetland) affects removal ability

stormwaterdata %>%
  filter(plant.form != "") %>%  # Remove Control rows
  group_by(plant.form) %>%
  summarise(
    mean_no3 = mean(no3),
    sd_no3 = sd(no3),
    mean_outflow = mean(outflow),
    n = n()
  )
```

    ## # A tibble: 2 × 5
    ##   plant.form mean_no3 sd_no3 mean_outflow     n
    ##   <chr>         <dbl>  <dbl>        <dbl> <int>
    ## 1 Dryland        69.0   19.2         1.44    50
    ## 2 Wetland        56.6   16.0         2.04    40

## Planted vs Control: Testing Plant Effectiveness

``` r
# Compare average NO3 removal between planted systems and unplanted control (soil only)
# This determines if plants significantly improve nitrate removal compared to no treatment

# Control data (soil only, no plants)
control_data <- stormwaterdata %>%
  filter(species == "Control") %>%
  summarise(
    mean_no3 = mean(no3),
    sd_no3 = sd(no3),
    mean_outflow = mean(outflow)
  )

# Planted data (all plant species combined)
planted_data <- stormwaterdata %>%
  filter(species != "Control") %>%
  summarise(
    mean_no3 = mean(no3),
    sd_no3 = sd(no3),
    mean_outflow = mean(outflow)
  )
```

``` r
# View control data
control_data
```

    ##   mean_no3   sd_no3 mean_outflow
    ## 1 26.73628 12.57206         3.28

``` r
#view plant data
planted_data
```

    ##   mean_no3   sd_no3 mean_outflow
    ## 1 63.46516 18.78328     1.703667

## Visualizing Results

``` r
# Plot 1: NO3 removal ability ranked by plant species, colored by plant form

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
```

![](code_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
## Plot 2: Average NO3 removal comparison between Dryland and Wetland plants

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
```

![](code_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
