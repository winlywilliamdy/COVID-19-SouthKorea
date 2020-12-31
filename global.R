library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(reshape2)
library(leaflet)
library(glue)
library(tidyr)

options(scipen = 9999)

#data for page 2
time <- read_csv("dataset/Time.csv")

#data for page 3 - province lat&long
reg <- read_csv("dataset/Region.csv")

prov_coord <- reg %>% 
  filter(province == city) %>% 
  select(province, latitude, longitude)

#data for page 3 - province time-series
loc <- read_csv("dataset/TimeProvince.csv")

exact_loc <- loc %>% 
  inner_join(prov_coord)

#----page2----
#wrangling
time_plot <- time %>% 
  mutate_if(is.numeric, as.integer)

time_plot <- time_plot %>% 
  mutate(month = months(time_plot$date))

#factor reordering
time_plot$month <- factor(time_plot$month, levels = 
                            c("January", "February", "March", "April", "May", "June"))

#rename columns
time_plot <- time_plot %>% 
  rename(
    Tanggal = date,
    Test = test,
    Negatif = negative,
    Positif = confirmed,
    Sembuh = released,
    Meninggal = deceased,
    Bulan = month
  )

#melting
time_plot_melted <- subset(time_plot, select = c("Tanggal", "Test", "Negatif", "Positif", "Sembuh", 'Meninggal'))
time_plot_melted <- melt(time_plot_melted, "Tanggal")

#table
table_1 <- subset(time_plot, select = c("Tanggal", "Test", "Negatif", "Positif", "Sembuh", 'Meninggal'))


#----page3----
icon <- makeIcon(
  iconUrl = "asset/corona.png",
  iconWidth= 25, iconHeight=25
)

table_2 <- exact_loc %>% 
  rename(
    Tanggal = date,
    Provinsi = province,
    Positif = confirmed,
    Sembuh = released,
    Meninggal = deceased
  ) %>% 
  select(Tanggal, Provinsi, Positif, Sembuh, Meninggal)


#----page4----
prov <- exact_loc %>% 
  select(date, province, confirmed, released, deceased)%>% 
  rename(
    Tanggal = date,
    Provinsi = province,
    Positif = confirmed,
    Sembuh = released,
    Meninggal = deceased
  )
prov$Provinsi <- as.factor(prov$Provinsi)


#----page5----
demo <- read_csv("dataset/PatientInfo.csv",)
demo <- demo %>% 
  mutate (age = factor(demo$age, levels = 
                         c("0s", "10s", "20s", "30s","40s", "50s","60s", "70s","80s", "90s", "100s"))) %>% 
  rename(
    JenisKelamin = sex,
    Umur = age,
    Kota = city
  ) %>% 
  drop_na(JenisKelamin, Umur)
demo$JenisKelamin <- recode(demo$JenisKelamin, "male"= "Laki-laki")
demo$JenisKelamin <- recode(demo$JenisKelamin, "female"= "Perempuan")
demo$Kota <- recode(demo$Kota, "etc"= "Others")

#----sourcing----
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)