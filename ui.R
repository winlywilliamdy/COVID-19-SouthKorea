ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = 'SOUTH KOREA COVID-19 CASE - 2020'),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem(text = "About", tabName = "menu_item1", icon = icon("info-circle")),
                menuItem(text = "Trend Kasus", tabName = "menu_item2", icon = icon("chart-line")),
                conditionalPanel("input.sidebarmenu === 'menu_item2'",
                                 dateRangeInput(label = "Filter by Date",
                                                inputId = "input_1",
                                                min = min(exact_loc$date),
                                                max = max(exact_loc$date),
                                                start = min(exact_loc$date),
                                                end = max(exact_loc$date)
                                 )),
                menuItem(text = "Peta Sebaran", tabName = "menu_item3", icon = icon("map-marker")),
                conditionalPanel("input.sidebarmenu === 'menu_item3'",
                                 dateRangeInput(label = "Filter by Date",
                                                inputId = "input_2",
                                                min = min(exact_loc$date),
                                                max = max(exact_loc$date),
                                                start = min(exact_loc$date),
                                                end = max(exact_loc$date)
                                 ),
                                 radioButtons (label = "Plot Type",
                                               inputId = "radio1",
                                               choices = c("Pin Point", "Bubble Plot"))),
                menuItem(text = "Trend Provinsi", tabName = "menu_item4", icon = icon("map-marked-alt")),
                conditionalPanel("input.sidebarmenu === 'menu_item4'",
                                 selectInput(
                                   inputId = "input_3",
                                   label = "Provinsi 1:",
                                   choices = levels(prov$Provinsi)),
                                 selectInput(
                                   inputId = "input_4",
                                   label = "Provinsi 2:",
                                   choices = levels(prov$Provinsi), selected = "Ulsan")),
                menuItem(text = "Demografi Provinsi", tabName = "menu_item5", icon = icon("venus-mars")),
                conditionalPanel("input.sidebarmenu === 'menu_item5'",
                                 selectInput(
                                   inputId = "input_5",
                                   label = "Provinsi:",
                                   choices = levels(prov$Provinsi)))
                
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "menu_item1", #body page_1
              fluidPage(
                fluidRow(
                  column = 12,
                  box(width = 12,
                      title = "ABOUT THIS PROJECT",
                      tags$p("Author : Winly Williamdy"),
                      tags$p("Project ini adalah project yang dibuat untuk LBB (Learning by Building) dan Capstone Project
                                 di Algoritma Data Science School pada materi Interactive Plotting. 
                                 Dataset yang digunakan adalah dataset kasus COVID-19 di Korea Selatan dan dataset ini didapatkan
                                 dari Kaggle. Dashboard ini akan menampilkan trend penambahan kasus di Korea Selatan berdasarkan
                                 waktu dan provinsi."),
                      tags$a(href="https://www.kaggle.com/kimjihoo/coronavirusdataset?select=Time.csv", "Data Science for COVID-19 (DS4C)."),
                  )
                ))),
      tabItem(tabName = "menu_item2", #body page_2
              fluidPage(
                fluidRow(
                  column(width = 12,
                         box(width = 12,
                             title = "Trend Kasus COVID-19 di Korea Selatan", 
                             solidHeader = F, 
                             collapsible = T,
                             plotlyOutput(outputId = "plot_1")))
                ),
                fluidRow(
                  column(width = 12,
                         box(width = 12, 
                             title = paste("Angka Kasus COVID-19 di Korea Selatan per Hari"), 
                             solidHeader = F, 
                             collapsible = T,
                             dataTableOutput(outputId = "table_1"),style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                         ))),
              )
      ),
      tabItem(tabName = "menu_item3", #body page_3
              fluidPage(
                fluidRow(
                  column(width = 12,
                         infoBoxOutput("info_1"),
                         infoBoxOutput("info_2"),
                         infoBoxOutput("info_3"),
                         box(width = 12,
                             title = "Peta Sebaran dalam Provinsi selama Rentang Waktu Tertentu",
                             leafletOutput("map_1"))
                         
                  )
                ),
                fluidRow(
                  column(width = 12,
                         box(width = 12, 
                             title = "Akumulasi Kasus COVID-19 di Korea Selatan sesuai Provinsi selama Rentang Waktu Tertentu", 
                             collapsible = T,
                             dataTableOutput(outputId = "table_2"),style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                         )))
              )),
      tabItem(tabName = "menu_item4", #body page_4
              fluidPage(
                fluidRow(
                  column(width = 12,
                         box(width = 12,
                             title = "Perbandingan Trend Kasus COVID-19 pada Provinsi", 
                             solidHeader = F, 
                             collapsible = T,
                             plotlyOutput(outputId = "plot_2",height = "600px"))
                  )
                )
              )),
      tabItem(tabName = "menu_item5", #body page_4
              fluidPage(
                fluidRow(
                  column(width = 12,
                         box(width = 12,
                             title = "Kota dengan Kasus Terbanyak",
                             infoBoxOutput("info_4"),
                             infoBoxOutput("info_5"),
                             infoBoxOutput("info_6"))),
                  column(width = 12,
                         box(width = 12,
                             title = paste("Sebaran Kategori Umur"), 
                             solidHeader = F, 
                             collapsible = T,
                             plotlyOutput(outputId = "plot_6",height = "400px"))
                  )
                )
              )
      )
    )
  )
)
