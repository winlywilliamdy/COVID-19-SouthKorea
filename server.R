server <- function(input, output){
  #line plot 1 - trend kasus keseluruhan
  output$plot_1 <- renderPlotly({
    p1 <- time_plot_melted %>% 
      subset((Tanggal >= date(input$input_1[1]) & Tanggal <= date(input$input_1[2])))%>%
      rename(
        Jumlah = value,
        Kasus = variable
      ) %>% 
      ggplot(aes(x = Tanggal, y = Jumlah)) +
      geom_line(aes(col = Kasus))+
      labs(
        x = "",
        y = "",
        col = "Status"
      )+
      theme_minimal()
    ggplotly(p1, tooltips = "ket")
    
  })
  
  #table1 - trend kasus keseluruhan per hari
  output$table_1 <- renderDataTable({
    table_1 %>% 
      subset((Tanggal >= date(input$input_1[1]) & Tanggal <= date(input$input_1[2])))
  })
  
  #map - jumlah kasus per hari di setiap provinsi
  output$map_1 <- renderLeaflet({
    palette_1 <- colorNumeric("viridis", NULL)
    
    exact_loc <- exact_loc %>% 
      subset((date >= date(input$input_2[1]) & date <= date(input$input_2[2]))) %>% 
      group_by(province) %>% 
      summarise(Positif = sum(confirmed),
                Sembuh = sum (released),
                Meninggal = sum(deceased),
                latitude = max(latitude),
                longitude = max(longitude))
    
    content_popup <- paste0("<b>",toupper(exact_loc$province),"</b>","<br>",
                            "Positif : ",as.numeric(exact_loc$Positif),"<br>",
                            "Sembuh : ",exact_loc$Sembuh,"<br>",
                            "Meninggal : ",exact_loc$Meninggal)
    
    map1 <- leaflet()
    map1 <- addTiles(map1)
    if(input$radio1 == 'Pin Point'){
      map1 <- addMarkers(map = map1,
                         lng = exact_loc$longitude,
                         lat = exact_loc$latitude,
                         popup = content_popup,
                         icon = icon,
                         clusterOptions = T)
    } else{
      map1 <- addCircleMarkers(map = map1,
                               lng = exact_loc$longitude,
                               lat = exact_loc$latitude,
                               popup = content_popup,
                               radius = sqrt(exact_loc$Positif)/12.5,
                               stroke = F,
                               fillColor = palette_1(exact_loc$Meninggal),
                               fillOpacity = 0.5)
      map1 <- addLegend(map = map1,
                        pal = palette_1,
                        values = exact_loc$Meninggal,
                        title = "Jumlah Kasus Meninggal")
    }
    
    map1 <- addProviderTiles(map1, providers$CartoDB.Positron)
    map1
  })
  
  #table2 - jumlah kasus per hari di setiap provinsi
  output$table_2 <- renderDataTable({
    table_2 <- exact_loc %>% 
      subset((date >= date(input$input_2[1]) & date <= date(input$input_2[2]))) %>% 
      group_by(province) %>% 
      summarise(Positif = sum(confirmed),
                Sembuh = sum (released),
                Meninggal = sum(deceased)) %>% 
      rename(Provinsi = province) %>% 
      arrange(desc(Positif))
  })
  
  #infoboxes
  output$info_1 <- renderInfoBox({
    table_pos <- exact_loc %>% 
      subset((date >= date(input$input_2[1]) & date <= date(input$input_2[2])))
    
    infoBox(title = "Akumulasi Kasus Positif",
            value = sum(table_pos$confirmed),
            icon = icon("plus"),
            color = "navy",
            fill = F)
  })
  
  output$info_2 <- renderInfoBox({
    table_sem <- exact_loc %>% 
      subset((date >= date(input$input_2[1]) & date <= date(input$input_2[2])))
    infoBox(title = "Akumulasi Kasus Sembuh",
            value = sum(table_sem$released),
            icon = icon("home"),
            color = "olive",
            fill = F)
  })
  
  output$info_3 <- renderInfoBox({
    table_men <- exact_loc %>% 
      subset((date >= date(input$input_2[1]) & date <= date(input$input_2[2])))
    infoBox(title = "Akumulasi Kasus Meninggal",
            value = sum(table_men$deceased),
            icon = icon("skull-crossbones"),
            color = "black",
            fill = F)
  })
  
  #line plot 2 - trend kasus per hari di provinsi 1, untuk perbandingan
  output$plot_2 <- renderPlotly({
    versus <- melt(prov, c("Tanggal","Provinsi")) %>% 
      filter(Provinsi == c(input$input_3, input$input_4)) %>% 
      rename(
        Jumlah = value,
        Kasus = variable
      )
    
    plot_2 <- versus %>%
      ggplot(aes (x = Tanggal, y=Jumlah))+
      geom_line(aes(fill = Kasus, color = Provinsi))+
      theme_minimal()+
      labs(
        title = paste("<B>",input$input_3 ,"dan", input$input_4,"</B>"),
        x = "",
        y = "",
        col = "Status"
      )+
      theme(panel.spacing = unit(1, "lines"), legend.position = "none")+
      facet_grid(rows = vars(Kasus), scales = "free_y")
    
    ggplotly(plot_2)
  }) 
  
  
  #gender & age group
  output$plot_6 <- renderPlotly({
    province_demo <- input$input_5
    demo_plot <- demo %>%
      filter (province == input$input_5) %>% 
      group_by(JenisKelamin,Umur) %>% 
      summarise(n=n()) %>% 
      mutate(prop = round(n/sum(n),4)*100)%>% 
      mutate(keterangan = paste("Kategori Umur : ", Umur, "\nJenis Kelamin : ", JenisKelamin, "\nPersentase : ", prop,"%")) %>% 
      ggplot(aes(x=Umur, y=prop, text = keterangan))+
      geom_col(aes(fill=JenisKelamin))+
      theme_minimal()+
      scale_fill_manual(values = c("#143D59", "#F4B41A"))+
      labs(
        title = input$input_5,
        x="Kategori Umur",
        y="",
        fill = "Jenis Kelamin"
      )
    
    ggplotly(demo_plot, tooltip = "keterangan")
  })
  
  output$info_4 <- renderInfoBox({
    no_1 <- demo %>% 
      subset(province == input$input_5) %>% 
      group_by(Kota) %>% 
      summarise(n=n()) %>% 
      mutate(perc = n/sum(n)*100) %>% 
      arrange(desc(perc)) %>% 
      head(3) %>% 
      select(Kota, perc)
    
    infoBox(title = "#1",
            value = no_1[1,"Kota"],
            subtitle = paste(round(no_1[1,"perc"],2),"%"),
            icon = icon("city"),
            color = "navy",
            fill = F)
  })
  
  output$info_5 <- renderInfoBox({
    no_1 <- demo %>% 
      subset(province == input$input_5) %>% 
      group_by(Kota) %>% 
      summarise(n=n()) %>% 
      mutate(perc = n/sum(n)*100) %>% 
      arrange(desc(perc)) %>% 
      head(3) %>% 
      select(Kota, perc)
    
    infoBox(title = "#2",
            subtitle = paste(round(no_1[2,"perc"],2),"%"),
            value = no_1[2,"Kota"],
            icon = icon("city"),
            color = "navy",
            fill = F)
  })
  
  output$info_6 <- renderInfoBox({
    no_1 <- demo %>% 
      subset(province == input$input_5) %>% 
      group_by(Kota) %>% 
      summarise(n=n()) %>% 
      mutate(perc = n/sum(n)*100) %>% 
      arrange(desc(perc)) %>% 
      head(3) %>% 
      select(Kota, perc)
    
    infoBox(title = "#3",
            subtitle = paste(round(no_1[3,"perc"],2),"%"),
            value = no_1[3,"Kota"],
            icon = icon("city"),
            color = "navy",
            fill = F)
  })
}