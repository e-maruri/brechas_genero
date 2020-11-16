server = function(input, output) {


# ENOE --------------------------------------------------------------------
  output$graph_enoe_brecha_sal_anual_barras <- renderPlotly({
    enoe_brecha_sal_anual_barras %>%
      filter(year>=input$enoe_brecha_sal_anual_barras_year[1] & year<=input$enoe_brecha_sal_anual_barras_year[2]) %>% 
      plot_ly(x=~get(input$enoe_brecha_sal_anual_barras_var), y=~year, color=~sex, 
              #texttemplate = '$ %{$x:.2s}', textposition = 'outside',
              type = "bar", orientation = "h") %>%
      layout(barmode = 'group', 
             xaxis = list(title = "Pesos del primer trimestre de 2019"),
             yaxis = list(title = ""), 
             title = paste(get_label(enoe_brecha_sal_anual_barras[, input$enoe_brecha_sal_anual_barras_var])) )
  }) # Termina primera de nivel nacional 
  
  output$enoe_brecha_intro_xhora <- renderValueBox({
    enoe_brecha_sal_anual %>%
      filter(year == input$enoe_brecha_intro_year ) %>%
      select(wgap_ixh) %>% 
      summarize(wgap_ixh = mean(wgap_ixh)*100) %>% 
      .$wgap_ixh %>% 
      as.integer() %>% 
      paste("%") %>% 
      valueBox(subtitle = "Brecha salarial en el ingreso promedio por hora",
               icon = icon("wallet"),
               color = "purple")
  }) 
  
  output$enoe_brecha_intro_mensual <- renderValueBox({
    enoe_brecha_sal_anual %>%
      filter(year == input$enoe_brecha_intro_year) %>%
      select(wgap_ing) %>% 
      summarize(wgap_ing = mean(wgap_ing)*100) %>% 
      .$wgap_ing %>% 
      as.integer() %>% 
      paste("%") %>% 
      valueBox(subtitle = "Brecha salarial en el ingreso promedio mensual",
               icon = icon("money-bill-alt"),
               color = "fuchsia")
  }) 
  
  output$graph_enoe_brecha_sal_anual <- renderPlotly({
    enoe_brecha_sal_anual %>%
      filter(year>=input$enoe_brecha_nacional_year[1] & year<=input$enoe_brecha_nacional_year[2]) %>% 
      plot_ly() %>% 
      add_trace(x = ~year, y = ~wgap_ixh, mode = 'markers+lines', name = "Ingreso por hora") %>% 
      add_trace(x = ~year, y = ~wgap_ing, mode = 'markers+lines', name = "Ingreso mensual") %>% 
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = "%"))
  }) # Termina primera de nivel nacional 
  
  output$graph_enoe_brecha_sal_deciles <- renderPlotly({ 
    enoe_brecha_deciles %>%
      plot_ly() %>%
      add_trace(x = ~nqd, y = ~wgap, mode = 'markers+lines', color= ~year) %>%
      layout(#title = "Comparación internacional de indicadores 2000-2020",
        xaxis = list(title = "Deciles"),
        yaxis = list(title = "", tickformat = "%"))
  }) # Termina gráfica por deciles 
  
  output$graph_enoe_brecha_sal_anual_cruzada <- renderPlotly({
    enoe_brecha_sal_anual_cruzada %>%
      filter(year>=input$enoe_brecha_sal_anual_cruzada_year[1] & year<=input$enoe_brecha_sal_anual_cruzada_year[2], 
             sector == input$enoe_brecha_sal_anual_cruzada_sector) %>% 
      plot_ly() %>%
      add_trace(x = ~year, y = ~wgap_ixh, mode = 'markers+lines', color=~nivel_educ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = "%"))
  }) # Termina gráfica sector y nivel educativo
  
  output$graph_enoe_brecha_sal_anual_sector <- renderPlotly({
    enoe_brecha_sal_anual_sector %>%
      filter(year>=input$enoe_brecha_sal_anual_sector_year[1] & year<=input$enoe_brecha_sal_anual_sector_year[2]) %>% 
      plot_ly() %>%
      add_trace(x = ~year, y = ~wgap_ixh, mode = 'markers+lines', color=~sector) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = "%"))
  }) # Termina Termina gráfica sector
  
  output$graph_enoe_brecha_sal_anual_nivel_educ <- renderPlotly({
    enoe_brecha_sal_anual_nivel_educ %>%
      filter(year>=input$enoe_brecha_sal_anual_nivel_educ_year[1] & year<=input$enoe_brecha_sal_anual_nivel_educ_year[2]) %>% 
      plot_ly() %>%
      add_trace(x = ~year, y = ~wgap_ixh, mode = 'markers+lines', color=~nivel_educ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = "%"))
  }) # Termina gráfica nivel educativo
  
  output$graph_enoe_brecha_sal_anual_informal <- renderPlotly({
    enoe_brecha_sal_anual_informal %>%
      filter(year>=input$enoe_brecha_sal_anual_informal_year[1] & year<=input$enoe_brecha_sal_anual_informal_year[2]) %>% 
      plot_ly() %>%
      add_trace(x = ~year, y = ~wgap_ixh, mode = 'markers+lines', color=~emp_ppal) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = "%"))
  }) # Termina gráfica formales/informales 
  
  
  output$enoe_brecha_sal_edos_map <- renderPlotly({
    
    enoe_map_temp <- enoe_brecha_sal_anual_edos %>% 
      filter(year==input$enoe_brecha_sal_edos_year)
    
    mapeo_edos <- left_join(mapa_est, enoe_map_temp, by = "CVE_EDO")
    
    ggplotly(
      mapeo_edos %>% 
        ggplot(aes(label=ENTIDAD)) +
        geom_sf(aes(fill=wgap_ixh)) + 
        scale_fill_viridis(option = "viridis", direction = -1)+
        theme(panel.background= element_rect(fill = "white"),
              panel.grid=element_blank(),
              legend.background = element_rect(),
              legend.position = c(0.9, 0.7),
              legend.title =element_text(size=10, face="bold"),
              axis.ticks = element_blank(),
              axis.text = element_blank())+
        labs(fill = "Porcentaje"), 
    )
  }) # Termina Mapa de brecha salarial 
  
  output$graph_enoe_empleo <- renderPlotly({
    enoe_empleo %>%
      filter(year>=input$enoe_empleo_year[1] & year<=input$enoe_empleo_year[2]) %>% 
      plot_ly() %>%
      add_trace(x = ~date, y = ~get(input$enoe_var_emp), mode = 'markers+lines', color=~sex) %>%
      layout(title = paste(get_label(enoe_empleo[, input$enoe_var_emp])), 
             xaxis = list(title = ""),
             yaxis = list(title = "", ticksuffix = "%"))
  }) # Termina gráfica empleo a nivel agregado 
  
  output$graph_enoe_empleo_econ <- renderPlotly({
    enoe_empleo_econ %>%
      filter(year==input$enoe_empleo_econ_year & q==input$enoe_empleo_econ_q) %>% 
      plot_ly() %>%
      add_trace(x = ~e_con, y = ~get(input$enoe_empleo_econ_var), type = "bar", color=~sex) %>%
      layout(title = paste(get_label(enoe_empleo_econ[, input$enoe_empleo_econ_var])), 
             xaxis = list(title = ""),
             yaxis = list(title = "", ticksuffix = "%"))
  }) # Termina gráfica empleo por estado conyugal 
  
  output$graph_enoe_empleo_educ <- renderPlotly({
    enoe_empleo_educ %>%
      filter(year==input$enoe_empleo_educ_year & q==input$enoe_empleo_educ_q) %>% 
      plot_ly() %>%
      add_trace(x = ~nivel_educ, y = ~get(input$enoe_empleo_educ_var), type = "bar", color=~sex) %>%
      layout(title = paste(get_label(enoe_empleo_educ[, input$enoe_empleo_educ_var])), 
             xaxis = xform,
             yaxis = list(title = "", ticksuffix = "%"))
  }) # Termina gráfica empleo por nivel educativo 
  
  
  output$enoe_empleo_edos_map <- renderPlotly({
    
    enoe_map_emp_temp <- enoe_empleo_edos %>% 
      filter(year==input$enoe_empleo_edos_year & sex==input$enoe_empleo_edos_sex)
    
    mapeo_edos2 <- left_join(mapa_est, enoe_map_emp_temp, by = "CVE_EDO")
    
    ggplotly(
      mapeo_edos2 %>% 
        ggplot(aes(label=ENTIDAD)) +
        geom_sf(aes(fill=get(input$enoe_empleo_edos_var))) + 
        scale_fill_viridis(option = "viridis", direction = -1)+
        theme(panel.background= element_rect(fill = "white"),
              panel.grid=element_blank(),
              legend.background = element_rect(),
              legend.position = c(0.9, 0.7),
              legend.title =element_text(size=10, face="bold"),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              plot.title = element_text(hjust = 0.58))+
        labs(fill = "Porcentaje", 
             title = paste(get_label(enoe_map_emp_temp[, input$enoe_empleo_edos_var])) )
    ) 
  }) # Termina Mapa de brecha salarial 
  
  
# ENUT --------------------------------------------------------------------
  output$RawData<-DT::renderDataTable({   #primera pestaÃÂ±a
    if(input$sexo==1){
      distribucion<-dist_hom
    }
    if (input$sexo==2){
      distribucion<-dist_muj
    }
    DT::datatable({
      distribucion #nombre del dataset
    },
    options = list(lengthMenu=list(c(10,20, 33), c('10', '20', 'All')), #nÃÂºmero de observaciones que aparecen en la tabla
                   pageLength=33),
    filter="top", #hace la tabla mÃÂ¡s interactiva
    selection='multiple',
    style='bootstrap'
    )
  })
  
  output$mapahrs<-renderPlotly({
    if(input$trabajo==0){
      data1<-mapa2$estados.br_linghr
      titulo<-"Diferencia Logarítmica"}
    if(input$trabajo==1){
      data1<-mapa2$estados.br_ttt2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==2){
      data1<-mapa2$estados.br_rem2 
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==3){
      data1<-mapa2$estados.br_norem2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==4){
      data1<-mapa2$estados.br_prod2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==5){
      data1<-mapa2$estados.br_domestico2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==6){
      data1<-mapa2$estados.br_cuidado2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==7){
      data1<-mapa2$estados.br_comun2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==8){
      data1<-mapa2$estados.br_alim2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==9){
      data1<-mapa2$estados.br_limpviv2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==10){
      data1<-mapa2$estados.br_limprop2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==11){
      data1<-mapa2$estados.br_rep2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==12){
      data1<-mapa2$estados.br_cui52
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==13){
      data1<-mapa2$estados.br_cui142
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==14){
      data1<-mapa2$estados.br_cui602
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    if(input$trabajo==15){
      data1<-mapa2$estados.br_estudio2
      titulo<-"Diferencia Promedio en Horas de Trabajo a la Semana"}
    
    mapa2<-mapa2%>%
      mutate(namemex=sprintf("<b>Estado:</b> %s <br><b>Brecha:</b> %s",estados.entidad, round(data1,3))) %>%
      mutate(groupmap=1)
    cart1<-ggplot(data = mapa2, aes(text=namemex, group=groupmap)) +
      geom_sf(aes(fill = data1), color=NA)+#, color = NA)+ #con color=NA se quitan las lineas del mapa
      #con esto se ponen los colores del mapa
      scale_fill_viridis(option = "viridis", direction = -1)+
      theme(panel.background= element_rect(fill = "cadetblue1"),
            #con esto se quitan las lineas de fondo
            panel.grid=element_blank(),
            #con esto se pone el color de fondo de la leyenda
            legend.background = element_rect(),
            #con esto se pone la posicion de la leyenda en el grafico
            legend.position = c(0.9, 0.7),
            #con esto se pone el tamaño de letra de la leyenda, bold es negrita
            legend.title =element_text(size=10, face="bold"),
            #con esto se quitan las marquitas de los bordes
            axis.ticks = element_blank(),
            #con esto se quitan los nombres en los bordes
            axis.text = element_blank())+
      #titulo de la leyenda
      labs(fill = "Brecha entre \nmujeres y \nhombres", title=titulo, caption="Fuente: Elaboración porpia con datos de la ENUT.")
    cart2<-ggplotly(cart1, tooltip="text")
    return(cart2)
  })
  
  output$hrshombres<-renderPlotly({
    if(input$trabajo==0){
      datah<-estados$h_linghr
      tituloh<-"Logaritmo del Ingreso por Hora"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Log ingreso por hora:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah, 2), round(br_linghr,4))) %>%
        mutate(group1=1)}
    if(input$trabajo==1){
      datah<-estados$h_ttt2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_ttt2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==2){
      datah<-estados$h_rem2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_rem2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==3){
      datah<-estados$h_norem2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_norem2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==4){
      datah<-estados$h_prod2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_prod2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==5){
      datah<-estados$h_domestico2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_domestico2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==6){
      datah<-estados$h_cuidado2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_cuidado2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==7){
      datah<-estados$h_comun2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_comun2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==8){
      datah<-estados$h_alim2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_alim2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==9){
      datah<-estados$h_limpviv2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_limpviv2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==10){
      datah<-estados$h_limprop2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_limprop2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==11){
      datah<-estados$h_rep2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_rep2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==12){
      datah<-estados$h_cui52
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_cui52,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==13){
      datah<-estados$h_cui142
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_cui142,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==14){
      datah<-estados$h_cui602
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_cui602,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==15){
      datah<-estados$h_estudio2
      tituloh<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqh=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datah,2), round(br_estudio2,2))) %>%
        mutate(group1=1)}
    
    men<-ggplot(estados, aes(y=datah, x=abrev, text=etiqh, group=group1))+geom_bar(stat="identity", fill="seagreen4") +
      ggtitle("HOMBRES")+theme_light()+ xlab('Estado')+ theme(axis.text.x = element_text(size=6, angle=45)) +
      ylab(tituloh)
    men2<-ggplotly(men, tooltip="text")
    return(men2)
  })
  
  output$hrsmujeres<-renderPlotly({
    if(input$trabajo==0){
      datam<-estados$m_linghr
      titulom<-"Logaritmo del Ingreso por Hora"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Log ingreso por hora:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam, 2), round(br_linghr,4))) %>%
        mutate(group1=1)}
    if(input$trabajo==1){
      datam<-estados$m_ttt2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_ttt2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==2){
      datam<-estados$m_rem2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_rem2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==3){
      datam<-estados$m_norem2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_norem2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==4){
      datam<-estados$m_prod2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_prod2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==5){
      datam<-estados$m_domestico2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_domestico2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==6){
      datam<-estados$m_cuidado2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_cuidado2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==7){
      datam<-estados$m_comun2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_comun2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==8){
      datam<-estados$m_alim2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_alim2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==9){
      datam<-estados$m_limpviv2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_limpviv2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==10){
      datam<-estados$m_limprop2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_limprop2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==11){
      datam<-estados$m_rep2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_rep2,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==12){
      datam<-estados$m_cui52
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_cui52,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==13){
      datam<-estados$m_cui142
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_cui142,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==14){
      datam<-estados$m_cui602
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_cui602,2))) %>%
        mutate(group1=1)}
    if(input$trabajo==15){
      datam<-estados$m_estudio2
      titulom<-"Horas promedio de trabajo a la semana"
      estados<-estados%>%
        mutate(etiqm=sprintf("<b>Estado:</b> %s <br><b>Hrs por semana:</b> %s <br><b>Brecha:</b> %s", entidad, round(datam,2), round(br_estudio2,2))) %>%
        mutate(group1=1)}
    
    women<-ggplot(estados, aes(y=datam, x=abrev, text=etiqm, group=group1))+geom_bar(stat="identity", fill="mediumpurple4") +
      ggtitle("MUJERES")+theme_light()+ xlab('Estado')+ theme(axis.text.x = element_text(size=6, angle=45)) +
      ylab(titulom)
    women2<-ggplotly(women, tooltip="text")
    return(women2)
  })
  
  output$mapapart<-renderPlotly({
    if(input$trabajop==1){
      datap<-mapa2$estados.br_parrem}
    if(input$trabajop==2){
      datap<-mapa2$estados.br_parnorem}
    if(input$trabajop==3){
      datap<-mapa2$estados.br_parprod}
    if(input$trabajop==4){
      datap<-mapa2$estados.br_pardomestico}
    if(input$trabajop==5){
      datap<-mapa2$estados.br_parcuidado}
    if(input$trabajop==6){
      datap<-mapa2$estados.br_parcomun}
    if(input$trabajop==7){
      datap<-mapa2$estados.br_paralim}
    if(input$trabajop==8){
      datap<-mapa2$estados.br_parlimpviv}
    if(input$trabajop==9){
      datap<-mapa2$estados.br_parlimprop}
    if(input$trabajop==10){
      datap<-mapa2$estados.br_parrep}
    if(input$trabajop==11){
      datap<-mapa2$estados.br_parcui5}
    if(input$trabajop==12){
      datap<-mapa2$estados.br_parcui14}
    if(input$trabajop==13){
      datap<-mapa2$estados.br_parcui60}
    if(input$trabajop==14){
      datap<-mapa2$estados.br_parestudio}
    
    mapa2<-mapa2%>%
      mutate(namemex2=sprintf("<b>Estado:</b> %s <br><b>Brecha en la Tasa:</b> %s", estados.entidad, round(datap,3))) %>%
      mutate(groupp=1)
    plotpart<- ggplot(data = mapa2, aes(text=namemex2, group=groupp)) +
      geom_sf(aes(fill = datap), color=NA)+#, color = NA)+ #con color=NA se quitan las lineas del mapa
      #con esto se ponen los colores del mapa
      scale_fill_viridis(option = "magma", direction = -1)+
      theme(panel.background= element_rect(fill = "cadetblue1"),
            #con esto se quitan las lineas de fondo
            panel.grid=element_blank(),
            #con esto se pone el color de fondo de la leyenda
            legend.background = element_rect(),
            #con esto se pone la posicion de la leyenda en el grafico
            legend.position = c(0.9, 0.7),
            #con esto se pone el tamaño de letra de la leyenda, bold es negrita
            legend.title =element_text(size=10, face="bold"),
            #con esto se quitan las marquitas de los bordes
            axis.ticks = element_blank(),
            #con esto se quitan los nombres en los bordes
            axis.text = element_blank())+
      #titulo de la leyenda
      labs(fill = "Brecha entre \nmujeres y \nhombres", title="Brecha en las tasas de participación", caption="Fuente: Elaboración porpia con datos de la ENUT.")
    plotpart2<-ggplotly(plotpart, tooltip="text")
    return(plotpart2)
  })
  
  output$parthombres<-renderPlotly({
    if(input$trabajop==1){
      parth<-estados$hparrem
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parrem,4))) %>%
        mutate(group3=1)}
    if(input$trabajop==2){
      parth<-estados$hparnorem
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parnorem,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==3){
      parth<-estados$hparprod
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parprod,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==4){
      parth<-estados$hpardomestico
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_pardomestico,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==5){
      parth<-estados$hparcuidado
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parcuidado,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==6){
      parth<-estados$hparcomun
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parcomun,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==7){
      parth<-estados$hparalim
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_paralim,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==8){
      parth<-estados$hparlimpviv
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parlimpviv,3))) %>%
        mutate(group3=1) }
    if(input$trabajop==9){
      parth<-estados$hparlimprop
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parlimprop,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==10){
      parth<-estados$hparrep
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parrep,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==11){
      parth<-estados$hparcui5
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parcui5,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==12){
      parth<-estados$hparcui14
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parcui14,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==13){
      parth<-estados$hparcui60
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parcui60,3))) %>%
        mutate(group3=1)}
    if(input$trabajop==14){
      parth<-estados$hparestudio
      estados<-estados%>%
        mutate(etiqhp=sprintf("<b>Estado:</b> %s <br><b>Tasa de Participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(parth,4), round(br_parestudio,3))) %>%
        mutate(group3=1)}
    
    menp<-ggplot(estados, aes(y=parth, x=abrev, text=etiqhp, group=group3))+geom_bar(stat="identity", fill="seagreen4") +
      ggtitle("HOMBRES")+theme_light()+ xlab('Estado')+ theme(axis.text.x = element_text(size=6, angle=45)) +
      ylab("Tasa de participación")
    men2p<-ggplotly(menp, tooltip="text")
    return(men2p)
  })
  
  output$partmujeres<-renderPlotly({
    if(input$trabajop==1){
      partm<-estados$parrem
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parrem,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==2){
      partm<-estados$parnorem
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parnorem,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==3){
      partm<-estados$parprod
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parprod,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==4){
      partm<-estados$pardomestico
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_pardomestico,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==5){
      partm<-estados$parcuidado
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parcuidado,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==6){
      partm<-estados$parcomun
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parcomun,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==7){
      partm<-estados$paralim
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_paralim,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==8){
      partm<-estados$parlimpviv
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parlimpviv,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==9){
      partm<-estados$parlimprop
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parlimprop,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==10){
      partm<-estados$parrep
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parrep,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==11){
      partm<-estados$parcui5
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parcui5,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==12){
      partm<-estados$parcui14
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parcui14,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==13){
      partm<-estados$parcui60
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parcui60,3))) %>%
        mutate(group4=1)}
    if(input$trabajop==14){
      partm<-estados$parestudio
      estados<-estados%>%
        mutate(etiqmp=sprintf("<b>Estado:</b> %s <br><b>Tasa de participación:</b> %s <br><b>Brecha:</b> %s", entidad, round(partm,4), round(br_parestudio,3))) %>%
        mutate(group4=1)}
    
    womenp<-ggplot(estados, aes(y=partm, x=abrev, text=etiqmp, group=group4))+geom_bar(stat="identity", fill="mediumpurple4") +
      ggtitle("MUJERES")+theme_light()+ xlab('Estado')+ theme(axis.text.x = element_text(size=6, angle=45)) +
      ylab("Tasa de participación")
    women2p<-ggplotly(womenp, tooltip="text")
    return(women2p)
  })
  
  output$hrsmex<-renderPlotly({
    if(input$mextrab==0){
      mex1<-nacional$salario
      titulomex<-"Logaritmo del Ingreso por Hora"
      titulomex2<-"Brecha en el Ingreso por Hora"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Log ingreso por hora:</b> %s", round(mex1, 4))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==1){
      mex1<-nacional$ttt
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo Total de Trabajo"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==2){
      mex1<-nacional$rem
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo de Trabajo Remunerado"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==3){
      mex1<-nacional$norem
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo de Trabajo No Remunerado"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==4){
      mex1<-nacional$prod
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo de Trabajo para Producción de Bienes para el Hogar"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==5){
      mex1<-nacional$domestico
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo de Trabajo Doméstico"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==6){
      mex1<-nacional$cuidado
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo de Trabajo de Cuidado"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==7){
      mex1<-nacional$comun
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo de Trabajo Comuniatrio"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==8){
      mex1<-nacional$alim
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo dedicado a la Preparación de Alimentos"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==9){
      mex1<-nacional$limpviv
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo  dedicado a la Limpieza de la Vivienda"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==10){
      mex1<-nacional$limprop
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo dedicado a la Limpieza de Ropa"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==11){
      mex1<-nacional$rep
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo dedicado a Reparaciones Domésticas"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==12){
      mex1<-nacional$cui5
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo dedicado al Cuidado de Menores de 5 años"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==13){
      mex1<-nacional$cui14
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo dedicado al Cuidado de Menores de 14 años"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==14){
      mex1<-nacional$cui60
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo dedicado al Cuidado de Mayores de 60 años"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    if(input$mextrab==15){
      mex1<-nacional$estudio
      titulomex<-"Horas promedio de trabajo a la semana"
      titulomex2<-"Tiempo dedicado al Estudio"
      nacional<-nacional%>%
        mutate(label_nac1=sprintf("<b>Hrs de trabajo a la semana:</b> %s", round(mex1, 1))) %>%
        mutate(nacgroup1=1)}
    
    nacional$year<-as.factor(nacional$year)
    plotnac1<-ggplot(data=nacional, aes(y=mex1, x=year, fill=sex, text=label_nac1, group=nacgroup1))+geom_bar(stat="identity", position=position_dodge2())+
      scale_fill_viridis(discrete="TRUE")+ theme_gray()+coord_flip()+labs(fill="Sexo", title=titulomex2)+
      xlab("Año")+ ylab(titulomex)
    plotnac2<-ggplotly(plotnac1, tooltip="text")
    return(plotnac2)
  })
  
  output$partmex<-renderPlotly({
    if(input$mextrab==0){
      mex2<-nacional$salario
      titulomex3<-"Logaritmo del Ingreso por Hora"
      titulomex4<-"Brecha en el Ingreso por Hora"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Log ingreso por hora:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==1){
      mex2<-nacional$parttt
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Trabajo en General"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==2){
      mex2<-nacional$parrem
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Trabajo Remunerado"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción::</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==3){
      mex2<-nacional$parnorem
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Trabajo No Remunerado"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==4){
      mex2<-nacional$parprod
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en la Producción de Bienes para el Hogar"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==5){
      mex2<-nacional$pardomestico
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Trabajo Doméstico"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==6){
      mex2<-nacional$parcuidado
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en los Trabajos de Cuidado"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==7){
      mex2<-nacional$parcomun
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Trabajo Comuniatrio"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==8){
      mex2<-nacional$paralim
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en la Preparación de Alimentos"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==9){
      mex2<-nacional$parlimpviv
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación la Limpieza de la Vivienda"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==10){
      mex2<-nacional$parlimprop
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en la Limpieza de Ropa"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==11){
      mex2<-nacional$parrep
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en las Reparaciones Domésticas"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==12){
      mex2<-nacional$parcui5
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Cuidado de Menores de 5 años"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==13){
      mex2<-nacional$parcui14
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Cuidado de Menores de 14 años"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==14){
      mex2<-nacional$parcui60
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Cuidado de Mayores de 60 años"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    if(input$mextrab==15){
      mex2<-nacional$parestudio
      titulomex3<-"Proporción de participación"
      titulomex4<-"Tasa de Participación en el Estudio"
      nacional<-nacional%>%
        mutate(label_nac2=sprintf("<b>Proporción:</b> %s", round(mex2, 4))) %>%
        mutate(nacgroup2=1)}
    
    nacional$year<-as.factor(nacional$year)
    plotnac3<-ggplot(data=nacional, aes(y=mex2, x=year, fill=sex, text=label_nac2, group=nacgroup2))+geom_bar(stat="identity", position=position_dodge2())+
      scale_fill_viridis(discrete="TRUE")+ theme_gray()+coord_flip()+labs(fill="Sexo", title=titulomex4)+
      xlab("Año")+ ylab(titulomex3)
    plotnac4<-ggplotly(plotnac3, tooltip="text")
    return(plotnac4)
  })
  
  output$enutregcuantil<-renderPlotly({
    if (input$enutcoef==1){
      pvarqreg<-qreg$mujer
      ptitreg<-"Coeficiente por ser mujer"
      qreg<-qreg %>%
        mutate(plabelreg=sprintf("<b> Cuantil:</b> %s <br><b> Coeficiente:</b> %s", n, round(mujer,2)))}
    if (input$enutcoef==2){
      pvarqreg<-qreg$indigena
      ptitreg<-"Coeficiente por ser indígena"
      qreg<-qreg %>%
        mutate(plabelreg=sprintf("<b> Cuantil:</b> %s <br><b> Coeficiente:</b> %s", n, round(indigena,2))) 
    }
    if (input$enutcoef==3){
      pvarqreg<-qreg$mujind
      ptitreg<-"Coeficiente por ser mujer indígena"
      qreg<-qreg %>%
        mutate(plabelreg=sprintf("<b> Cuantil:</b> %s <br><b> Coeficiente:</b> %s", n, round(mujind,2)))
    }
    if (input$enutcoef==4){
      pvarqreg<-qreg$soltero
      ptitreg<-"Coeficiente por ser soltero"
      qreg<-qreg %>%
        mutate(plabelreg=sprintf("<b> Cuantil:</b> %s <br><b> Coeficiente:</b> %s", n, round(soltero,2)))}
    
    qreg$year<-as.factor(qreg$year)
    enutplotreg<-ggplot(qreg, aes(x=n, y=pvarqreg, group=year, color=year, text=plabelreg))+geom_line(size=2)+xlab("Cuantil")+
      ylab(ptitreg)+
      labs(title="Regresión Cuantil para explicar las horas de trabajo doméstico", color="Año")+
      theme_minimal()+scale_color_manual(values=c("violetred4", "purple4"))
    enutplotreg2<-ggplotly(enutplotreg, tooltip="text")  
    return(enutplotreg2)
  })
  
  output$pdem<-renderPlotly({
    if (input$pactividad==1){
      pxlab<-"Diferenecia Logarítmica"
      enutgroup<-brechas$linghr}
    if (input$pactividad==2){
      pxlab<-"Diferencia en horas de trabajo semanales"
      enutgroup<-brechas$ttt2
    }
    if (input$pactividad==3){
      pxlab<-"Diferencia en horas de trabajo semanales"
      enutgroup<-brechas$hrs_rem
    }
    if (input$pactividad==4){
      pxlab<-"Diferencia en horas de trabajo semanales"
      enutgroup<-brechas$norem2
    }
    if (input$pactividad==5){
      pxlab<-"Diferencia en horas de trabajo semanales"
      enutgroup<-brechas$prod_hogar2
    }
    if (input$pactividad==6){
      pxlab<-"Diferencia en horas de trabajo semanales"
      enutgroup<-brechas$domestico2
    }
    if (input$pactividad==7){
      pxlab<-"Diferencia en horas de trabajo semanales"
      enutgroup<-brechas$cuidado2
    }
    if (input$pactividad==8){
      pxlab<-"Diferncia en la tasa de participación"
      enutgroup<-brechas$part_ocu
    }
    if (input$pactividad==9){
      pxlab<-"Diferncia en la tasa de participación"
      enutgroup<-brechas$part_dom
    }
    if (input$pactividad==10){
      pxlab<-"Diferncia en la tasa de participación"
      enutgroup<-brechas$part_cuidado
    }
    
    brechas<-brechas %>%
      mutate(plabdem=sprintf("<b>Año:</b> %s <br><b>Brecha: </b> %s <br><b>Grupo: </b> %s", 
                             year, round(enutgroup, 3), grupo))
    
    brechas$year<-as.factor(brechas$year)
    pgruposdem<-ggplot(brechas, aes(y=enutgroup, x=reorder(grupo, -enutgroup), fill=year, text=plabdem))+geom_bar(stat="identity", position = position_dodge2())+
      scale_fill_manual(values=c("seagreen4", "royalblue4"))+theme_bw()+
      theme(axis.text.x = element_text(size=8, angle=90))+
      labs(title="Brecha entre mujeres y hombres por grupo demográfico",
           fill="Año")+
      xlab("Grupo")+ylab(pxlab)
    pgruposdem2<-ggplotly(pgruposdem, tooltip = "text")
    return(pgruposdem2)
    
  })
  
  output$enutpie<-renderPlotly({
    if (input$ppie==1){
      pvarpie<-piedata$ind
    }
    if (input$ppie==2){
      pvarpie<-piedata$noind
    }
    enutpiechart<-plot_ly(labels=~piedata$trabajo, values=~pvarpie, sort=FALSE,
            marker=list(colors=piedata$trabajo, line=list(color="white", width=2))) %>%
      add_pie(hole=0.6)%>%
      layout(title="Distribución del Uso del Tiempo")
    return(enutpiechart)
    
  })
  
}