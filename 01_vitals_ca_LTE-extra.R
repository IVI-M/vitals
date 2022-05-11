


if (F) {
  
  
  
  # ```{r r.dt0 r.dtAll} -----
  
  if (input$read_from=="Cached Data") {
    dt = copy(dtCached)
  } 
  
  
  
  if (input$per_million==T) {
    dt <- dtGeo[dtCached, on="GEO"]
    dt [, GEO:=fct_relevel(GEO, choicesGEO)]
    dt [, value:=round(1000000*value/population)]
    setcolorder(dt, c("Date",  "GEO", "population", "Cause of death (ICD-10)", "value"))
  } 
  
  if (input$average==T) {
    convolution_window <- 3 
    dt[, value := frollmean(value, convolution_window, align = "right", fill = 0), by = .(GEO, `Cause of death (ICD-10)`)]
  }
  
  # if (input$clumpAllOtherCauses==T) {
  #   
  # }
  
  
  
  if (input$vaccination==F) {
    dtAll = dt 
  } else {
    dtVacAllAgesAllSexes <- # because causes don report age ad sex
      dtVac [sex == "All sexes", lapply(.SD, sum, na.rm=T), by=c("GEO", "Date"), .SD=colValues]
    
    # dtAll <- dtVacAllAgesAllSexes[dt, on=.(prename=GEO, week_end=Date)]
    dtAll <- dtVacAllAgesAllSexes[dt, on=c("GEO", "Date")]
    dtAll[, GEO:=fct_relevel(GEO, choicesGEO)]
    
    dtAll [, numtotal_atleast1dose:=round(100*numtotal_atleast1dose/population)]
    dtAll [, numtotal_fully:=round(100*numtotal_fully/population)]
  }
  
  
  
  
  q <- quote(Date >= input$date[1] & Date <= input$date[2] & GEO %in% input$state & as.character(`Cause of death (ICD-10)`) %in% input$cause)
  
  dt0 <- dtAll[eval(q)] # for testing offline
  # r.dtAll()[
  
  
  
  # ```{r r.dt0_wideGeo1 dygraphs} ----
  
  
  region1 <- dt0$GEO %>% unique() %>% .[1]; region1
  
  # dt0_wideGeo1 <- dt0[Date >= "2020-01-01" & GEO==region1, !c("GEO"), with=T] %>% dcast(... ~ `Cause of death (ICD-10)`)
  dt0_wideGeo1 <- dt0[Date >= "2020-01-01" & GEO == region1, !c("GEO"), with = T] %>% dcast( Date ~ `Cause of death (ICD-10)`, value.var = "value")
  # dt0_wideGeo1 <- dt0[Date >= "2020-01-01" & GEO == region1, !c("GEO"), with = T] %>% dcast( ... ~ `Cause of death (ICD-10)`, value.var = "value")
  
  dt0_wideGeo1
  
  
  setcolorder(dt0_wideGeo1, "Date")
  dts <- as.xts.data.table(dt0_wideGeo1)
  
  dygraph.title(dts, region1)
  
}



if (F) { #  > Yearly tables for Trent----
  
  dtAll <- copy(dtCached)
  
  dtAll %>% names()
  
  dtAll$`Cause of death (ICD-10)` %>% unique
  
  dtAll [GEO == "Canada"]$value %>% sum(na.rm = T) # 6,362,005
  
  
  dtAll [`Cause of death (ICD-10)` == "Total, all causes of death [A00-Y89]" & GEO == "Canada", .(total=sum(value, na.rm = T)), by=year(Date)]
  
  dtAll [`Cause of death (ICD-10)` == "Malignant neoplasms [C00-C97]" & GEO == "Canada", sum(value, na.rm = T), by=year(Date)]
  
  dtAll [`Cause of death (ICD-10)` == "Intentional self-harm (suicide) [X60-X84, Y87.0]" & GEO == "Canada", .(totals=sum(value, na.rm = T)), by=year(Date)]  
  
  dtAll [`Cause of death (ICD-10)` == "COVID-19 [U07.1,U07.2]" & GEO == "Canada", sum(value, na.rm = T), by=year(Date)]
  
  dtAll [  GEO == "Canada", sum(value, na.rm = T), by=.(year(Date),`Cause of death (ICD-10)`)] 
  
  dtAll [  GEO == "Canada", sum(value, na.rm = T), by=.(`Cause of death (ICD-10)`)]  
  
  
  q <- quote(Date >= input$date[1] &  Date <= input$date[2] & GEO %in% input$state & as.character(`Cause of death (ICD-10)`) %in% input$cause )
  
  
  dt0 <- dtAll[GEO == "Canada" & Date >= "2020-01-01" &  Date <= "2021-01-01" ]
  dt0 <- dtAll[GEO == "Canada" ]
  
  dt0[, date_year := year(Date)]
  dtAll[, date_year := year(Date)]
  # dt0[, year := year(Date)]
  
  
  dtS <- dtAll [  , .(
    `value`=sum(value, na.rm=T), 
    `Mean`=mean(value, na.rm = T) %>% round(0) 
    # `Weekly dynamics`=( lm(value ~ as.integer( Date - min(Date)),  na.action=na.omit)$coefficients[2] * 7 ) %>% round(1)
  ),  by=.(`Cause of death (ICD-10)`,GEO, date_year)] %>% setnames("date_year", "Date")
  
  
  
  dtSwide <- dtS [GEO == "Canada", !c("GEO"), with = T] %>% dcast( Date ~ `Cause of death (ICD-10)`, value.var = "value")
  
  
  dtSwide[, Date:=ymd(Date, truncated = 2L)]
  dtSwide %>% datatable.title("Deaths yearly") 
  
  fwrite(dtSwide, "All-deaths-yearly-totals.csv", sep = "\t")
  
  dtSwide %>% dygraph.title( "Canada: deaths by causes yearly")
  
  convolution_window <- 4 
  
  # > ggplot(dt) CANSIM Excess  <- -----
  
  dt.g <- dt[GEO != "Yukon"
  ][, GEO:=ifelse(GEO %in% c("Nunavut", "Northwest Territories"), "Nunavut and NWT", GEO) 
  ][, .(value=sum(value, na.rm=T)), by = .(Date, GEO, Age, Characteristics)
    ][, ':=' (value = frollmean(value, convolution_window, align = "right", fill = NA)), by = .(GEO, Age)
  ][ Date > ymd("2018-01-02") ]
  
  choicesCharacteristics <-  dt.g$Characteristics %>% unique()
  
  
  str_age = "0 to 44 years"
  i=7
  
  for (i in c(1,4,7)) {
    for (str_age in dt$Age %>% unique()) {
      g <-  
        ggplot() +  
        theme(legend.position = "bottom") +
        geom_vline(xintercept = ymd("2020-01-01"), linetype=1, alpha=0.2) +
        geom_vline(xintercept = ymd("2021-01-01"), linetype=1, alpha=0.4) +
        geom_vline(xintercept = ymd("2021-05-01"), linetype=1, size=2, alpha=0.4) +
        geom_hline(yintercept = 0, col="grey") +
        
        # geom_line(aes(Date, value-ave, col = `Cause of death (ICD-10)`)) +
        geom_line(data=dt.g [ Age == str_age & Characteristics == choicesCharacteristics[i]], 
                  mapping=aes(Date, value),  alpha=0.5, col = "black",linetype=1) +
        geom_line(data=dt.g [ Age == str_age & Characteristics == choicesCharacteristics[i+1]], 
                  mapping=aes(Date, value),  alpha=0.5, col = "blue", linetype=1) +
        
        geom_line(data=dt.g [ Age == str_age & Characteristics == choicesCharacteristics[i+2]], 
                  mapping=aes(Date, value),  alpha=0.5, col = "red", linetype=1) +
        
        
        # facet_grid(GEO ~ ., scales = "free")
        facet_wrap(GEO ~ ., scales = "free") +
        # facet_wrap(GEO ~ `Cause of death (ICD-10)`, scales = "free")
        
        # guides(x =  guide_axis(angle = 90)) +
        # guides(col="none")  +
        # scale_x_date(guide = guide_axis(n.dodge = 2)) +
        # scale_x_discrete(guide = guide_axis(angle = 90)) +
        # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs( title = paste0(choicesCharacteristics[i]),
              subtitle=paste0("Age group: ", str_age), 
              x = "Red line: Upper 95% prediction interval. Blue line: Lower 95% prediction interval",
              # y = ifelse(F,  "Deaths per million per week", "Deaths per week"),
              y="Deaths per week", # 13-10-0768-01 (age); 13-10-0810-01 (cause); Table 13-10-0792-01 (excess)
              caption = "Source: Statistics Canada - Table 13-10-0792-01\n Generated by Open Canada Deaths Tracker App (ivim.ca/app)"     )  
      
      
      print(c(str_age, choicesCharacteristics[i]))
      ggsave(paste0("../", choicesCharacteristics[i], "-", str_age, ".png"), width = 15, height = 9)
      
    }
  }
  
  
  # > ggplot(dtExcess1)  <- -----
  
  
  
  dt.g <- dtExcess[, GEO:=ifelse(GEO %in% c("Nunavut", "Northwest Territories"), "Nunavut and NWT", GEO)
  ][, .(value=sum(value, na.rm=T), ave=sum(ave, na.rm=T)), by = .(Date, GEO, `Cause of death (ICD-10)`)
  ][ Date > ymd("2020-01-07") &
       GEO != "Yukon" &  `Cause of death (ICD-10)`== "Total, all causes of death [A00-Y89]"]
  
  
  
  # [, value:=as.integer(value)][ , ave:=as.integer(ave)]
  
  convolution_window <- 3 
  
  dt.g <- dtExcess [GEO != "Yukon"
  ][, GEO:=ifelse(GEO %in% c("Nunavut", "Northwest Territories"), "Nunavut and NWT", GEO) 
  ][, .(value=sum(value, na.rm=T), ave=sum(ave, na.rm=T)), by = .(Date, GEO, Age)
    # ][, ':=' (value = frollmean(value, convolution_window, align = "right", fill = NA), 
    #           ave = frollmean(ave, convolution_window, align = "right", fill = NA)), by = .(GEO, Age)
  ][ Date > ymd("2018-01-02") ]
  
  str_age = "0 to 44 years"
  
  
  for (str_age in dtExcess$Age %>% unique()) {
    
    # dtExcess1 <- dtExcess [ Age == str_age] 
    
    g <-  dt.g [ Age == str_age] %>% 
      ggplot() +  
      theme(legend.position = "bottom") +
      geom_vline(xintercept = ymd("2020-01-01"), linetype=1, alpha=0.2) +
      geom_vline(xintercept = ymd("2021-01-01"), linetype=1, alpha=0.4) +
      geom_vline(xintercept = ymd("2021-05-01"), linetype=1, size=2, alpha=0.4) +
      geom_hline(yintercept = 0, col="grey") +
      
      # geom_line(aes(Date, value-ave, col = `Cause of death (ICD-10)`)) +
      geom_step(aes(Date, value),  col = "red", fill="red", linetype=1) +
      geom_step(aes(Date, ave),  col = "blue", fill="blue", linetype=1) +
      
      # facet_grid(GEO ~ ., scales = "free")
      facet_wrap(GEO ~ ., scales = "free") +
      # facet_wrap(GEO ~ `Cause of death (ICD-10)`, scales = "free")
      
      # guides(x =  guide_axis(angle = 90)) +
      # guides(col="none")  +
      # scale_x_date(guide = guide_axis(n.dodge = 2)) +
      # scale_x_discrete(guide = guide_axis(angle = 90)) +
      # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs( title = paste0("Mortality rate in Canada since the start of pandemic and vaccination"),
            subtitle=paste0("Age group: ", str_age), 
            x = "Blue line: pre-pandemic 2018-2019 average",
            # y = ifelse(F,  "Deaths per million per week", "Deaths per week"),
            y="Deaths per week", # 13-10-0768-01 (age); 13-10-0810-01 (cause)
            caption = "Source: Statistics Canada - Table 13-10-0768-01\n Generated by Open Canada Deaths Tracker App (ivim.ca/app)"     )  
    
    
    print(str_age)
    ggsave(paste0("1excess-death_", str_age, "_raw.png"), width = 15, height = 9)
    
  }
  
  ggplotly(g)
  
  # 
  # g <- ggplot(dtAll [GEO == "Canada"]) +  
  #   theme(legend.position = "bottom") +
  #   geom_col(aes(Date, value, col = `Cause of death (ICD-10)`)) +
  #   # guides(x =  guide_axis(angle = 90)) +
  #   # scale_x_date(guide = guide_axis(n.dodge = 2)) +
  #   # scale_x_discrete(guide = guide_axis(angle = 90)) +
  #   # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #   labs( title = NULL, x = NULL,  
  #         y = ifelse(F,  "Deaths per million per week", "Deaths per week"), 
  #         caption = "Source: Statistics Canada - Table 13-10-0810-01\nGenerated by Open Canada Deaths Tracker App (o-canada.shinyapps.io/vitals)"     ) +
  #   guides(col="none")  + 
  #   scale_x_date(guide = guide_axis(n.dodge = 2)) +
  #   facet_grid(GEO ~ `Cause of death (ICD-10)`, 
  #              scales = ifelse (input$keep_scale, "fixed", "free") ) 
  
  
  g
  
  
  ggplotly(g)
  
  
}