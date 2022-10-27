library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)
library(suncalc)
library(RCurl)
library(magrittr)

shinyServer(function(input, output, session)
  
{
  
  source("graphVars.R", local = TRUE)
  
  
  FileUpdate                  <- reactiveFileReader(60000, session, "ftp://haliburton%40cgts.ca:cry0sphere!@ftp.cgts.ca/hali_data/LogData_All.dat", read.csv)
  
  
  # FileUpdate                      <- reactivePoll(60000, session,
  #                                                 
  #                                         checkFunc = function() {
  #                                         DestFile                <- "LogDataAll.csv"
  #                                         url                     <- "ftp://haliburton%40cgts.ca:cry0sphere!@ftp.cgts.ca/hali_data/LogData_All.dat"
  #                                         download.file (url, DestFile, mode = "wb", method = "curl", extra = "--remote-time", quiet = T)
  #                                         
  #                                         
  #                                       if (file.exists(DestFile))    {
  #                                                                     file.info(DestFile)$mtime[1]
  #                                                                     } else {
  #                                                                     ""
  #                                                                     }
  #                                       },
  #                                       
  #                                       valueFunc = function() {
  #                                       
  #                                         
  #                                       read.csv(DestFile, skip = 4, na.strings = c("NAN", "-6999", "6999", "Inf", "-Inf", "NULL"))
  #                                       
  #                                       })
  #     
  # Filter data to everything since the new battery install. Pick only batt_voltage and date. Provide minimum voltage per day
          reactive ({
          hali_data                   <- FileUpdate()
      
          colnames(hali_data)         <- c( "TIMESTAMP", "RECORD", "batt_volt_Avg", "short_up_Avg", "short_dn_Avg", "long_up_Avg", "long_dn_Avg", "CNR4TC_Avg", "CNR4TK_Avg", "long_up_corr_Avg", "long_down_corr_Avg", "Air_temp_Avg", "RH", "VP_Avg", "Wind_S_Avg", "Wind_Dir_Avg", "WS_ms_S_WVT", "WindDir_D1_WVT", "WindDir_SD1_WVT", "Air_Press", "snow_depth", "sig_Qual", "rain_mm_Tot", "T109_Avg(1)", "T109_Avg(2)", "T109_Avg(3)", "T109_Avg(4)", "T109_Avg(5)", "T109_Avg(6)" )
          hali_data                   <- as_tibble(hali_data)
          hali_data$TIMESTAMP         <- ymd_hms(hali_data$TIMESTAMP, tz = "EST")
          hali_data                   <- hali_data %>% filter(TIMESTAMP > "2019-01-01")
          })
          reactive ({
          MinBattVolts                <- 
            hali_data                   %>%           filter(TIMESTAMP > "2019-09-27") %>%
            dplyr::select(TIMESTAMP, batt_volt_Avg) %>%
            dplyr::mutate(day = date(TIMESTAMP)) %>%
            dplyr::group_by(day) %>%
            dplyr::summarise(minimum_batt_v = min(batt_volt_Avg), sd_batt_v = sd(batt_volt_Avg))
          })
          
          latest_AirTemp_Avg          <- tail(hali_data$Air_temp_Avg, 1)
          latest_RH_Avg               <- tail(hali_data$RH, 1)
          latest_TimeStamp            <- tail(hali_data$TIMESTAMP, 1)
          fill_bars_current_temp      <- ifelse(latest_AirTemp_Avg >= 0, fill_bars_pos, fill_bars_neg)
          
          AirTemp_minaxis             <- min(hali_data$Air_temp_Avg) - 1
          AirTemp_maxaxis             <- max(hali_data$Air_temp_Avg) + 1
    
    
    
    
    
  
  
  
  # Used for determining the axis extents for both current and historical data. Takes min/max and expands it by one degree to add some buffer to the graph for display.
  
  
  # Pulls the latest value from the hali_data
  
  
  
  
  output$RadiationPlot <- renderPlotly({
    
    
    # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
    x_begin                 <- input$time_slider[1]
    x_end                   <- input$time_slider[2]
    
    
    
    plot_ly(hali_data) %>%
      
      add_trace(
        x = ~ TIMESTAMP,
        y = ~ short_up_Avg,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        name = 'Incoming Shortwave',
        fillcolor = fill_sw_in,
        line = list(color = line_fill,
                    width = line_width),
        hoverlabel = list(bgcolor = fill_sw_in),
        hoverinfo = 'x+y'
      ) %>%
      
      
      add_trace(
        x = ~ TIMESTAMP,
        y = ~ short_dn_Avg,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        name = 'Outgoing Shortwave',
        fillcolor = fill_sw_out,
        line = list(color = line_fill,
                    width = line_width),
        hoverlabel = list(bgcolor = fill_sw_out),
        hoverinfo = 'x+y'
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Incoming and Outgoing Shortwave Radiation", x = 0.1),
        
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        
        font = title_font_style,
        
        legend = default_legend,
        
        hovermode = 'x',
        
        margin = margins,
        
        xaxis = list(
          title = "Date",
          titlefont = axis_title_fontstyle,
          range = list(x_begin, x_end),
          type = 'date',
          ticklen = 8,
          showgrid = TRUE,
          showline = TRUE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
        ),
        
        yaxis2 = list(
          range = c(0.0, 100.0),
          title = list(text = "Albedo (%)"),
          titlefont = axis_title_fontstyle,
          tickfont = list(size = tick_fontsize),
          ticks = "outside",
          side = 'right',
          showgrid = FALSE,
          zeroline = FALSE,
          linecolor = axis_line_color
        ),
        
        yaxis = list(
          title = list(text = "Solar Radiation (W m<sup>2</sup>)"),
          titlefont = axis_title_fontstyle,
          overlaying = 'y2',
          ticks = "outside",
          range = c(0, 1300),
          showgrid = FALSE,
          zeroline = FALSE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
        )
      ) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  
  
  output$PrecipitationPlot <- renderPlotly({
    # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
    x_begin                 <- input$time_slider[1]
    x_end                   <- input$time_slider[2]
    
    plot_ly(hali_data) %>%
      
      add_trace(
        x = ~ TIMESTAMP - hours(12),
        y = ~ rain_mm_Tot,
        name = 'Precipitation',
        type = 'bar',
        yaxis = 'y',
        marker = list(
          color = fill_bars,
          line = list(color = line_fill,
                      width = line_width)
        ),
        hoverlabel = list(bgcolor = fill_bars),
        hoverinfo = 'y'
      ) %>%
      
      
      layout(
        title = "Daily Total Precipitation",
        
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        font = title_font_style,
        
        legend = default_legend,
        
        hovermode = 'x',
        
        margin = margins,
        
        
        xaxis = list(
          title = "Date",
          titlefont = axis_title_fontstyle,
          range = list(x_begin, x_end),
          type = 'date',
          ticklen = 8,
          showgrid = TRUE,
          showline = TRUE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
        ),
        
        
        
        yaxis = list(
          title = list(text = "Precipitation (mm)"),
          titlefont = axis_title_fontstyle,
          ticks = "outside",
          
          showgrid = FALSE,
          zeroline = FALSE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
        )
      ) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  
  output$PrecipitationCumulativePlot <- renderPlotly({
    
    
    # Precipitation, cumulative time series - this might not be the best way to construct this...
    Precip_mm_TimeSeries_x      <- c(tail(hali_data$TIMESTAMP, 1) - hours(1), tail(hali_data$TIMESTAMP, 1) - hours(2), tail(hali_data$TIMESTAMP, 1) - hours(4), tail(hali_data$TIMESTAMP, 1) - hours(8), tail(hali_data$TIMESTAMP, 1) - hours(16), tail(hali_data$TIMESTAMP, 1) - hours(24), tail(hali_data$TIMESTAMP, 1) - hours(48), tail(hali_data$TIMESTAMP, 1) - days(3), tail(hali_data$TIMESTAMP, 1) - days(4), tail(hali_data$TIMESTAMP, 1) - days(5), tail(hali_data$TIMESTAMP, 1) - days(6), tail(hali_data$TIMESTAMP, 1) - days(7) )
    Precip_mm_TimeSeries_y      <- c(
      sum(head(hali_data$rain_mm_Tot, 1)),  # Past hour
      sum(head(hali_data$rain_mm_Tot, 2)),  # Past two hours
      sum(head(hali_data$rain_mm_Tot, 4)),  # 4h
      sum(head(hali_data$rain_mm_Tot, 8)),  # 8h
      sum(head(hali_data$rain_mm_Tot, 12)), # 12h
      sum(head(hali_data$rain_mm_Tot, 24)), # 24h
      sum(head(hali_data$rain_mm_Tot, 48)), # 48h, 2 days
      sum(head(hali_data$rain_mm_Tot, 72)), # 72h, 3 days
      sum(head(hali_data$rain_mm_Tot, 4 * 24)),   # 4 days
      sum(head(hali_data$rain_mm_Tot, 5 * 24)),   # 5 days
      sum(head(hali_data$rain_mm_Tot, 6 * 24)),   # 6 days
      sum(head(hali_data$rain_mm_Tot, 7 * 24)) # 7 days
    )
    
    
    plot_ly() %>%
      
      add_trace(
        x = Precip_mm_TimeSeries_x,
        y = Precip_mm_TimeSeries_y,
        name = 'Precipitation',
        type = 'bar',
        yaxis = 'y',
        marker = list(
          color = fill_bars,
          line = list(color = line_fill,
                      width = line_width)
        ),
        hoverlabel = list(bgcolor = fill_bars),
        hoverinfo = 'y'
      ) %>%
      
      
      layout(
        title = "Cumulative Precipitation",
        
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        font = title_font_style,
        legend = default_legend,
        hovermode = 'x',
        margin = margins,
        
        xaxis = list(
          title = "Date",
          titlefont = axis_title_fontstyle,
          type = 'date',
          ticklen = 8,
          tickvals = c('30m', '1h'),
          ticktext = c('poo', 'poo2'),
          showgrid = FALSE,
          showline = TRUE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
        ),
        
        
        
        yaxis = list(
          title = "Precipitation (mm)",
          titlefont = axis_title_fontstyle,
          showgrid = FALSE,
          zeroline = FALSE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
        )
      ) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  
  output$AirTempRHPlot <- renderPlotly({
    # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
    x_begin                 <- input$time_slider[1]
    x_end                   <- input$time_slider[2]
    
    AirTemp_minaxis             <- min(hali_data$Air_temp_Avg) - 1
    AirTemp_maxaxis             <- max(hali_data$Air_temp_Avg) + 1
    
    plot_ly(hali_data) %>%
      
      add_trace(
        x = ~ TIMESTAMP,
        y = ~ RH,
        type = 'scatter',
        mode = 'lines',
        name = 'RH',
        yaxis = 'y2',
        line = line_rh,
        hoverlabel = list(bgcolor = fill_rh),
        hoverinfo = 'y'
      ) %>%
      
      add_trace(
        x = ~ TIMESTAMP,
        y = ~ Air_temp_Avg,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        name = 'Temperature     ',
        # Added 5 spaces to fix legend cutoff
        connectgaps = FALSE,
        fillcolor = fill_temp,
        line = list(color = line_fill,
                    width = line_width),
        hoverlabel = list(bgcolor = fill_temp),
        hoverinfo = 'y'
      ) %>%
      
      layout(
        title = "Air Temperature and Humidity",
        
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        
        font = title_font_style,
        
        legend = default_legend,
        
        hovermode = 'x',
        
        margin = margins,
        
        xaxis = list(
          title = "Date",
          range = list(x_begin, x_end),
          type = 'date',
          ticklen = 8,
          showgrid = TRUE,
          showline = TRUE,
          linecolor = axis_line_color,
          titlefont = axis_title_fontstyle,
          tickfont = list(size = tick_fontsize)
        ),
        
        yaxis = list(
          title = 'Air Temperature (&deg;C)',
          titlefont = axis_title_fontstyle,
          tickfont = list(size = tick_fontsize),
          showgrid = FALSE,
          overlaying = 'y2',
          linecolor = axis_line_color,
          ticks = "outside",
          range = c(AirTemp_minaxis, AirTemp_maxaxis)
        ),
        
        yaxis2 = list(
          title = "RH (%)",
          titlefont = axis_title_fontstyle,
          side = 'right',
          range = c(0, 105),
          linecolor = axis_line_color,
          ticks = "outside",
          showgrid = FALSE,
          tickfont = list(size = tick_fontsize)
        )
      ) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  
  output$BatteryPlot <- renderPlotly({
    # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
    
    x_begin                 <- input$time_slider[1]
    x_end                   <- input$time_slider[2]
    
    plot_ly(hali_data) %>%
      
      add_trace(
        x = ~ TIMESTAMP,
        y = ~ batt_volt_Avg,
        type = 'scatter',
        mode = 'lines',
        name = 'Battery Voltage',
        yaxis = 'y2',
        line = line_batt,
        hoverlabel = list(bgcolor = fill_rh),
        hoverinfo = 'y'
      ) %>%
      
      add_trace(
        x = MinBattVolts$day,
        y = MinBattVolts$minimum_batt_v,
        type = 'scatter',
        mode = 'lines',
        name = 'Battery Voltage',
        yaxis = 'y2',
        line = line_batt,
        hoverlabel = list(bgcolor = fill_rh),
        hoverinfo = 'y'
      ) %>%
      
      add_trace(
        x = ~ TIMESTAMP,
        y = ~ short_up_Avg,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        name = 'Short Wave In     ',
        # Added 5 spaces to fix legend cutoff
        connectgaps = FALSE,
        fillcolor = fill_sw_in,
        line = list(color = line_fill,
                    width = line_width),
        hoverlabel = list(bgcolor = fill_temp),
        hoverinfo = 'y'
      ) %>%
      
      
      layout(
        title = "Battery Voltage and Radiation",
        
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        
        font = title_font_style,
        
        legend = default_legend,
        
        hovermode = 'x',
        
        margin = margins,
        
        xaxis = list(
          title = "Date",
          range = list(x_begin, x_end),
          type = 'date',
          ticklen = 8,
          showgrid = TRUE,
          showline = TRUE,
          linecolor = axis_line_color,
          titlefont = axis_title_fontstyle,
          tickfont = list(size = tick_fontsize)
        ),
        
        yaxis = list(
          title = 'ShortWave Rad',
          titlefont = axis_title_fontstyle,
          tickfont = list(size = tick_fontsize),
          showgrid = FALSE,
          overlaying = 'y2',
          linecolor = axis_line_color,
          ticks = "outside",
          range = c(0, 1200)
        ),
        
        yaxis2 = list(
          title = "Battery Voltage",
          titlefont = axis_title_fontstyle,
          side = 'right',
          range = c(12, 14),
          linecolor = axis_line_color,
          ticks = "outside",
          showgrid = FALSE,
          tickfont = list(size = tick_fontsize)
        )
      ) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  
  
  output$CurrentTempBar <- renderPlotly ({
    

    
    plot_ly(hali_data) %>%
      add_trace(
        type = "bar",
        width = bars_width_curcond,
        x = paste0(
          "As of<br>",
          hour(latest_TimeStamp),
          ":",
          ifelse(
            nchar(minute(latest_TimeStamp)) == 1,
            paste0("0", minute(latest_TimeStamp)),
            minute(latest_TimeStamp)
          ),
          ifelse(am(latest_TimeStamp), "am (EST)", "pm (EST)")
        ),
        
        y = latest_AirTemp_Avg,
        textposition = "auto",
        text = paste0(round(latest_AirTemp_Avg, 2), "&deg;C"),
        marker = list(
          color = fill_bars_current_temp,
          line = list(color = line_fill,
                      width = line_width)
        )
      ) %>%
      
      layout(
        title = "Current <br> Temperature",
        
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        font = title_font_style,
        
        margin = current_conditions_margins,
        
        yaxis = list(
          title = "&deg;C",
          titlefont = axis_title_fontstyle,
          range = c(AirTemp_minaxis, AirTemp_maxaxis),
          side = "left",
          showgrid = TRUE,
          zeroline = TRUE,
          showline = TRUE,
          linecolor = axis_line_color,
          ticks = "outside",
          tickfont = list(size = tick_fontsize)
        ),
        
        xaxis = list(
          showline = TRUE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
        )
        
      ) %>%
      
      config(displayModeBar = FALSE)
    
    
  })
  
  output$CurrentRHBar <- renderPlotly ({
    
    latest_TimeStamp            <- tail(hali_data$TIMESTAMP, 1)
    latest_RH_Avg               <- tail(hali_data$RH, 1)
    
    
    plot_ly(hali_data) %>%
      add_trace(
        type = "bar",
        width = bars_width_curcond,
        x = paste0(
          "As of<br>",
          hour(latest_TimeStamp),
          ":",
          ifelse(
            nchar(minute(latest_TimeStamp)) == 1,
            paste0("0", minute(latest_TimeStamp)),
            minute(latest_TimeStamp)
          ),
          ifelse(am(latest_TimeStamp), "am (EST)", "pm (EST)")
        ),
        
        y = latest_RH_Avg,
        textposition = "auto",
        text = paste0(round(latest_RH_Avg, 1), "%"),
        
        marker = list(
          color = fill_bars,
          line = list(color = line_fill,
                      width = line_width)
        )
      ) %>%
      
      layout(
        title = "Current <br> Humidity",
        
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        font = title_font_style,
        
        margin = current_conditions_margins,
        
        yaxis = list(
          title = "%",
          titlefont = axis_title_fontstyle,
          range = c(0, 105),
          side = "left",
          showgrid = TRUE,
          zeroline = TRUE,
          showline = TRUE,
          linecolor = axis_line_color,
          ticks = "outside",
          tickfont = list(size = tick_fontsize)
        ),
        
        xaxis = list(
          showline = TRUE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
        )
        
      ) %>%
      
      config(displayModeBar = FALSE)
    
    
  })
  
  
  
  
  
  
  
  
  output$CurrentNetRad <- renderPlotly ({  
    
    CurrentNetRadValue      <-    round((tail(hali_data$short_up_Avg, 1) - tail(hali_data$short_dn_Avg, 1) ) + (tail(hali_data$long_up_Avg, 1) - tail(hali_data$long_dn_Avg, 1)), 1)
    
    plot_ly() %>%
      add_trace(
        type = "bar",
        width = 10,
        orientation = 'h',
        name = "Shortwave",
        x = CurrentNetRadValue,
        y = '',
        hoverinfo = "none",
        marker = list(
          color = c('rgba(198, 21, 21, 1)'),
          line = list(color = line_fill,
                      width = line_width)
        )
      ) %>%
      
      
      layout(
        title = "Current Net Radiation",
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        font = title_font_style,
        margin = current_conditions_margins,
        
        xaxis = list(
          title = "W m<sup>2</sup>",
          titlefont = axis_title_fontstyle,
          side = "left",
          showgrid = TRUE,
          zeroline = TRUE,
          showline = TRUE,
          range = c(0, 1000),
          linecolor = axis_line_color,
          ticks = "outside",
          tickfont = list(size = tick_fontsize)
        ),
        
        yaxis = list(
          showline = TRUE,
          linecolor = axis_line_color,
          tickfont = list(size = tick_fontsize)
          
        )
        
      ) %>%
      
      add_annotations(
        text = paste0(CurrentNetRadValue, ' W m<sup>2</sup>'),
        font = list(color = 'grey35',
                    size = 12),
        xref = 'x',
        yref = 'y',
        x = (CurrentNetRadValue + 20),
        yshift = -5,
        xanchor = 'left',
        showarrow = FALSE
      ) %>%
      
      config(displayModeBar = FALSE)
    
    
  })
  
  
  output$CurrentRadBar <- renderPlotly ({
    
    latest_TotalRad_y           <- c( tail(hali_data$short_up_Avg, n = 1), tail(hali_data$short_dn_Avg, n = 1), tail(hali_data$long_up_corr_Avg, n = 1), tail(hali_data$long_down_corr_Avg, n = 1) )
    latest_TotalRad_x           <- c("SW Incoming/Upfacing", "Shortwave Outgoing", "Longwave Incoming", "Longwave Outgoing")
    
    
    plot_ly() %>%
      add_trace(
        type = "bar",
        width = bars_width_curcond,
        
        name = "Shortwave",
        
        x = latest_TotalRad_x,
        y = latest_TotalRad_y,
        #latest_SR01,
        textposition = "auto",
        hoverinfo = "none",
        text = paste0(round(latest_TotalRad_y, 1), "W m<sup>2</sup>"),
        
        marker = list(
          color = c(fill_sw_in, fill_sw_out, fill_lw_in, fill_lw_out),
          line = list(color = line_fill,
                      width = line_width)
        )
      ) %>%
      
      
      
      
      layout(
        title = "Current Solar <br> Radiation Components",
        
        paper_bgcolor = paper_bg_global,
        plot_bgcolor = plot_bg_global,
        font = title_font_style,
        margin = current_conditions_margins,
        
        yaxis = list(
          title = "W m<sup>2</sup>",
          titlefont = axis_title_fontstyle,
          side = "left",
          showgrid = TRUE,
          zeroline = TRUE,
          showline = TRUE,
          linecolor = axis_line_color,
          ticks = "outside",
          tickfont = list(size = tick_fontsize)
        ),
        
        xaxis = list(
          showline = TRUE,
          linecolor = axis_line_color,
          categoryarray = c("a", "b", "c", "d"),
          #Stupid hack to reorder the bars
          categoryorder = "array",
          tickfont = list(size = tick_fontsize)
          
        )
        
      ) %>%
      
      config(displayModeBar = FALSE)
  })
})
