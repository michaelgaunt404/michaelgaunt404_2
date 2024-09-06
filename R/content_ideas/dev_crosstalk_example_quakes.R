

#blogpost_of_crosstalk_wwidgets
#making big blog with different shared data sizes

library(crosstalk)
library(leaflet)
library(DT)
library(sf)
library(dplyr)


quakes %>%
  gr

quakes_sf = quakes %>%
  mutate(across(c("long", "lat"), round, 1)) %>%
  group_by(long, lat) %>%
  summarise(count = n()
            ,mag_mean = mean(mag)
            ,mag_med = median(mag)
            ,mag_q95 = quantile(mag, .95)
            ,"mag01" = sum(between(mag, 0, 1))
            ,"mag12" = sum(between(mag, 1, 2))
            ,"mag23" = sum(between(mag, 2, 3))
            ,"mag34" = sum(between(mag, 3, 4))
            ,"mag45" = sum(between(mag, 4, 5))
            ,"mag56" = sum(between(mag, 5, 6))
  ) %>%
  ungroup() %>%
  filter(count > 1) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4236)
mapview::mapview(quakes_sf, cex = "count")

# Wrap data frame in SharedData
sd <- SharedData$new(quakes[sample(nrow(quakes), 100),])

# Create a filter input
filter_slider("mag", "Magnitude", sd, column=~mag, step=0.1, width=250)

# Use SharedData like a dataframe with Crosstalk-enabled widgets
bscols(
  leaflet(sd) %>%
    addTiles() %>%
    addMarkers()
  ,datatable(sd, extensions="Scroller", style="bootstrap"
             ,class="compact", width="100%"
             ,options=list(deferRender=TRUE
                           ,scrollY=300, scroller=TRUE))
)







#reference item=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(crosstalk)
library(d3scatter)
devtools::install_github("jcheng5/d3scatter")


row.names(mtcars) <- NULL
sd_mtcars_all <- SharedData$new(mtcars, group = "mtcars_subset")
sd_mtcars_auto <- SharedData$new(mtcars[mtcars$am == 0,], group = "mtcars_subset")
sd_mtcars_manual <- SharedData$new(mtcars[mtcars$am == 1,], group = "mtcars_subset")

bscols(widths = c(6, 6),
       list(
         d3scatter(sd_mtcars_all, ~hp, ~mpg, ~factor(cyl),
                   width = "100%", height = 200),
         reactable::reactable(sd_mtcars_all, filterable = T
                              , selection = "multiple", onClick = "select")
       ),
       list(
         d3scatter(sd_mtcars_auto, ~hp, ~mpg, ~factor(cyl),
                   width = "100%", height = 200),
         plotly::plot_ly(sd_mtcars_manual, x=~hp, y=~mpg, color = ~factor(cyl), height = 200) %>%
           plotly::highlight(dynamic = T, on = 'plotly_selected')
       )
)

row.names(mtcars) <- NULL
sd_mtcars_all <- SharedData$new(mtcars, group = "mtcars_subset")
sd_mtcars_auto <- SharedData$new(mtcars[mtcars$am == 0,], group = "mtcars_subset")
sd_mtcars_manual <- SharedData$new(mtcars[mtcars$am == 1,], group = "mtcars_subset")

bscols(widths = c(6, 6),
       list(
         d3scatter(sd_mtcars_all, ~hp, ~mpg, ~factor(cyl),
                   width = "100%", height = 200),
         reactable::reactable(sd_mtcars_all, filterable = T
                              , selection = "multiple", onClick = "select")
       ),
       list(
         d3scatter(sd_mtcars_auto, ~hp, ~mpg, ~factor(cyl),
                   width = "100%", height = 200),
         plotly::plot_ly(sd_mtcars_manual, x=~hp, y=~mpg, color = ~factor(cyl), height = 200) %>%
           plotly::highlight(dynamic = T, on = 'plotly_selected')
       )
)




temp_mtcars = mtcars %>%
  group_by(cyl) %>%
  mutate(hp_m = mean(hp) %>% round(2), mpg_m = mean(mpg) %>% round(2)
         ,count = 1) %>%
  ungroup()

temp_mtcars_agg = temp_mtcars %>%
  group_by(hp_m, mpg_m) %>%
  summarise(vs_1 = sum(vs == 1)
            ,gear_3 = sum(gear == 3)
            ,gear_4 = sum(gear == 4)
            ,disp_mean = sum(disp>mean(disp))) %>%
  pivot_longer(cols = !ends_with("_m"), values_to = "count") %>%
  ungroup() %>%
  mutate(hidden_key = paste0(hp_m, row_number()))

temp_mtcars_agg_pnts = temp_mtcars_agg %>%
  select(ends_with("_m")) %>%
  unique() %>%
  mutate(hidden_key = paste0(hp_m, row_number()))

temp_mtcars_sd <- SharedData$new(temp_mtcars, group = "mtcars_agg")
temp_mtcars_agg_sd <- SharedData$new(temp_mtcars_agg, group = "mtcars_agg", key = ~hidden_key)
temp_mtcars_agg_pnts_sd <- SharedData$new(temp_mtcars_agg_pnts, group = "mtcars_agg", key = ~hidden_key)
all(
  temp_mtcars_agg_sd$key() == temp_mtcars_agg_pnts_sd$key() )


bscols(widths = c(6, 6),

       reactable::reactable(temp_mtcars_agg_sd, filterable = T
                            , selection = "multiple", onClick = "select")
       ,plotly::plot_ly(temp_mtcars_agg_sd, x=~hp_m, y=~mpg_m, height = 400) %>%
         plotly::highlight(dynamic = F, on = 'plotly_selected')
)
)

state_info <- data.frame(stringsAsFactors = FALSE,
                         state.name,
                         state.region,
                         state.area
)
sd1 <- SharedData$new(state_info, ~state.name)
sd2 <- SharedData$new(state_info, state_info$state.name)
sd3 <- SharedData$new(state_info, function(data) data$state.name)

# Do all three forms give the same results?
all(sd1$key() == sd2$key() & sd2$key() == sd3$key())











