#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# DESC: Demo for different Leaflet popup methods
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: details methods for different popup types - plots/tables/maps/widgets
#-------- should be incoprorated into ablog at some point
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#sec: inputs====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_x = list(c("big", "small"), c("big", "small"))
plot_y = list(c(15, 4), c(7, 11))

df = read.csv(textConnection(
  "Name,Lat,Long
  Samurai Noodle,47.597131,-122.327298
  Another Place,47.687,-121.753
  Third Plce,47.344,-122.112"
))

#sec: popup_objects=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plots_ggplot = Map(function(x, y) {
  df = data.frame(x = x, y = y)
  ggplot(data=df, aes(x=x, y=y)) + geom_col()
}, x = plot_x, y = plot_y)

plots_plotly = Map(function(x, y) {
  df = data.frame(x = x, y = y)
  plot_ly(data=df, x=~x, y=~y, type="bar")
}, x = plot_x, y = plot_y)

plots_plotly_files = plots_plotly %>%
  map(~{fl = tempfile(fileext = ".html")
    saveWidget(.x, file = fl)
    return(fl)})

plots_xtalk = plots_reg %>% map(~{crosstalk::bscols(widths = 12, .x)})

iframe_link = "https://www.youtube.com/embed/iApz08Bh53w?autoplay=1"

map_widget = mapview() %>% .@map

html_tablesDt = df %>%
  group_by(Name_1 = Name) %>%
  group_map(~{DT::datatable(.x)})

html_tablesRctble = df %>%
  group_by(Name_1 = Name) %>%
  group_map(~{reactable::reactable(.x)})

#sec: inputs====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sec: Deploying via popup input================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
base_map = leaflet() %>% addTiles()


### plots====
base_map %>%
  addCircleMarkers(
    data = df, group = "pnts"
    ,popup = popupGraph(plots_ggplot, height = 200, width = 200))

base_map %>%
  addCircleMarkers(
    data = df, group = "pnts"
    ,popup = popupGraph(plots_plotly, height = 200, width = 200))

### maps====
base_map %>%
  addCircleMarkers(
    data = df, group = "pnts"
    ,popup = popupGraph(map_widget, height = 200, width = 200))

### iframe====
base_map %>%
  addCircleMarkers(
    data = df, group = "pnts"
    ,popup = leafpop:::popupIframe(iframe_link, height = 200, width = 200))

### html tables====
base_map %>%
  addCircleMarkers(
    data = df, group = "pnts"
    ,popup = popupTable(html_tables))

base_map %>%
  addCircleMarkers(
    data = df, group = "pnts"
    ,popup = popupGraph(html_tables, type = "html"))

base_map %>%
  addCircleMarkers(
    data = df, group = "pnts"
    ,popup = popupGraph(html_tablesRctble, type = "html"
                        ,width = 400, height = 150))

##sec: Deploying via addPopup%% functions=======================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
base_map = leaflet(df) %>% addTiles() %>%
  addMarkers(~Long, ~Lat, group="pnts")

base_map %>%
  addPopupGraphs(
    plots_ggplot, group = "pnts"
    ,height = 200, width = 200)

#this will not work because they are plotly
base_map %>% addPopupGraphs(plots_plotly, group = "pnts")
base_map %>% addPopupGraphs(plots_plotly, group = "pnts", type = "html")

#this will work, we are piping the temp files of the saved plotly plots
base_map %>%
  leafpop:::addPopupIframes(
    source = plots_plotly_files, group = "pnts"
    ,height = 200, width = 200)





fl = lapply(
  p
  , function(j) {
    fl = tempfile(fileext = ".html")
    saveWidget(j, file = fl)
    save_html()
    return(fl)
  }
)

file_1 = "C:/Users/USMG687637/OneDrive - WSP O365/Desktop/xxx.html"
file_1ex = "C:/Users/USMG687637/OneDrive - WSP O365/Desktop/xxx_export.html"

xxx_export
file_1 = "C:/Users/USMG687637/OneDrive - WSP O365/Desktop/qqqqq.html"

qqqqq

fl = append(fl, "https://www.youtube.com/embed/iApz08Bh53w?autoplay=1")
fl = append(list(file_1, file_1), "https://www.youtube.com/embed/iApz08Bh53w?autoplay=1")
fl = list(file_1, file_1ex, "https://www.youtube.com/embed/iApz08Bh53w?autoplay=1")





%>%
  leafpop:::addPopupIframes(
    source = fl
    , group = "3"
  )

m
tab

sd <- SharedData$new(quakes[sample(nrow(quakes), 100),])

# Create a filter input
filter_slider("mag", "Magnitude", sd, column=~mag, step=0.1, width=250)

# Use SharedData like a dataframe with Crosstalk-enabled widgets
yolo = bscols(
  leaflet(sd) %>% addTiles() %>% addMarkers(),
  datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)


yolo_1 = htmlwidgets::prependContent(yolo)


saveWidget(yolo_1
          ,file = "C:/Users/USMG687637/OneDrive - WSP O365/Desktop/qqqqq_1.html"
)



= navset_pill(
  nav_panel(title = "One", p("First tab content.")),
  nav_panel(title = "Two", p("Second tab content.")),
  nav_panel(title = "Three", p("Third tab content"))
)




#needed for crosstalk or bslibs stuff ======
shared_iris <- SharedData$new(iris)
fig <- bscols(
  plot_ly(shared_iris, x = ~Petal.Length, y = ~Petal.Width, colors = ~Species),
  plot_ly(shared_iris, x = ~Sepal.Length, y = ~Sepal.Width, colors = ~Species)
)
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = TRUE) # doesn't work
tempPlotly <- tempfile(pattern = "plotly", fileext = ".html")
htmltools::save_html(fig, tempPlotly)
rmarkdown::pandoc_self_contained_html(input = tempPlotly, output = tempPlotly)
utils::browseURL(tempPlotly)







v
