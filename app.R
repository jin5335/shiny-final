########################################################
# last modified on 28.10.2017
# dont forget - rename NA status
# changes
# 1) 전체적인 코드정리 (server reactive 파트에서 데이터 filter 중복은 아직 남아있음)
# 2) line plot 오류 해결
# 3) 나라선택시 다른나라데이터도 표시하면서 zoom만 옮겨지도록 수정
# 4) 데이터의 나라 갯수에 상관없도록 re-coding (12개 나라 그대로일 경우 # 붙여놓은 palette구문 살리면 진주가 뽑은 색깔 팔레트로 적용됨)
########################################################
#### Global part. ####
library(shiny)
library(leaflet)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(RColorBrewer)

# import database
dat_nearsea <- read_csv("data/DB_wide.csv") %>%  as.data.frame()

# get control panel input lists
countries <- unique(dat_nearsea$`Country (Country)`) %>% sort()
list_countries <- c("Whole", countries)
list_species <-unique(dat_nearsea$`Species (ASFIS species)`) %>% sort()

# country color palette
pal_all <- colorFactor(
   palette = "viridis",
   #palette = c("#FF0033","#FF3399","#FF9900","#CC9933","#66FFCC","#66FF66","#006600","#666600","#66CCFF","#003366","#000066","#9900FF"),
   domain = countries
)

# bubble information 
scale <- 30000
min <- 5

# country geology & zoom info
setView_cont <- dat_nearsea %>% select(c(`Country (Country)`,`lat`,`lng`)) %>% unique() %>% as.data.frame() %>% cbind(zoom=c(4))
setView_cont$zoom[setView_cont$`Country (Country)`=="Singapore"]=5
setView_cont <- setView_cont %>% rbind(c("Whole", 14, 108, 4))
setView_cont$lat <- as.numeric(setView_cont$lat)
setView_cont$lng <- as.numeric(setView_cont$lng)

#### UI part. ####
ui <- fluidPage(
  sidebarLayout(
    tabsetPanel(
      # tab 1
      tabPanel("Fish amount by year",
               leafletOutput("map_bubble", width = "60%", height = "800px"),
               absolutePanel(id="slide", 
                             fixed=F, draggable = T, top=30, left="5%", right="65%", bottom=30, height="40", width="30%",
                             tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: steelblue}")),
                             sliderInput("year","", min=1950, max=2015, value=2015, width="90%", sep="", 
                                         animate = animationOptions(interval = 300))),
               # database info
               tags$div(id="cite", ' Dataset info:', 'FAO. 2017. Fishery and Aquaculture Statistics.', 
                        tags$em('Global production by production source 1950-2015 (FishstatJ).'),
                        tags$br(),'In: FAO Fisheries and Aquaculture Department [online]. Rome. Updated 2017. www.fao.org/fishery/statistics/software/fishstatj/en')
      ),
      # tab 2
      tabPanel("Finance network",
               # set leaflet environment
               tags$head(tags$link(
                 href="//unpkg.com/leaflet@1.2.0/dist/leaflet.css",rel="stylesheet"),
                 integrity="sha512-M2wvCLH6DSRazYeZRIm1JnYyh22purTM+FDB5CsyxtQJYeKq83arPe5wgbNmcFXGqiSH2XR8dT/fJISVA1r/zQ==",
                 crossorigin=""
               ),
               tags$head(tags$script(
                 src="//unpkg.com/leaflet@1.2.0/dist/leaflet.js",
                 integrity="sha512-lInM/apFSqyy1o6s89K4iQUKg6ppXEgsVxT35HbzUupEVRh2Eu9Wdl4tHj7dZO0s1uvplcYGmt3498TtHq+log==",
                 crossorigin=""
               )),
               # set d3 environment 
               tags$head(tags$script(src="//d3js.org/d3.v4.min.js")),
               tags$head(tags$script(src="//unpkg.com/topojson@3")),
               # d3 flowmap script
               tags$head(tags$script(src="flowmap.js")),
               # svg UIs
               div(id="flowmap0", class="flowmap-output", style="height:100vh;width:60vw;")),
      # tab 3
      tabPanel("Culture-skill network",
               # svg UIs
               div(id="flowmap1", class="flowmap-output", style="height:100vh;width:60vw;"))
    ),
    # control panel
    absolutePanel(
      id="controls", fixed=F, draggable = F, top = 50, left = "auto", right = 0, bottom = 10,
      width = "35%", height = "auto",
      fluidRow(
        selectInput("fish",
                    label="Select a fish",
                    choices=list_species, width = "90%",
                    selected=1),
        selectInput("country",
                    label="Select a country", width = "90%",
                    choices=list_countries,
                    selected="Whole"),
        plotOutput("ggplot_area", width = "95%", height = "200px"),
        plotOutput("ggplot_country", width = "90%", height = "200px")
      )
    )
  )
)

#### Server part. ####
# render list for flowchart
renderList <- function(expr, env=parent.frame(), quoted=FALSE) {
  func <- exprToFunction(expr, env, quoted)
  function() {
    val <- func()
  }
}

# get export networks
getexportNDB <- function(name){
  test <- dat_nearsea %>% filter(`Species (ASFIS species)` == name)
  ## Find network
  test$delta <- ifelse(test$status %in% c("increase", "decrease"), test$grad, 
                       ifelse(test$status == "new", test$recent,
                              ifelse(test$status == "dangerous", test$recent - test$max, 0)))
  test_select <- test%>%
    select(`Species (ASFIS species)`, `Country (Country)`, status, delta, lng, lat)
  colnames(test_select) <- c("species", "country", "status", "delta", "lng", "lat")
  from_country <- from_lng <- from_lat <- 
    to_country <- to_lng <- to_lat <- width <- c()
  # 최대 계산비용 12 * 12 이후 나라가 더 많아지면 효율적인 계산 필요할수도
  for(i in 1:nrow(test_select)){
    if(test_select$status[i] %in% c("increase", "new")){
      for(j in 1:nrow(test_select)){
        if(test_select$status[j] %in% c("decrease", "dangerous")){
          from_country <- c(from_country, test_select$country[i])
          from_lng <- c(from_lng, test_select$lng[i])
          from_lat <- c(from_lat, test_select$lat[i])
          to_country <- c(to_country, test_select$country[j])
          to_lng <- c(to_lng, test_select$lng[j])
          to_lat <- c(to_lat, test_select$lat[j])
          width <- c(width, min(test_select$delta[i], abs(test_select$delta[j])))
        }
      }
    }
  }
  # network flow links
  flowdat_1 <- data.frame(from_country, from_lng, from_lat,
                          to_country, to_lng, to_lat,
                          width) %>% toJSON()
  return (flowdat_1)
}

# get culture networks
getcultureNDB <- function(name){
  test <- dat_nearsea %>% filter(`Species (ASFIS species)` == name)
  test_select <- test %>%
    select(`Species (ASFIS species)`, `Country (Country)`, status, max, lng, lat) #일단은 max값을 width로
  colnames(test_select) <- c("species", "country", "status", "delta", "lng", "lat")
  from_country <- from_lng <- from_lat <- 
    to_country <- to_lng <- to_lat <- width <- c()
  for(i in 1:nrow(test_select)){
    if(test_select$status[i] %in% c("new")){
      for(j in 1:nrow(test_select)){
        if(test_select$delta > 0 && i!=j){
          from_country <- c(from_country, test_select$country[j])
          from_lng <- c(from_lng, test_select$lng[j])
          from_lat <- c(from_lat, test_select$lat[j])
          to_country <- c(to_country, test_select$country[i])
          to_lng <- c(to_lng, test_select$lng[i])
          to_lat <- c(to_lat, test_select$lat[i])
          width <- c(width, test_select$delta[j])
        }
      }
    }
  }
  # network flow links
  flowdat_2 <- data.frame(from_country, from_lng, from_lat,
                          to_country, to_lng, to_lat,
                          width) %>% toJSON()
  return (flowdat_2)
}

# Server reactive functions
server <- function(input, output, session) {
  # tab1 - bubble chart
  output$map_bubble <- renderLeaflet({
    set <- setView_cont[setView_cont$`Country (Country)`==input$country,]
    leaflet() %>%
      addTiles(urlTemplate = "//cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>% 
      setView(lng=set$lng, lat=set$lat, zoom=set$zoom)
  })
  observe({
    input$country
    tmp <- dat_nearsea %>% 
      filter(`Species (ASFIS species)` == input$fish) %>% 
      gather("Year","Amount", "1950":"2015")  %>% 
      filter(Year == input$year)
    tmp$Amount[tmp$Amount < min] <- c(min)
    radius_dat <- log2(tmp$Amount)
    # draw bubble
    leafletProxy("map_bubble", data = tmp) %>%
      clearShapes() %>% 
      addCircles(~lng, ~lat, popup = paste(sep="<br/>", paste0("Country : ", tmp$`Country (Country)`), paste0("Amount : ", tmp$Amount), paste0("Status : <b>", tmp$status, "</b>")), radius =  radius_dat * scale, stroke = F, fillOpacity = 0.5, fillColor = pal_all(tmp$`Country (Country)`))
    q <- tmp
  })
  
  # control panel - area plot
  output$ggplot_area <- renderPlot({
    tmp <- dat_nearsea %>% 
      filter(`Species (ASFIS species)`==input$fish) %>%
      gather("Year","Amount", "1950":"2015")
    tmp$Year <- as.numeric(tmp$Year)
    tmp %>% ggplot(aes(x = Year, y = Amount, fill=`Country (Country)`)) +
      geom_area() + 
      scale_fill_manual(values = pal_all(tmp$`Country (Country)`))
  })
  
  # control panel - line plot
  output$ggplot_country <- renderPlot({
    if(input$country != "Whole"){
      tmp <- dat_nearsea %>% filter(`Country (Country)`==input$country)
      tmp <- tmp %>% filter(`Species (ASFIS species)`==input$fish) %>%
        gather("Year","Amount", "1950":"2015")
      tmp$Year <- as.numeric(tmp$Year)
      tmp %>% ggplot(aes(x = Year, y = Amount)) + geom_line() +
        ggtitle(paste0("Fish amount by year at ",input$country))
    }
  })

  # tab 2 - export network flow chart
  output$flowmap0 <- renderList({
    getexportNDB(input$fish)
  })
  # tab 3 - cultural network flow chart
  output$flowmap1 <- renderList({
    getcultureNDB(input$fish)
  })
}

shinyApp(ui, server)
