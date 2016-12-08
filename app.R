require(shiny)
require(leaflet)
require(data.table)
require(shinydashboard)
load('./data/data.rdata')

#### SERVER ####
server <- function(input, output){ 

  # Variables to populate 2nd dropdown
  var_in <- reactive({
    if(input$lat_long == ''){data_attributes$school}else
    {data_attributes[school_to_city == input$lat_long]$school}
  })
  
  # Create output based on input to send back to output framework
  output$dynamic_dropdown <- renderUI({
    selectInput(
      'lat_long_school', 
      h4('Find a School'), 
      c(Choose ='', sort(var_in())),
      selected = '',
      selectize=TRUE)})
  
  # Managing Data
  data <- reactive({
    if(length(input$filter) == 0 | length(input$grades) == 0){
      data_attributes[school_type == 'All']
    }else{
      data_attributes[school_type %in% input$filter][grade_2016 %in% c('Blank',input$grades),]
    }
  })

  
  # Lat / Long view set
  lat_long_input <- reactive({
    
    x <- input$lat_long_school
    if(
      input$lat_long == '' & input$lat_long_school == ''
      ){
      data.table(
        lat = 30.984298,
        lon = -91.962333)
      }else if(
        x == ''
        ){
        city_attributes[
          city == input$lat_long,
          .(lat, lon)]
        }else if(
          x != ''          
          ){
          data_attributes[
            school == x,
            .(lat, lon)]
        }
    })
  
  # Color Constraints
  pal <- colorFactor(
    c('#17A768', '#88DE63','#F1AD1D','#F1601D','#E81725'),
    domain = c('A','B', 'C', 'D', 'F'))
  
  # Map 
  output$myMap <- renderLeaflet(
    leaflet(
      data = data()) %>% 
      addPolygons(
        data = state_poly,
        fillColor = 'transparent',
        color = 'black',
        weight = 3,
        dashArray = '5,5',
        opacity = 1
        
        ) %>%
      setView(
        lng = lat_long_input()$lon,
        lat = lat_long_input()$lat, 
        zoom = ifelse(input$lat_long != '' & input$lat_long_school == '', 10,
                      ifelse(
                        (input$lat_long != '' & input$lat_long_school != '') | (input$lat_long == '' & input$lat_long_school != ''), 14, 7))) %>% 
    addCircleMarkers(
        color = ~pal(grade_2016),
        stroke = F,
        popup = ~paste(
          '<b>',school,'</b><br><br>',
          '<center><b>2015-2016</b><br></center>',
          '<b> Grade</b>',grade_2016,'<br>',
          '<b>Score</b>',score_2016,'<br>',
          '<center><b>2014-2015</b><br></center>',
          '<b>Grade</b>',grade_2015,'<br>',
          '<b>Score</b>',score_2015,'<br>',
          '<center><a href="', pdf, '"  target="_blank">Detailed Report Card</a></center>'
        ),
        fillOpacity = ifelse(
          length(input$filter)  != 0 & length(input$grades) != 0 ,.7,0),
        radius = 9
        ) %>% 
      {
          if(
            (length(input$filter) !=  0 & length(input$grades) != 0)
            )
            addLegend(., 
                      pal = pal,
                      values = ~grade_2016,
                      labels = NULL,
                      opacity = .9,
                      title = NULL)
          else
            .
          } %>% 
      addProviderTiles("CartoDB.Positron"))
    

  
}

#### UI ####
ui <-
  dashboardPage(
    dashboardHeader(
      title = h3('Louisiana 2016 School Performance Scores'),titleWidth = '500px',
      tags$li(class = "dropdown",
              div( id = 'logo',
                tags$a(
                  img(src="tembo-logo.png",
                      height = 35 
                      ),
                  href="http://temboinc.com/", target="_blank")
                )
              ),
      dropdownMenuOutput('messageMenu')
      ), 
    dashboardSidebar( 
      div( id = 'sidebar',
      selectInput(
        'lat_long', 
        h4('Find a City'), 
        c(Choose ='', sort(city_attributes$city)),
        selectize=TRUE,
        selected = ''),
      uiOutput("dynamic_dropdown"),
      checkboxGroupInput(
        inputId = 'filter',
        label = h4('Select School Types'),
        choices = unique_filters_choices ,
        selected = unique_filters_choices),
      checkboxGroupInput(
        inputId = 'grades',
        label = h4('Select Grades'),
        choices = unique_grade_choices ,
        selected = unique_grade_choices)
     )),
    
    dashboardBody(
      # READ CSS
      tags$head(
        includeCSS("styles_v2.css")),
      # REMOVE ERROR MSGS
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      # MAP HERE
      leafletOutput('myMap', width = '100%', height = '800px'),
      # Floating Panel
      absolutePanel(
      id = 'splash',
      class = "panel panel-default",
      fixed = TRUE,
      draggable = F,
      top = '100',
      left = "auto",
      right = '100', 
      bottom = "auto",
      width = '330', 
      height = "auto",
      style = "z-index: 1000",
      
      fluidRow(
        h4('Overview:',align = 'center')),
      h5('This tool helps users navigate the 2015-2016 Louisiana School Performance Scores across the state. Utilize the controls to the left to focus on a city, then schools within that city. Click on a school to access a PDF of its 2015-2016 school performance score.'),
      HTML('<div class="modal-footer">
        <button type="button" class="btn btn-default btn-sm" data-target="#splash" data-dismiss="alert">Close</button>
           </div>')
      
      
      )
    )
  )
    
    
    
  
               
      
    
  
        
          

shinyApp(ui = ui, server = server)




