library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(visNetwork)
library(ggraph)
library(plotly)
library(datasets)
library(tidyverse)
library(tidymodels)
library(tidygraph)
library(igraph)

sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("upload", "Choose csv", accept = c(".csv")),
    
    menuItem("Description", tabName = "description", icon = icon("table")),
    
    menuItem("Social Network", tabName = "fig", icon = icon("table")),
    
    menuItem("Data", icon = icon("book"),
             menuSubItem("Nodes", tabName = "nodes"),
             menuSubItem("Edges", tabName = "edges")),
    #download
    menuItem("Download",icon = icon("fas fa-download"), downloadButton("download", "Download Network"))
  )
)

setup <- dashboardBody(
  tabItems(
    tabItem(tabName = "description",
            HTML("<body>
                  <h1>Entrenepenurial Marketing definition</h1>
                  <br>
                  <h4>
                  <b>Entrepreneurial Marketing</b> is “a combination of innovative, proactive, 
                  and risk-taking activities that create, 
                  communicate, and deliver value to and by customers, entrepreneurs, marketers, their 
                  partners, and society at large” <i>(Whalen et al. 2016)</i>.
                  </h4>
                  <br>
                  <h4>
                  <b>Entrepreneurial Networking</b> is defined as “what entrepreneurs do in creating and 
                  shaping network ties and may, therefore, include tie formation and maintenance 
                  behaviors as well as any assemblage of such behaviors into unique networking styles, 
                  strategies or processes” <i>(Bensaou et al. 2014)</i>.
                  </h4>
                </body>")),
    tabItem(tabName = "fig",
            fluidPage(br(),h3(textOutput("carga")),visNetworkOutput("graf", width = "100%", height = "500px"))
    ),
    tabItem(tabName = "nodes",
            DT::dataTableOutput("nodesTable")
    ),
    tabItem(tabName = "edges",
            DT::dataTableOutput("edgesTable")
    )
  )
  )


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Entrepreneurial Marketing",
                  titleWidth = "270px",
                  dropdownMenu(type = "notifications", icon = shiny::icon("code"),
                               badgeStatus = "info", headerText = "Desarrolladores",
                               tags$li(a(href = "https://github.com/srobledog",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Sebastian Robledo")),
                               tags$li(a(href = "https://github.com/bryanariasq02",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Bryan Arias")),
                               tags$li(a(href = "https://github.com/camilogs1",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Camilo García"))
                  )),
  sidebar,
  setup
)

server <- function(input, output) {
  
  exportar <- reactive({
    
    req(input$upload)
    
    df <- read.csv(input$upload$datapath, header = TRUE, sep = ",")
    
    graph_tbl <- 
      df |> 
      graph_from_data_frame(directed = FALSE) |> 
      as_tbl_graph() |> 
      convert(to_simple) |> 
      activate(nodes)
    
    return(graph_tbl)
  })
  
  getstatus <- reactive(
    if(is.null(input$upload)){
      "Please import file to view the information"
    }
  )
  
  output$carga <- renderText({
    getstatus()
  })
  
  grafos <- function(graph_tbl) {
    colores <- c(palette("R3"),palette("Dark2"),palette("alphabet"))
    nodes <- 
      graph_tbl |> 
      activate(nodes) |>
      mutate(community=as.numeric(group_louvain())) |>
      mutate(value = centrality_degree(),
             value = value *5) |> 
      mutate(id = row_number()) |>
      data.frame() |> 
      rename(label = name) |> 
      mutate(color = colores[community])
    
    edges_1 <- 
      graph_tbl |> 
      activate(nodes) |> 
      activate(edges) |> 
      data.frame() |> 
      rename(strength = 3) |> 
      select(from, to, strength)
    
    width = c()
    for(i in edges_1$strength){
      width <- append(width, length(i))
    }
    
    width = as.data.frame(width)
    edges <- cbind(edges_1, width)|> 
      mutate(width = width*5)
    
    return(list(nodes=nodes, edges=edges))
  }
  
  output$graf <- renderVisNetwork ({
    
    graph_tb <- exportar()
    
    grafo1 <- grafos(graph_tb)
    
    visNetwork(nodes = grafo1$nodes, 
               edges = grafo1$edges, 
               width = "100%") |>
      visLegend() |> 
      visExport() |>
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 nodesIdSelection = TRUE,
                 selectedBy = "community") |>
      visPhysics(solver ='forceAtlas2Based', 
                 stabilization = FALSE)
    
  })
  output$download <- downloadHandler(
    filename = function() {
      paste('network-','.html', sep='')
    },
    content = function(con) {
      graph_tb <- exportar()
      
      grafo1 <- grafos(graph_tb)
      
      visNetwork(nodes = grafo1$nodes, 
                 edges = grafo1$edges, 
                 width = "100%") |>
        visLegend() |> 
        visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                   nodesIdSelection = TRUE,
                   selectedBy = "community") |>
        visPhysics(solver ='forceAtlas2Based', 
                   stabilization = FALSE) |> visSave(con)
    }
    
  )
  
  output$nodesTable <-  DT::renderDataTable(server = FALSE,{
    
    graph_tb <- exportar() 
    
    grafo1 <- grafos(graph_tb)
    
    graph <- grafo1$nodes |>
      select("ID" = id,
             "Label" = label, 
             "Community" = community,
             "Degree" = value) |> 
      mutate(Degree = Degree/5) |> 
      DT::datatable(class = "cell-border stripe",
                                    rownames = F,
                                    filter = "top",
                                    editable = FALSE,
                                    extensions = c('Scroller','Buttons'),
                                    options = list(dom = "Bfrtip",
                                                   buttons = c("copy",
                                                               "csv",
                                                               "excel",
                                                               "pdf",
                                                               "print"),
                                                   scrollY = 420,
                                                   scroller = TRUE))
  })
  
  output$edgesTable <-  DT::renderDataTable(server = FALSE,{
    
    req(input$upload)
    
    df <- read.csv(input$upload$datapath, header = TRUE, sep = ",") |> 
      DT::datatable(class = "cell-border stripe",
                    rownames = F,
                    filter = "top",
                    editable = FALSE,
                    extensions = c('Scroller','Buttons'),
                    options = list(dom = "Bfrtip",
                                   buttons = c("copy",
                                               "csv",
                                               "excel",
                                               "pdf",
                                               "print"),
                                   scrollY = 420,
                                   scroller = TRUE))
  })
  
}

shinyApp(ui, server)