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

sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("upload", "Choose csv", accept = c(".csv")),
    menuItem("Figura", tabName = "fig", icon = icon("table")),
    
    menuItem("Tabla", icon = icon("book"), tabName = ("tabla")),
    #download
    menuItem("Descargar",icon = icon("fas fa-download"), downloadButton("download", "Download full results"))
  )
)

setup <- dashboardBody(
  tabItems(
    tabItem(tabName = "fig",
            fluidPage(visNetworkOutput("graf"))
    ),
    tabItem(tabName = "tabla",
            DT::dataTableOutput("contents")
    )
  )
)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Red social",
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
      expand(from = Source, to = Target) |>
      filter(from != to) |>
      ungroup() |>
      graph_from_data_frame(directed = FALSE) |>
      as_tbl_graph() |>
      convert(to_simple)
    
    return(graph_tbl)
  })
  
  grafos <- function(graph_tbl) {
    nodes <- 
      graph_tbl |> 
      activate(nodes) |>
      # modularidad
      mutate(community=as.character(group_louvain())) |>
      #grado
      mutate(degree = centrality_degree()) |>
      data.frame() |> 
      mutate(community = as.numeric(community) + 16) |> 
      select(name, community, degree) 
    
    edges <- 
      graph_tbl |> 
      activate(edges) |> 
      data.frame() |> 
      select(from, to)
    
    return(list(nodes=nodes, edges=edges))
  }
  
  output$graf <- renderVisNetwork ({
    
    graph_tb <- exportar()
    
    grafo1 <- grafos(graph_tb)
    
    
    visNetwork(nodes = grafo1$nodes, 
               edges = grafo1$edges, 
               height = "500px")  |> 
      visExport() |> 
      visEdges(arrows = "to") |>
      visLegend() |> 
      visOptions(manipulation = list(enabled = TRUE, 
                                     editEdgeCols = c("label"), 
                                     editNodeCols = c("group"), 
                                     addNodeCols = c("id", "label"))) |> 
      visNodes(color = grafo1$nodes$community, size = grafo1$edges$degree)
  })
  
  output$contents <-  DT::renderDataTable(server = FALSE,{
    
    graph_tb <- exportar() 
    
    graph <- graph_tb |>
      activate(nodes) |>
      mutate(community=as.character(group_louvain())) |>
      #grado
      mutate(degree = centrality_degree()) |>
      data.frame() |>
      select("Nombre" = name,
             "Grado" = degree,
             "Comunidad" = community) |>
      DT::datatable(class = "cell-border stripe",
                    rownames = F,
                    filter = "top",
                    editable = FALSE,
                    extensions = "Buttons",
                    options = list(dom = "Bfrtip",
                                   buttons = c("copy",
                                               "csv",
                                               "excel",
                                               "pdf",
                                               "print")))
  })
  
  output$download <- downloadHandler(
    
    graph_tb <- exportar(),
    
    grafo1 <- grafos(graph_tb),
    
    descarga <- graph_from_data_frame(d = grafo1$edges,
                                      directed = FALSE,
                                      vertices = grafo1$nodes) |>
      write_graph("academic_social_network_universidades.graphml",
                  "graphml")
  )
  
}

shinyApp(ui, server)