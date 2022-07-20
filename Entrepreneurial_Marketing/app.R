library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(visNetwork)
library(ggraph)
library(plotly)
library(datasets)

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
    
    # graph_tbl <- 
    #   df |> 
    #   expand(from = Source, to = Target) |> 
    #   filter(from != to) |> 
    #   ungroup() |> 
    #   graph_from_data_frame(directed = FALSE) |> 
    #   as_tbl_graph() |> 
    #   convert(to_simple)
    # 
    # return(graph_tbl) 
  })
  
  output$graf <- renderVisNetwork ({
    
    graph_tb <- exportar()
    graph_tbl <- as.data.frame(graph_tb)
    nodes <- 
      graph_tbl |> 
      activate(nodes) |>
      # modularidad
      mutate(community=as.character(group_louvain())) |>
      #grado
      mutate(degree = centrality_degree()) |>
      data.frame() |> 
      select(name, community, degree) 
    
    edges <- 
      graph_tbl |> 
      activate(edges) |> 
      data.frame() |> 
      select(from, to)
    
    visNetwork(nodes = nodes, 
               edges = edges, 
               height = "500px") |> 
      visOptions(highlightNearest = TRUE, selectedBy = "name" ) 
  })
  
  output$contents <-  DT::renderDataTable(server = FALSE,{
    
    graph_tb <- exportar() 
    m <- as.data.frame(graph_tb) |> 
      DT::datatable()
    # graph <-as.data.frame(graph_tb) |>  
    #   activate(nodes) |>
    #   mutate(community=as.character(group_louvain())) |>
    #   #grado
    #   mutate(degree = centrality_degree()) |>
    #   data.frame() |> 
    #   select("Nombre" = name,
    #          "Grado" = degree,
    #          "Comunidad" = community) |>
    #   DT::datatable(class = "cell-border stripe", 
    #                 rownames = F, 
    #                 filter = "top", 
    #                 editable = FALSE, 
    #                 extensions = "Buttons", 
    #                 options = list(dom = "Bfrtip",
    #                                buttons = c("copy",
    #                                            "csv",
    #                                            "excel", 
    #                                            "pdf", 
    #                                            "print")))
  })
  
  output$download <- downloadHandler(
    
  )
  
}

shinyApp(ui, server)