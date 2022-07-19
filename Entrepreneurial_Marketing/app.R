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
    menuItem("Importar datos", tabName = "importar_datos", icon = icon("upload")),
    
    menuItem("Figura", tabName = "fig", icon = icon("table")),
    
    menuItem("Tabla", icon = icon("book"), tabName = ("tabla")),
    #download
    menuItem("Descargar",icon = icon("fas fa-download"), downloadButton("download", "Download full results"))
  )
)

setup <- dashboardBody(
  tabItems(
    tabItem(tabName = "importar_datos",
            fluidPage(br(), h2("Importar"), fileInput("upload", "Choose csv", accept = c(".csv"), width = '500px'))
    ),
    tabItem(tabName = "fig",
            fluidPage(plotlyOutput("graf"))
    ),
    tabItem(tabName = "tabla",
            tableOutput("contents")
    )
  )
)

ui <- dashboardPage(
  skin = "red",
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
    
    req(input$file)
    
    df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    
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
  
  output$graf <- renderPlotly ({
    
    graph_tb <- exportar()
    
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
  
  output$contents <- renderTable(
    
    graph_tb <- exportar() |>  
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
  )
  
  output$download <- downloadHandler(
    
  )
  
}

shinyApp(ui, server)