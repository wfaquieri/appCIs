lg = "logo.png"
bg = "bg_fgv.jpg"

# ui ----------------------------------------------------------------------

fluidPage(
  div(column(width = 10, 
             h6("Aplicativo Web - v.1.04"),
             hr()
  ),
  
  column(width = 2, 
         tags$img(src = lg)
  )),
  use_theme(create_theme(
    theme = "superhero",
    bs_vars_wells(
      bg = "#17202A"
    )
  )),
  shinyWidgets::setBackgroundImage(src = bg),
  sidebarLayout(
    sidebarPanel(
      width = 6,
      h3("Arquivos de CI's SPDO-SIMG"),
      hr(),
      fileInput("csvs", "Upload dos arquivos aqui", multiple = T),
      actionButton("go", "Gerar Planilhas"),
      downloadButton("goD", "Parcial CI Sem Retorno"),
      downloadButton("goW", "Parcial CI Positivo")),
    mainPanel()
    
  )
  
)