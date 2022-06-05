#    .-----------------------------------------------------.
#    | FGV  - Fundacao Getulio Vargas                      |
#    | IBRE - Instituto Brasileiro de Economia             |
#    | Superintendência de Pesquisa, Dados e Operação      |
#    | Aplicativo Web - Arquivos CI's                      |
#    | Data inicial : 06/07/2021                           |                    
#    | written_by   : winicius.faquieri@fgv.br             |
#    '-----------------------------------------------------'

packages <- c("dplyr","readxl","tidyr","stringr","shinyWidgets","shinythemes",
              "shiny","purrr","magrittr","openxlsx","fresh","lubridate","janitor")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))
