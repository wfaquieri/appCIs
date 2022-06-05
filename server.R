function(input, output, session) {
  mydata <-
    reactive({
      xlsx_files <-
        input$csvs$datapath %>% .[str_detect(., ".xlsx")]
      csv_files  <- input$csvs$datapath %>% .[str_detect(., ".csv")]
      
      n = length(xlsx_files)
      
      lst(
        df = map2_df(
          1:2,
          1:n,
          ~ read_excel(
            xlsx_files[.y],
            sheet = .x,
            col_types = "text",
            trim_ws = T
          )
        ) %>%
          janitor::clean_names() %>%
          mutate(familia = familia %>% str_replace_all(" ", "")),
        
        # # Lista de CI's informadas
        family = read.csv2(csv_files) %>% as_tibble()
      )
      
    })
  
  # Planilha de Status
  status_df = read_excel("src/Planilha de Status.xlsx",
                         col_types = "text",
                         trim_ws = T) %>%
    janitor::clean_names()
  
  observeEvent(input$go, {
    # Obter cada elemento da lista
    df  <- mydata()$df
    fam <- mydata()$family
    
    # Parcial_CI_Sem_Retorno ---------------------------------------------------
    
    fam %<>%
      janitor::clean_names() %>%
      mutate(familia = familia %>% str_replace_all(" ", "")) %>% pull
    
    # Excluir os itens já Finalizados - FN
    df %<>% filter(status_do_item != "FN")
    
    # Excluir famílias sem um elementar correspondente
    df %<>% filter(elementar != "-")
    
    # Filtro por Familias
    df1 = df %>% filter(familia %in% fam)
    
    # Quantidades de CNPJ por Elementar e UF
    n_cnpj_2 = df1 %>% group_by(elementar, uf_do_preco) %>% select(cnpj) %>%
      count() %>% rename(empr_mapead = n)
    
    n_cnpj =
      df1 %>% group_by(uf_do_preco, job, elementar, familia, status_do_item) %>%
      select(cnpj) %>% count() %>%
      left_join(status_df, by = c("status_do_item" = "status")) %>%
      mutate(info_relev = paste0(n, " ", significado)) %>%
      select(-n,-significado) %>% left_join(n_cnpj_2) %>%
      mutate(
        date = Sys.Date(),
        ano = substr(date, 1, 4),
        mes = substr(date, 6, 7),
        dia = substr(date, 9, 10),
        date = paste0(dia, "-", mes, "-", ano)
      ) %>%
      select(-dia,-mes,-ano) %>%
      relocate(date, .before = uf_do_preco) %>%
      relocate(empr_mapead, .before = info_relev) %>% ungroup() %>%
      select(-status_do_item) %>%
      arrange(familia, elementar)
    
    # Resumindo linhas repetidas por status -----------------------------------
    de_para = n_cnpj %>%
      select(uf_do_preco, elementar, familia, info_relev) %>% distinct()
    
    n_repeticoes <- de_para %>%
      group_by(uf_do_preco, elementar, familia) %>%
      summarise(n_info_by_id = n())
    
    de_para %<>%
      left_join(n_repeticoes,
                by = c("uf_do_preco", "elementar", "familia")) %>%
      arrange(n_info_by_id)
    
    
    de_para %<>% unite("id", uf_do_preco:familia) %>% distinct()
    
    catalogo <- de_para %>%
      pivot_wider(names_from = n_info_by_id,
                  values_from = info_relev)
    
    catalogo %<>% mutate_if(is.list, as.character)
    
    catalogo %<>% replace(. == "NULL", NA)
    
    n = length(catalogo)
    
    catalogo %<>%
      unite("info_relev", 2:n, sep = "", na.rm = T)
    
    
    # Trazendo as informações para a Tabela de Resultados
    n_cnpj %<>% unite("id", c(uf_do_preco, elementar, familia), remove = F)
    n_cnpj %<>% select(-info_relev) %>% distinct()
    
    n_cnpj %<>% left_join(catalogo, by = "id")
    n_cnpj %<>% select(-id)
    
    n_cnpj %<>%
      mutate(
        info_relev = gsub("^c\\(", "", info_relev),
        info_relev = gsub('"', "", info_relev)
      )
    
    
    
    
    
    
    
    
    
    # Parcial_CI_Positivo ------------------------------------------------------
    
    dataset = mydata()$df %>% filter(familia %in% fam)
    
    # Excluir famílias sem um elementar correspondente
    dataset %<>% filter(elementar != "-")
    
    # Quantidade de empresas mapeadas por elementar
    qtd_empr_map = dataset %>% group_by(elementar, familia) %>%
      summarise(n_cnpj = n_distinct(cnpj))
    
    # Filtrar apenas status POSITIVO - PO
    dataset2 = dataset %>% filter(status_do_item == "PO")
    
    # Total de preços coletados
    t_precos = dataset2 %>% group_by(elementar) %>% summarise(n_obs = n())
    
    # De-Para Elementar - Família
    elem_fam = dataset2 %>% select(elementar, familia) %>% distinct()
    
    # Tabela de Resultados
    tab = qtd_empr_map %>% left_join(t_precos) %>% left_join(elem_fam) %>%
      relocate(familia, .before = elementar) %>% relocate(n_obs, .after = last_col()) %>%
      mutate(n_obs = ifelse(is.na(n_obs), 0, n_obs))
    
    sendSweetAlert(
      session = session,
      "Concluido.",
      "Planilhas criadas com sucesso.",
      type = "success"
    )
    
    # Downloadable xlsx of selected dataset ----
    output$goD <- downloadHandler(
      filename = paste0(Sys.Date(), "_Parcial_CI_Sem_Retorno.xlsx"),
      content = function(file) {
        wb = loadWorkbook("src/mod_layout_1.xlsx")
        
        wb %>% writeData(
          sheet = 1,
          n_cnpj,
          startRow = 3,
          colNames = F
        )
        
        # Definindo estilos de formatação
        hs <- createStyle(
          fontName = "Calibri",
          fontSize = 10,
          fontColour = "#ffffff",
          fgFill = "#0F2147",
          textDecoration = "bold",
          border = "TopBottomLeftRight",
          borderStyle = "thin",
          valign = "center",
          halign = "center",
          wrapText = T
        )
        
        hs2 <- createStyle(
          fontName = "Calibri",
          fontSize = 10,
          fontColour = "#ffffff",
          fgFill = "#4472C4",
          textDecoration = "bold",
          border = "TopBottomLeftRight",
          borderStyle = "thin",
          valign = "center",
          halign = "center",
          wrapText = T
        )
        
        # Defines the date column style + the body style
        ds <- createStyle(
          fontName = "Calibri",
          fontSize = 9,
          border = "TopBottomLeftRight",
          borderStyle = "thin",
          numFmt = "DATE",
          wrapText = T,
          valign = "center",
          halign = "center"
        )
        
        bs <- createStyle(
          fontName = "Calibri",
          fontSize = 9,
          border = "TopBottomLeftRight",
          borderStyle = "thin",
          wrapText = T,
          valign = "center",
          halign = "center"
        )
        
        bs2 <- createStyle(
          fontName = "Calibri",
          fontSize = 9,
          border = "TopBottomLeftRight",
          borderStyle = "thin",
          wrapText = T,
          valign = "center",
          halign = "left"
        )
        
        ## style for header
        addStyle(
          wb,
          sheet = 1,
          style = hs,
          rows = 1,
          cols = 1:7,
          gridExpand = T
        )
        addStyle(
          wb,
          sheet = 1,
          style = hs2,
          rows = 2,
          cols = 1:7,
          gridExpand = T
        )
        
        ## style for body
        n = nrow(n_cnpj) + 2
        addStyle(
          wb,
          sheet = 1,
          style = ds,
          rows = 3:n,
          cols = 1,
          gridExpand = T
        )
        addStyle(
          wb,
          sheet = 1,
          style = bs,
          rows = 3:n,
          cols = 2:6,
          gridExpand = T
        )
        addStyle(
          wb,
          sheet = 1,
          style = bs2,
          rows = 3:n,
          cols = 7,
          gridExpand = T
        )
        
        ## Ajustando a largura da coluna 'Elementar indicado'
        wb %>% setColWidths(sheet = 1,
                            cols = 4,
                            widths = 20.71)
        # Salvando workbook
        saveWorkbook(wb, file, overwrite = TRUE)
        
        
      }
    )
    
    output$goW <- downloadHandler(
      filename = paste0(Sys.Date(), "_Parcial_CI_Positivo.xlsx"),
      content = function(file) {
        wb = loadWorkbook("src/mod_layout_2.xlsx")
        
        wb %>% writeData(sheet = 1,
                         tab,
                         startRow = 2,
                         colNames = F)
        
        # Definindo estilos de formatação
        hs <- createStyle(
          fontName = "Calibri",
          fontSize = 9,
          fontColour = "#ffffff",
          fgFill = "#0F2147",
          textDecoration = "bold",
          border = "TopBottomLeftRight",
          borderStyle = "thin",
          valign = "center",
          halign = "center",
          wrapText = T
        )
        
        bs <- createStyle(
          fontName = "Calibri",
          fontSize = 9,
          border = "TopBottomLeftRight",
          borderStyle = "thin",
          wrapText = T,
          valign = "center",
          halign = "center"
        )
        
        wb %>% writeData(sheet = 1,
                         tab,
                         startRow = 2,
                         colNames = F)
        ## style for header
        addStyle(
          wb,
          sheet = 1,
          style = hs,
          rows = 1,
          cols = 1:4,
          gridExpand = T
        )
        ## style for body
        n = nrow(tab) + 1
        addStyle(
          wb,
          sheet = 1,
          style = bs,
          rows = 2:n,
          cols = 1:4,
          gridExpand = T
        )
        
        # Salvando workbook
        saveWorkbook(wb, file, overwrite = TRUE)
        
        
      }
    )
    
  })
}
