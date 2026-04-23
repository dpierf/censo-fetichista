# server.R
# Censo Fetichista — CRCetim

server <- function(input, output, session) {
  
  # ── 0. HELPERS ──────────────────────────────────────────────────────────────
  
  nulo  <- function(x) if (length(x) == 0L) NULL else x
  sid   <- function(p, s) input[[paste0(p, '_', s)]]
  
  # Gera base reativa filtrada a partir do prefixo da aba
  mk_base <- function(p, extra_fn = NULL) {
    reactive({
      dt <- filtrar_base(base,
                         generos      = nulo(sid(p,'gen')),
                         orientacoes  = nulo(sid(p,'ori')),
                         idades       = nulo(sid(p,'ida')),
                         ufs          = nulo(sid(p,'uf')),
                         capital_int  = nulo(sid(p,'ci')),
                         grupos_renda = nulo(sid(p,'ren')),
                         papeis       = nulo(sid(p,'pap'))
      )
      if (!is.null(extra_fn)) dt <- extra_fn(dt)
      dt
    })
  }
  
  # Reset de filtros por prefixo
  mk_reset <- function(p) {
    observeEvent(input[[paste0(p,'_rst')]], {
      for (s in c('gen','ori','ida','uf','ci','ren','pap'))
        updateSelectizeInput(session, paste0(p,'_',s), selected = character(0))
    })
  }
  
  # Barra horizontal padrão
  barh <- function(dt, x_col, y_col, cor = paleta[2]) {
    dt_ord <- dt[order(get(x_col))]
    plot_ly(dt_ord, x = ~get(x_col), y = ~factor(get(y_col), levels = get(y_col)),
            type = 'bar', orientation = 'h', marker = list(color = cor),
            hovertemplate = '%{y}: %{x}<extra></extra>'
    ) |> layout_padrao() |> layout(xaxis = list(title='n'), yaxis = list(title=''))
  }
  
  # Pizza padrão
  pizza <- function(dt, lbl, val) {
    plot_ly(dt, labels = ~get(lbl), values = ~get(val), type = 'pie',
            marker = list(colors = paleta), textinfo = 'percent',
            hovertemplate = '%{label}: %{value} (%{percent})<extra></extra>'
    ) |> layout_padrao() |>
      layout(legend = list(orientation = 'v', x = 1.02, xanchor = 'left',
                           y = 0.5, yanchor = 'middle'))
  }
  
  # Barra de variável temporal ordinal
  barh_tempo <- function(data_fn, col, cor = paleta[2]) {
    renderPlotly({
      dt <- contar_texto(data_fn(), col, excluir = c('',NA,'Não se Aplica'))
      dt[, categoria := factor(categoria, levels = NIVEIS_TEMPO, ordered = TRUE)]
      dt <- dt[!is.na(categoria)][order(categoria)]
      plot_ly(dt, x = ~n, y = ~categoria, type='bar', orientation='h',
              marker = list(color = cor), hovertemplate = '%{y}: %{x}<extra></extra>'
      ) |> layout_padrao() |> layout(xaxis=list(title='n'), yaxis=list(title=''))
    })
  }
  
  # Saúde: % Sim para 4 indicadores, agrupado por variável categórica
  saude_por_grupo <- function(dt, col_grupo, niveis = NULL) {
    ind <- c('PcD'='var11','Neurodiv.'='var12','Terapia'='var13','Medicação'='var14')
    r <- rbindlist(lapply(names(ind), function(nm) {
      c <- ind[nm]
      d <- dt[!is.na(get(col_grupo)) & get(col_grupo) != '',
              .(pct = round(100 * mean(get(c) == 'Sim', na.rm=TRUE), 1)),
              by = col_grupo]
      setnames(d, col_grupo, 'grupo')
      d[, indicador := nm]
      d
    }))
    if (!is.null(niveis))
      r[, grupo := factor(grupo, levels = niveis, ordered = TRUE)]
    r[order(grupo)]
  }
  
  # Posição × grupo: % por posição dentro de cada categoria
  posicao_por_grupo <- function(dt, col_grupo, niveis = NULL) {
    longa <- rbindlist(lapply(seq_len(nrow(dt)), function(i) {
      pos <- dt$posicao_lista[[i]]
      grp <- dt[[col_grupo]][i]
      if (!length(pos) || is.na(grp)) return(NULL)
      data.table(posicao = pos, grupo = grp)
    }))
    longa <- longa[posicao != 'Outros' & !is.na(grupo)]
    totais <- longa[, .N, by = grupo]
    setnames(totais, 'N', 'total')
    cnt <- longa[, .N, by = .(posicao, grupo)]
    res <- merge(cnt, totais, by = 'grupo')
    res[, pct := round(100 * N / total, 1)]
    if (!is.null(niveis))
      res[, grupo := factor(grupo, levels = niveis, ordered = TRUE)]
    res[order(grupo)]
  }
  
  # Texto → tokens (para wordcloud / trigramas)
  sw_pt <- c(stopwords('pt', source='stopwords-iso'),
             'nao','sim','ja','so','la','ser','ter','esse','essa','isso')
  
  tokenizar <- function(textos) {
    palavras <- unlist(strsplit(
      stri_trans_general(tolower(textos), 'Latin-ASCII') |>
        str_replace_all('[[:punct:]]',' ') |> str_squish(),
      '\\s+'))
    palavras[nchar(palavras) > 2 & !palavras %in% sw_pt]
  }
  
  # Heatmap plotly genérico (duas variáveis ordinais)
  heatmap_cruzado <- function(dt, col1, col2, niveis1, niveis2) {
    mat <- dt[!is.na(get(col1)) & !is.na(get(col2)) & get(col1) != '' & get(col2) != '',
              .N, by = .(v1 = get(col1), v2 = get(col2))]
    mat[, v1 := factor(v1, levels = niveis1, ordered = TRUE)]
    mat[, v2 := factor(v2, levels = niveis2, ordered = TRUE)]
    mat  <- mat[!is.na(v1) & !is.na(v2)]
    wide <- dcast(mat, v1 ~ v2, value.var = 'N', fill = 0L)
    cols <- setdiff(names(wide), 'v1')
    zm   <- as.matrix(wide[, ..cols])
    plot_ly(x = cols, y = as.character(wide$v1), z = zm,
            type = 'heatmap', colors = colorRamp(c('#1a1a1a', '#BA9D7A')),
            text = zm, texttemplate = '%{text}',
            hovertemplate = '%{y} × %{x}: %{z}<extra></extra>'
    ) |> layout_padrao()
  }

  # Eixos específicos para tempo e renda
  eixo_renda <- function(titulo = '') {
    list(title    = titulo,
         tickangle = -35,
         tickvals  = NIVEIS_RENDA_CAT,
         ticktext  = ROTULOS_RENDA[NIVEIS_RENDA_CAT])
  }
  
  eixo_tempo <- function(titulo) {
    list(title      = titulo,
         tickangle  = 0,
         tickvals   = NIVEIS_TEMPO,
         ticktext   = ROTULOS_TEMPO[NIVEIS_TEMPO])
  }
    
  # ── 1. RESETADORES ──────────────────────────────────────────────────────────
  
  for (p in c('p1','ps','p2','p3','p4','p5')) mk_reset(p)
  observeEvent(input$p2_ide, {}, ignoreInit=TRUE)  # placeholder para extensão
  
  # ── 2. BASES REATIVAS ────────────────────────────────────────────────────────
  
  b1 <- mk_base('p1')
  b_ps <- mk_base('ps')
  b2 <- mk_base('p2', extra_fn = function(dt) {
    ids <- nulo(input$p2_ide)
    if (!is.null(ids)) dt <- dt[sapply(identidade_lista, function(x) any(x %in% ids))]
    dt
  })
  b3 <- mk_base('p3')
  b4 <- mk_base('p4')
  b5 <- mk_base('p5')
  
  # ── 3. ABA 1: PERFIL ─────────────────────────────────────────────────────────
  
  output$vb_n   <- renderText(format(nrow(b1()), big.mark='.'))
  output$vb_data_range <- renderText({
    datas <- format(range(b1()$data_resposta, na.rm=TRUE), '%d/%m/%Y')
    paste0(datas[1], ' – ', datas[2])
  })
  
  output$vb_ter <- renderText(paste0(round(100*mean(b_ps()$var13=='Sim',na.rm=TRUE),1),'%'))
  output$vb_nd  <- renderText(paste0(round(100*mean(b_ps()$var12=='Sim',na.rm=TRUE),1),'%'))
  output$vb_pcd <- renderText(paste0(round(100*mean(b_ps()$var11=='Sim',na.rm=TRUE),1),'%'))
  
  output$g_genero <- renderPlotly({
    dt <- contar_texto(b1(), 'genero')
    dt[n <= 5, categoria := 'Outros']
    dt <- dt[, .(n = sum(n)), by = categoria]
    dt <- rbind(dt[categoria != 'Outros'][order(-n)], dt[categoria == 'Outros'])
    dt[, categoria := factor(categoria, levels = categoria)]
    plot_ly(dt,
            x = ~n, y = ~categoria,
            type = 'bar', orientation = 'h',
            marker = list(color = paleta[2]),
            hovertemplate = '%{y}: %{x}<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = 'n'), yaxis = list(title = '', categoryorder = 'array',
                                                     categoryarray = rev(as.character(dt$categoria))))
  })
  
  output$g_orientacao <- renderPlotly({
    dt <- contar_texto(b1(), 'orientacao')
    dt[n <= 5, categoria := 'Outros']
    dt <- dt[, .(n = sum(n)), by = categoria]
    dt <- rbind(dt[categoria != 'Outros'][order(-n)], dt[categoria == 'Outros'])
    dt[, categoria := factor(categoria, levels = categoria)]
    plot_ly(dt,
            x = ~n, y = ~categoria,
            type = 'bar', orientation = 'h',
            marker = list(color = paleta[1]),
            hovertemplate = '%{y}: %{x}<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = 'n'), yaxis = list(title = '', categoryorder = 'array',
                                                     categoryarray = rev(as.character(dt$categoria))))
  })
  
  output$g_idade <- renderPlotly({
    dt <- b1()[!is.na(faixa_agr), .N, by = faixa_agr]
    dt[, faixa_agr := factor(faixa_agr, levels = NIVEIS_IDADE_AGR, ordered = TRUE)]
    dt <- dt[order(faixa_agr)]
    
    cores_idade <- c('#29ABE2','#8DC63F','#F72585','#FFC000',
                     '#F7941D','#ED1C24','#2E6DB4')
    
    plot_ly(dt,
            labels = ~faixa_agr,
            values = ~N,
            type   = 'pie',
            marker = list(colors = cores_idade[seq_len(nrow(dt))]),
            textinfo      = 'label+percent',
            hovertemplate = '%{label}: %{value} (%{percent})<extra></extra>'
    ) |> layout_padrao()
  })
  
  output$g_raca   <- renderPlotly(pizza(contar_texto(b1(),'var5'), 'categoria','n'))
  output$g_escola <- renderPlotly({
    dt <- contar_texto(b1(),'var8')
    dt[, categoria := factor(categoria, levels=NIVEIS_ESCOLA, ordered=TRUE)]
    barh(dt[!is.na(categoria)][order(categoria)], 'n','categoria', paleta[4])
  })
  output$g_renda <- renderPlotly({
    dt <- b1()[!is.na(renda_cat_f), .N, by = renda_cat_f][order(renda_cat_f)]
    setnames(dt, c('categoria','n'))
    plot_ly(dt, x = ~as.character(categoria), y = ~n,
            type = 'bar',
            marker = list(color = paleta[2], line = list(color = '#1a1a1a', width = 1)),
            hovertemplate = '%{x}: %{y}<extra></extra>'
    ) |> layout_padrao() |>
      layout(bargap = 0,
             xaxis  = list(title = '', tickangle = -35, categoryorder = 'array',
                           categoryarray = as.character(levels(dt$categoria))),
             yaxis  = list(title = 'n'))
  })  
  # Choropleth por UF
  output$mapa_uf <- renderLeaflet({
    cnt <- b1()[!is.na(var6) & var6!='', .N, by=var6]
    setnames(cnt, 'var6','name_state')
    
    if (!is.null(shp_estados)) {
      shp <- merge(shp_estados, cnt, by='name_state', all.x=TRUE)
      shp$N[is.na(shp$N)] <- 0L
      pal_cor <- colorNumeric(c('#242424','#BA9D7A'), domain=shp$N)
      leaflet(shp) |>
        addProviderTiles('CartoDB.DarkMatter') |>
        setView(lng=-51, lat=-15, zoom=4) |>
        addPolygons(fillColor=~pal_cor(N), fillOpacity=0.8, color='#555', weight=1,
                    popup=~paste0('<b>',name_state,'</b><br>n = ',N),
                    highlightOptions=highlightOptions(weight=2, color='#BA9D7A', bringToFront=TRUE)) |>
        addLegend(pal=pal_cor, values=~N, title='n', position='bottomright')
    } else {
      leaflet() |> addProviderTiles('CartoDB.DarkMatter') |>
        addControl('<b>Shapefile não disponível</b>', position='topright')
    }
  })
  
  output$g_capital <- renderPlotly({
    dt <- b1()[!is.na(var7) & var7!='', .N, by=var7]
    setnames(dt, c('categoria','n'))
    pizza(dt, 'categoria','n')
  })
  
  # Helper: 100% área empilhada genérico
  area_100 <- function(dt, x_col, y_col, group_col, niveis_x = NULL, niveis_g = NULL, cor_map = NULL) {
    if (!is.null(niveis_x)) dt[, (x_col) := factor(get(x_col), levels = niveis_x, ordered = TRUE)]
    dt <- dt[!is.na(get(x_col))][order(get(x_col))]
    grupos <- if (!is.null(niveis_g)) niveis_g else unique(dt[[group_col]])
    p <- plot_ly()
    for (i in seq_along(grupos)) {
      g <- grupos[i]
      d <- dt[get(group_col) == g]
      cor_i <- if (!is.null(cor_map) && g %in% names(cor_map)) cor_map[[g]] else paleta[((i-1)%%5)+1]
      p <- add_trace(p, x = ~get(x_col), y = ~N, data = d,
                     type = 'scatter', mode = 'none', name = g,
                     stackgroup = 'one', groupnorm = 'percent', fillcolor = cor_i,
                     hovertemplate = paste0(g, ' em %{x}: %{y:.1f}%<extra></extra>'))
    }
    p |> layout_padrao() |>
      layout(xaxis = list(title = '', tickangle = -35),
             yaxis = list(title = '% do grupo', ticksuffix = '%'),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.08))
  }
  
  linhas_grupo <- function(dt, x_col, y_col, group_col,
                           niveis_x = NULL, niveis_g = NULL, cor_map = NULL) {
    if (!is.null(niveis_x))
      dt[, (x_col) := factor(get(x_col), levels = niveis_x, ordered = TRUE)]
    dt <- dt[!is.na(get(x_col))][order(get(x_col))]
    grupos <- if (!is.null(niveis_g)) niveis_g else unique(dt[[group_col]])
    p <- plot_ly()
    for (i in seq_along(grupos)) {
      g     <- grupos[i]
      d     <- dt[get(group_col) == g]
      cor_i <- if (!is.null(cor_map) && g %in% names(cor_map)) cor_map[[g]] else paleta[((i-1)%%length(paleta))+1]
      p <- add_trace(p, x = ~get(x_col), y = ~pct, data = d,
                     type = 'scatter', mode = 'lines+markers', name = g,
                     line   = list(color = cor_i, width = 2),
                     marker = list(color = cor_i, size  = 7),
                     hovertemplate = paste0(g, ' em %{x}: %{y:.1f}%<extra></extra>'))
    }
    p |> layout_padrao() |>
      layout(xaxis  = list(title = '', tickangle = -35),
             yaxis  = list(title = '% do grupo'),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.08))
  }
  
  cruzar_lista_por_faixa <- function(dt, col_lista, niveis_grupo = NULL) {
    rbindlist(lapply(seq_len(nrow(dt)), function(i) {
      vals <- dt[[col_lista]][[i]]
      faixa <- dt$faixa_agr[i]
      if (!length(vals) || is.na(faixa)) return(NULL)
      data.table(grupo = vals, faixa = faixa)
    }))
  }
  
  output$g_gen_idade <- renderPlotly({
    longa <- cruzar_lista_por_faixa(b1(), 'genero_simples')
    cnt   <- longa[grupo != '' & !is.na(faixa), .N, by = .(grupo, faixa)]
    area_100(cnt, 'faixa', 'N', 'grupo', NIVEIS_IDADE_AGR)
  })
  
  output$g_ori_idade <- renderPlotly({
    longa <- cruzar_lista_por_faixa(b1(), 'orientacao_simples')
    cnt   <- longa[grupo != '' & !is.na(faixa), .N, by = .(grupo, faixa)]
    area_100(cnt, 'faixa', 'N', 'grupo', NIVEIS_IDADE_AGR)
  })
  
  output$g_raca_idade <- renderPlotly({
    dt <- b1()[!is.na(faixa_agr) & !is.na(var5) & var5 != '',
               .N, by = .(grupo = var5, faixa = faixa_agr)]
    area_100(dt, 'faixa', 'N', 'grupo', NIVEIS_IDADE_AGR)
  })
  
  output$g_renda_idade <- renderPlotly({
    dt <- b1()[!is.na(faixa_agr) & !is.na(renda_cat) & renda_cat != '',
               .N, by = .(grupo = renda_cat, faixa = faixa_agr)]
    area_100(dt, 'faixa', 'N', 'grupo',
             niveis_x = NIVEIS_IDADE_AGR,
             niveis_g = NIVEIS_RENDA_CAT,
             cor_map  = as.list(COR_RENDA)) |> layout(xaxis = eixo_renda())
  })
  
  output$g_trabalho <- renderPlotly({
    dt <- contar_texto(b1(), 'trabalho', excluir = c('', NA))
    dt <- dt[order(-n)][1:min(10L, .N)]
    barh(dt[order(n)], 'n', 'categoria', paleta[3])
  })
  
  # Saúde (10.1–10.3)
  render_saude <- function(data_fn, col_grupo, niveis=NULL, cor_base=paleta[2]) {
    renderPlotly({
      dt  <- saude_por_grupo(data_fn(), col_grupo, niveis)
      indicadores <- unique(dt$indicador)
      p   <- plot_ly()
      for (i in seq_along(indicadores)) {
        d <- dt[indicador == indicadores[i]]
        p <- add_trace(p, x=~grupo, y=~pct, data=d, type='scatter', mode='lines+markers',
                       name=indicadores[i], line=list(color=paleta[i], width=2),
                       marker=list(color=paleta[i], size=7),
                       hovertemplate=paste0(indicadores[i],': %{y}%<extra></extra>'))
      }
      p |> layout_padrao() |>
        layout(xaxis=list(title=''), yaxis=list(title='% Sim', range=c(0,100)),
               legend=list(orientation='h', x=0.5, xanchor='center', y=1.08))
    })
  }
  
  render_saude_bar <- function(data_fn, col_grupo, niveis = NULL) {
    renderPlotly({
      r <- saude_por_grupo(data_fn(), col_grupo, niveis)
      plot_ly(r, x = ~grupo, y = ~pct, color = ~indicador, colors = paleta[1:4],
              type = 'bar', barmode = 'group',
              hovertemplate = '%{x} – %{fullData.name}: %{y}%<extra></extra>'
      ) |> layout_padrao() |>
        layout(xaxis = list(title = '', tickangle = -30),
               yaxis = list(title = '% Sim', range = c(0, 100)),
               legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.08))
    })
  }
  
  output$g_saude_idade <- render_saude(b_ps, 'faixa_agr', NIVEIS_IDADE_AGR)
  
  mk_tabela_saude <- function(data_fn, col_grupo, niveis = NULL) {
    renderDT({
      r <- saude_por_grupo(data_fn(), col_grupo, niveis)
      wide <- dcast(r, grupo ~ indicador, value.var = 'pct')
      if (!is.null(niveis)) wide <- wide[order(factor(grupo, levels = niveis))]
      setnames(wide, 'grupo', 'Grupo')
      datatable(wide, rownames = FALSE, style = 'bootstrap5',
                options = list(dom = 't', pageLength = 30,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'))
      ) |> formatStyle(
        c('PcD','Neurodiv.','Terapia','Medicação'),
        background = styleColorBar(c(0,100), '#BA9D7A'),
        backgroundSize = '90% 60%', backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    })
  }
  
  output$t_saude_gen  <- mk_tabela_saude(b_ps, 'genero_cat')
  output$t_saude_raca <- mk_tabela_saude(b_ps, 'var5')
  output$g_saude_renda <- renderPlotly({
    r <- saude_por_grupo(b_ps(), 'renda_cat', NIVEIS_RENDA_CAT)
    indicadores <- unique(r$indicador)
    p <- plot_ly()
    for (i in seq_along(indicadores)) {
      d <- r[indicador == indicadores[i]]
      p <- add_trace(p, x = ~grupo, y = ~pct, data = d,
                     type = 'scatter', mode = 'lines+markers', name = indicadores[i],
                     line   = list(color = paleta[i], width = 2),
                     marker = list(color = paleta[i], size  = 7),
                     hovertemplate = paste0(indicadores[i], ': %{y}%<extra></extra>'))
    }
    p |> layout_padrao() |>
      layout(xaxis  = eixo_renda(),
             yaxis  = list(title = '% Sim', range = c(0, 100)))
  })
  
  # ── 4. ABA 2: IDENTIDADE ─────────────────────────────────────────────────────
  
  output$g_id_idade <- renderPlotly({
    longa <- rbindlist(lapply(seq_len(nrow(b2())), function(i) {
      ids   <- b2()$identidade_lista[[i]]
      faixa <- b2()$faixa_agr[i]
      if (!length(ids) || is.na(faixa)) return(NULL)
      data.table(grupo = ids, faixa = faixa)
    }))
    cnt   <- longa[grupo != 'Outros' & !is.na(faixa), .N, by = .(grupo, faixa)]
    tot   <- cnt[, .(tot = sum(N)), by = faixa]
    r     <- merge(cnt, tot, by = 'faixa')
    r[, pct := round(100 * N / tot, 1)]
    linhas_grupo(r, 'faixa', 'pct', 'grupo', NIVEIS_IDADE_AGR)
  })
  
  output$g_id_renda <- renderPlotly({
    longa <- rbindlist(lapply(seq_len(nrow(b2())), function(i) {
      ids   <- b2()$identidade_lista[[i]]
      renda <- b2()$renda_cat[i]
      if (!length(ids) || is.na(renda)) return(NULL)
      data.table(grupo = ids, renda = renda)
    }))
    cnt <- longa[grupo != 'Outros' & !is.na(renda), .N, by = .(grupo, renda)]
    tot <- cnt[, .(tot = sum(N)), by = renda]
    r   <- merge(cnt, tot, by = 'renda')
    r[, pct := round(100 * N / tot, 1)]
    linhas_grupo(r, 'renda', 'pct', 'grupo', NIVEIS_RENDA_CAT) |> layout(xaxis = eixo_renda())
  }) 
  
  # Venn (10.4)
  output$g_identidade <- renderPlotly({
    vals <- unlist(b2()$identidade_lista)
    dt   <- as.data.table(sort(table(vals), decreasing = TRUE))
    setnames(dt, c('categoria', 'n'))
    dt[, cor := COR_IDENTIDADE[categoria]]
    dt[is.na(cor), cor := '#888888']
    dt <- dt[order(-n)]
    plot_ly(dt,
            x = ~n,
            y = ~factor(categoria, levels = rev(categoria)),
            type = 'bar', orientation = 'h',
            marker = list(color = dt$cor),
            hovertemplate = '%{y}: %{x}<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = 'n'), yaxis = list(title = ''),
             showlegend = FALSE)
  })
  
  output$g_venn <- renderPlot(bg = '#1a1a1a', {
    dt <- b2()
    lst <- list(
      Fetichista            = dt[sapply(identidade_lista, function(x) 'Fetichista' %in% x),         var45],
      `Praticante de BDSM`  = dt[sapply(identidade_lista, function(x) 'Praticante de BDSM' %in% x), var45],
      Sadomasoquista        = dt[sapply(identidade_lista, function(x) 'Sadomasoquista' %in% x),      var45]
    )
    ggvenn::ggvenn(lst,
                   fill_color      = unname(COR_IDENTIDADE[c('Fetichista','Praticante de BDSM','Sadomasoquista')]),
                   fill_alpha      = 0.6,
                   stroke_color    = '#F4E9D3',
                   stroke_size     = 0.5,
                   set_name_color  = '#F4E9D3',
                   set_name_size   = 4,
                   text_color      = '#F4E9D3',
                   text_size       = 4.5,
                   show_percentage = T
    ) +
      theme(
        plot.background  = element_rect(fill = '#1a1a1a', color = NA),
        panel.background = element_rect(fill = '#1a1a1a', color = NA)
      )
  })
  
  output$g_relacao <- renderPlotly({
    dt <- contar_texto(b2(), 'var16', delim = ';\\s*')
    pizza(dt, 'categoria', 'n')
  })
  
  # 10.5 — relação BDSM × faixa etária
  output$g_rel_idade <- renderPlotly({
    longa <- b2()[!is.na(faixa_agr) & !is.na(var16) & var16 != '',
                  .(rel = trimws(unlist(strsplit(var16, ';\\s*'))), faixa = faixa_agr), by = var45]
    cnt   <- longa[, .N, by = .(rel, faixa)]
    cnt[, faixa := factor(faixa, levels = NIVEIS_IDADE_AGR, ordered = TRUE)]
    cnt   <- cnt[!is.na(faixa)][order(faixa)]
    rels  <- unique(cnt$rel)
    p     <- plot_ly()
    for (i in seq_along(rels)) {
      d <- cnt[rel == rels[i]]
      p <- add_trace(p, x = ~as.character(faixa), y = ~N, data = d,
                     type = 'scatter', mode = 'none', name = rels[i],
                     stackgroup = 'one', groupnorm = 'percent',
                     fillcolor = paleta[((i - 1) %% 5) + 1],
                     hovertemplate = '%{x}: %{y:.1f}%<extra></extra>')
    }
    p |> layout_padrao() |>
      layout(xaxis = list(title = '', tickangle = -35),
             yaxis = list(title = '% do grupo', ticksuffix = '%'),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.08))
  })
  
  # 10.6a — relação BDSM × gênero
  output$g_rel_gen <- renderPlotly({
    dt <- b2()
    longa <- rbindlist(lapply(seq_len(nrow(dt)), function(i) {
      rels <- trimws(unlist(strsplit(dt$var16[i],';\\s*')))
      gens <- dt$genero_simples[[i]]
      if (!length(rels)||!length(gens)) return(NULL)
      CJ(rel=rels, gen=gens)
    }))
    cnt <- longa[gen!='', .N, by=.(rel,gen)]
    mat <- dcast(cnt, rel~gen, value.var='N', fill=0L)
    gens <- setdiff(names(mat),'rel')
    zm <- as.matrix(mat[, ..gens])
    plot_ly(x = gens, y = as.character(mat$rel), z = zm,
            type = 'heatmap', colors = colorRamp(c('#242424', '#BA9D7A')),
            text = zm, texttemplate = '%{text}',
            hovertemplate='Relação: %{y}<br>Gênero: %{x}<br>n: %{z}<extra></extra>'
    ) |> layout_padrao()
  })
  
  # 10.6b — relação BDSM × orientação
  output$g_rel_ori <- renderPlotly({
    dt <- b2()
    longa <- rbindlist(lapply(seq_len(nrow(dt)), function(i) {
      rels <- trimws(unlist(strsplit(dt$var16[i],';\\s*')))
      oris <- dt$orientacao_simples[[i]]
      if (!length(rels)||!length(oris)) return(NULL)
      CJ(rel=rels, ori=oris)
    }))
    cnt <- longa[ori!='', .N, by=.(rel,ori)]
    mat <- dcast(cnt, rel~ori, value.var='N', fill=0L)
    gens <- setdiff(names(mat),'rel')
    zm <- as.matrix(mat[, ..gens])
    plot_ly(x = gens, y = as.character(mat$rel), z = zm,
            type = 'heatmap', colors = colorRamp(c('#242424', '#BA9D7A')),
            text = zm, texttemplate = '%{text}',
            hovertemplate='Relação: %{y}<br>Orientação: %{x}<br>n: %{z}<extra></extra>'
    ) |> layout_padrao()
  })
  
  output$g_posicoes <- renderPlotly({
    vals <- unlist(b2()$posicao_lista)
    vals <- vals[vals != 'Outros']
    dt   <- as.data.table(sort(table(vals), decreasing = TRUE))
    setnames(dt, c('categoria','n'))
    dt[, cor := COR_POSICAO[categoria]]
    dt <- dt[order(-n)]
    plot_ly(dt, x = ~n, y = ~factor(categoria, levels = rev(categoria)),
            type = 'bar', orientation = 'h',
            marker = list(color = ~cor),
            hovertemplate = '%{y}: %{x}<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = 'n'), yaxis = list(title = ''),
             showlegend = FALSE)
  })
  
  # Heatmap posição × gênero (bug corrigido com CJ)
  output$g_pos_genero <- renderPlotly({
    dt <- b2()
    longa <- rbindlist(lapply(seq_len(nrow(dt)), function(i) {
      pos <- dt$posicao_lista[[i]]
      gen <- dt$genero_simples[[i]]
      if (!length(pos) || !length(gen)) return(NULL)
      CJ(posicao = pos, gen = gen)
    }))
    longa <- longa[posicao != 'Outros' & gen != '']
    cnt   <- longa[, .N, by = .(posicao, gen)]
    tot   <- longa[, .N, by = gen]
    setnames(tot, 'N', 'total')
    cnt   <- merge(cnt, tot, by = 'gen')
    cnt[, pct := round(100 * N / total, 1)]
    mat_pct <- dcast(cnt, posicao ~ gen, value.var = 'pct', fill = 0)
    mat_n   <- dcast(cnt, posicao ~ gen, value.var = 'N',   fill = 0L)
    gens    <- setdiff(names(mat_pct), 'posicao')
    zm_pct  <- as.matrix(mat_pct[, ..gens])
    zm_n    <- as.matrix(mat_n[, ..gens])
    txt     <- matrix(paste0(zm_pct, '%<br>n=', zm_n),
                      nrow = nrow(zm_pct), ncol = ncol(zm_pct))
    plot_ly(x = gens, y = mat_pct$posicao, z = zm_pct,
            type = 'heatmap', colors = colorRamp(c('#F4E9D3','#4a2a2a')),
            text = txt, texttemplate = '%{text}',
            hovertemplate = 'Posição: %{y}<br>Gênero: %{x}<br>%{text}<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = ''), yaxis = list(title = ''))
  })  
  output$g_t_fet  <- barh_tempo(b2,'var17',paleta[2])
  output$g_t_bdsm <- barh_tempo(b2,'var18',paleta[1])
  output$g_t_sado <- barh_tempo(b2,'var19',paleta[3])
  
  # 10.9 — posição × faixa etária e por grupo de renda
  output$g_pos_idade <- renderPlotly({
    r <- posicao_por_grupo(b2(), 'faixa_agr', NIVEIS_IDADE_AGR)
    linhas_grupo(r, 'grupo', 'pct', 'posicao',
                 niveis_x = NIVEIS_IDADE_AGR,
                 niveis_g = POSICOES_ORD,
                 cor_map  = as.list(COR_POSICAO_IND))
  })
  
  output$g_pos_renda <- renderPlotly({
    r <- posicao_por_grupo(b2(), 'renda_cat', NIVEIS_RENDA_CAT)
    linhas_grupo(r, 'grupo', 'pct', 'posicao',
                 niveis_x = NIVEIS_RENDA_CAT,
                 niveis_g = POSICOES_ORD,
                 cor_map  = as.list(COR_POSICAO_IND)) |> layout(xaxis = eixo_renda())
  })
  
  # ── 5. ABA 3: FETICHES ───────────────────────────────────────────────────────
  
  resumo_fet <- reactive(calcular_fetiches_pareados(copy(b3())))
  
  # Top-10 (ponto 11)
  # Top-10 global (calculado uma vez, fora do reativo)
  top10_global <- resumo_fetiches[order(-n_interesse)][1:10, .(fetiche, n_global = n_interesse)]
  
  output$g_top10 <- renderPlotly({
    rf <- resumo_fet()[order(-n_interesse)][1:min(10L,.N)]
    rf <- rf[order(n_interesse)]
    plot_ly(rf,
            x = ~n_interesse, y = ~factor(fetiche, levels = fetiche),
            type = 'bar', orientation = 'h',
            marker = list(color = colorRampPalette(c(paleta[1], paleta[2]))(nrow(rf))),
            text = ~n_interesse, textposition = 'outside',
            hovertemplate = '<b>%{y}</b>: %{x} respondentes<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = 'n com interesse'), yaxis = list(title = ''))
  })
  
  output$t_top10_comparativo <- renderDT({
    rf_fil <- resumo_fet()[order(-n_interesse)][1:min(10L,.N),
                                                .(fetiche, n_filtrado = n_interesse)]
    rf_fil[, rank_filtrado := seq_len(.N)]
    comp <- merge(
      top10_global[, rank_global := seq_len(.N)],
      rf_fil, by = 'fetiche', all = TRUE
    )[order(rank_filtrado)]
    comp[is.na(rank_filtrado), rank_filtrado := 11L]
    comp[, movimento := fcase(
      is.na(rank_global),           '★ Novo',
      rank_global > rank_filtrado,  paste0('▲ ', rank_global - rank_filtrado),
      rank_global < rank_filtrado,  paste0('▼ ', rank_filtrado - rank_global),
      default = '='
    )]
    comp[, .(Fetiche = fetiche, `# Global` = n_global,
             `# Filtrado` = n_filtrado, Movimento = movimento)] |>
      datatable(rownames = FALSE, style = 'bootstrap5',
                options = list(dom = 't', pageLength = 10,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'))
      ) |> formatStyle('Movimento',
                       color = styleEqual(
                         c('▲', '▼', '★', '='),
                         c('#8DC63F','#ED1C24','#FFC000','#D4CAA9')
                       )
      )
  })
  
  output$g_taxa_pratica <- renderPlotly({
    rf <- calcular_fetiches_pareados(copy(b3()))
    rf <- rf[n_interesse > 0][, taxa := round(100 * n_ambos / n_interesse, 1)]
    rf <- rf[order(taxa)]
    plot_ly(rf,
            x = ~taxa, y = ~factor(fetiche, levels = fetiche),
            type = 'bar', orientation = 'h',
            marker = list(color = colorRampPalette(c('#FFC000', '#ED1C24'))(nrow(rf))),
            text  = ~paste0(taxa, '%'),
            textposition = 'outside',
            hovertemplate = '<b>%{y}</b><br>%{x:.1f}% dos interessados já praticaram<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = '% dos interessados que já praticaram', range = c(0, 115)),
             yaxis = list(title = ''))
  })
  
  # Dumbbell
  output$g_dumbbell <- renderPlotly({
    rf   <- resumo_fet()[order(n_interesse)]
    modo <- input$p3_modo
    p    <- plot_ly()
    if (modo %in% c('ambos','interesse'))
      p <- add_trace(p, x=rf$n_interesse, y=rf$fetiche, type='scatter', mode='markers',
                     name='Interesse', marker=list(color=paleta[2],size=11),
                     hovertemplate='<b>%{y}</b> – Interesse: %{x}<extra></extra>')
    if (modo %in% c('ambos','pratica'))
      p <- add_trace(p, x=rf$n_pratica, y=rf$fetiche, type='scatter', mode='markers',
                     name='Prática', marker=list(color=paleta[1],size=11),
                     hovertemplate='<b>%{y}</b> – Prática: %{x}<extra></extra>')
    if (modo=='ambos') {
      for (i in seq_len(nrow(rf)))
        p <- add_segments(p, x=rf$n_pratica[i], xend=rf$n_interesse[i],
                          y=rf$fetiche[i], yend=rf$fetiche[i],
                          line=list(color='#555',width=1.5), showlegend=FALSE, hoverinfo='none')
    }
    p |> layout_padrao() |>
      layout(yaxis=list(title='',tickfont=list(size=10)),
             xaxis=list(title='n respondentes'),
             legend=list(orientation='h',x=0.5,xanchor='center',y=1.08))
  })
  
  output$g_so_interesse <- renderPlotly({
    rf <- resumo_fet()[order(-n_so_interesse)][1:min(10L,.N)]
    barh(rf[order(n_so_interesse)], 'n_so_interesse','fetiche', paleta[3])
  })
  output$g_so_pratica <- renderPlotly({
    rf <- resumo_fet()[order(-n_so_pratica)][1:min(10L,.N)]
    barh(rf[order(n_so_pratica)], 'n_so_pratica','fetiche', paleta[1])
  })
  
  # ── 6. ABA 4: TRAJETÓRIA ─────────────────────────────────────────────────────
  
  output$g_desc_descoberta <- renderPlotly({
    n_tot <- nrow(b4())
    mk <- function(col, label) {
      dt <- contar_texto(b4(), col, excluir = c('',NA,'Não se Aplica'))
      dt[, tipo := label]
      dt[, pct  := round(100 * n / n_tot, 1)]
      dt
    }
    dt  <- rbind(mk('var28','Fetichismo'), mk('var27','BDSM'))
    cats <- dt[tipo == 'Fetichismo'][order(-n), categoria]
    dt[, categoria := factor(categoria, levels = rev(cats))]
    plot_ly(dt, x = ~n, y = ~categoria, color = ~tipo,
            colors  = paleta[1:2], type = 'bar', orientation = 'h',
            barmode = 'group',
            text    = ~paste0(pct, '%'),
            textposition = 'outside',
            hovertemplate = '%{y} (%{fullData.name}): %{x} (=%{text})<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis  = list(title = 'n'),
             yaxis  = list(title = ''),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.08))
  })
  
  output$g_t_estudo  <- barh_tempo(b4,'var29',paleta[4])
  output$g_t_pratica <- barh_tempo(b4,'var30',paleta[2])
  output$g_t_eventos <- barh_tempo(b4,'var31',paleta[3])
  
  # 10.7 — heatmap tempo BDSM × tempo Fetichismo e tempo estudo x tempo prática
  output$g_cruzado_bdsm_fet <- renderPlotly({
    heatmap_cruzado(b4(),'var18','var17', NIVEIS_TEMPO, NIVEIS_TEMPO) |>
      layout(xaxis = eixo_tempo('Tempo (anos) que conhece Fetichismo'),
             yaxis = eixo_tempo('Tempo (anos) que conhece BDSM'))
  })
  output$g_cruzado_est_prat <- renderPlotly({
    heatmap_cruzado(b4(),'var29','var30', NIVEIS_TEMPO, NIVEIS_TEMPO) |>
      layout(xaxis = eixo_tempo('Tempo (anos) que pratica'),
             yaxis = eixo_tempo('Tempo (anos) que estuda'))
  })
  
  output$g_fontes <- renderPlotly({
    n_tot <- nrow(b4())
    vals  <- unlist(b4()$fonte_lista)
    vals  <- vals[!is.na(vals)]
    dt    <- as.data.table(sort(table(vals), decreasing = TRUE))
    setnames(dt, c('categoria','n'))
    dt[, pct := round(100 * n / n_tot, 1)]
    dt[, cor := ifelse(categoria == 'Não me mantenho informado', paleta[1], paleta[2])]
    dt <- dt[order(pct)]
    plot_ly(dt, x = ~pct, y = ~factor(categoria, levels = categoria),
            type = 'bar', orientation = 'h',
            marker = list(color = ~cor),
            text  = ~paste0(pct, '%'),
            textposition = 'outside',
            hovertemplate = '%{y}: %{x:.1f}% (n=%{customdata})<extra></extra>',
            customdata = ~n
    ) |> layout_padrao() |>
      layout(xaxis = list(title = '% do total de respondentes', range = c(0, 115)),
             yaxis = list(title = ''), showlegend = FALSE)
  })
  
  output$g_rel_eventos <- renderPlotly({
    dt <- contar_texto(b4(),'var37', delim=';\\s*')
    barh(dt[order(-n)], 'n','categoria', paleta[2])
  })
  
  # Primeira experiência (ponto 12)
  output$g_faixa_exp_fet <- renderPlotly({
    dt <- b4()[!is.na(faixa_exp_fet), .N, by=faixa_exp_fet]
    dt[, faixa_exp_fet:=factor(faixa_exp_fet,levels=NIVEIS_FAIXA_EXP,ordered=TRUE)]
    dt <- dt[!is.na(faixa_exp_fet)][order(faixa_exp_fet)]
    plot_ly(dt, x=~faixa_exp_fet, y=~N, type='bar', marker=list(color=paleta[2]),
            hovertemplate='%{x}: %{y}<extra></extra>'
    ) |> layout_padrao() |> layout(xaxis=list(title=''), yaxis=list(title='n'))
  })
  output$g_faixa_exp_bdsm <- renderPlotly({
    dt <- b4()[!is.na(faixa_exp_bdsm), .N, by=faixa_exp_bdsm]
    dt[, faixa_exp_bdsm:=factor(faixa_exp_bdsm,levels=NIVEIS_FAIXA_EXP,ordered=TRUE)]
    dt <- dt[!is.na(faixa_exp_bdsm)][order(faixa_exp_bdsm)]
    plot_ly(dt, x=~faixa_exp_bdsm, y=~N, type='bar', marker=list(color=paleta[1]),
            hovertemplate='%{x}: %{y}<extra></extra>'
    ) |> layout_padrao() |> layout(xaxis=list(title=''), yaxis=list(title='n'))
  })
  output$g_origem_exp_fet <- renderPlotly({
    dt <- b4()[!is.na(origem_exp_fet), .N, by=origem_exp_fet][order(-N)]
    setnames(dt, c('categoria','n'))
    pizza(dt,'categoria','n')
  })
  output$g_origem_exp_bdsm <- renderPlotly({
    dt <- b4()[!is.na(origem_exp_bdsm), .N, by=origem_exp_bdsm][order(-N)]
    setnames(dt, c('categoria','n'))
    pizza(dt,'categoria','n')
  })
  output$g_status_exp <- renderPlotly({
    dt <- b4()[!is.na(status_exp), .N, by=status_exp]
    setnames(dt, c('categoria','n'))
    pizza(dt,'categoria','n')
  })
  output$g_ano_exp <- renderPlotly({
    agrupar_ano <- function(ano) {
      fcase(
        ano < 1990,              'Antes de 1990',
        ano < 1995,              '1990–1994',
        ano < 2000,              '1995–1999',
        ano < 2005,              '2000–2004',
        ano < 2010,              '2005–2009',
        ano < 2015,              '2010–2014',
        ano < 2020,              '2015–2019',
        ano <= 2025,             '2020–2025',
        default = NA_character_
      )
    }
    niveis_ano <- c('Antes de 1990','1990–1994','1995–1999','2000–2004',
                    '2005–2009','2010–2014','2015–2019','2020–2025')
    dt_f <- b4()[!is.na(ano_exp_fet),  .(periodo = agrupar_ano(ano_exp_fet),  tipo = 'Fetichismo')]
    dt_b <- b4()[!is.na(ano_exp_bdsm), .(periodo = agrupar_ano(ano_exp_bdsm), tipo = 'BDSM')]
    cnt  <- rbind(dt_f, dt_b)[!is.na(periodo), .N, by = .(periodo, tipo)]
    cnt[, periodo := factor(periodo, levels = niveis_ano, ordered = TRUE)]
    cnt  <- cnt[order(periodo)]
    plot_ly(cnt, x = ~periodo, y = ~N, color = ~tipo, colors = paleta[1:2],
            type = 'scatter', mode = 'lines+markers+text',
            text = ~N, textposition = 'top center',
            textfont = list(size = 9), marker = list(size = 7),
            hovertemplate = '%{x}: %{y} respondentes<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis  = list(title = '', tickangle = -35),
             yaxis  = list(title = 'n'),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.08))
  })  
  
  output$g_cruzado_exp_fet_idade <- renderPlotly({
    dt <- b4()[!is.na(faixa_exp_fet) & !is.na(faixa_agr),
               .N, by = .(v1 = faixa_exp_fet, v2 = faixa_agr)]
    dt[, v1 := factor(v1, levels = NIVEIS_FAIXA_EXP, ordered = TRUE)]
    dt[, v2 := factor(v2, levels = NIVEIS_IDADE_AGR, ordered = TRUE)]
    dt   <- dt[!is.na(v1) & !is.na(v2)]
    wide <- dcast(dt, v1 ~ v2, value.var = 'N', fill = 0L)
    cols <- setdiff(names(wide), 'v1')
    zm   <- as.matrix(wide[, ..cols])
    plot_ly(x = cols, y = as.character(wide$v1), z = zm,
            type = 'heatmap', colors = colorRamp(c('#BA9D7A','#1a1a1a')),
            text = zm, texttemplate = '%{text}',
            hovertemplate = '1ª exp: %{y}<br>Faixa atual: %{x}<br>n: %{z}<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = 'Faixa etária atual'),
             yaxis = list(title = 'Faixa etária na 1ª exp. Fetichista',
                          tickvals = as.character(NIVEIS_FAIXA_EXP),
                          ticktext = gsub(' anos','', NIVEIS_FAIXA_EXP)))
  })
  
  output$g_cruzado_exp_bdsm_idade <- renderPlotly({
    dt <- b4()[!is.na(faixa_exp_bdsm) & !is.na(faixa_agr),
               .N, by = .(v1 = faixa_exp_bdsm, v2 = faixa_agr)]
    dt[, v1 := factor(v1, levels = NIVEIS_FAIXA_EXP, ordered = TRUE)]
    dt[, v2 := factor(v2, levels = NIVEIS_IDADE_AGR, ordered = TRUE)]
    dt   <- dt[!is.na(v1) & !is.na(v2)]
    wide <- dcast(dt, v1 ~ v2, value.var = 'N', fill = 0L)
    cols <- setdiff(names(wide), 'v1')
    zm   <- as.matrix(wide[, ..cols])
    plot_ly(x = cols, y = as.character(wide$v1), z = zm,
            type = 'heatmap', colors = colorRamp(c('#BA9D7A','#1a1a1a')),
            text = zm, texttemplate = '%{text}',
            hovertemplate = '1ª exp: %{y}<br>Faixa atual: %{x}<br>n: %{z}<extra></extra>'
    ) |> layout_padrao() |>
      layout(xaxis = list(title = 'Faixa etária atual'),
             yaxis = list(title = 'Faixa etária na 1ª exp. BDSM',
                          tickvals = as.character(NIVEIS_FAIXA_EXP),
                          ticktext = gsub(' anos','', NIVEIS_FAIXA_EXP)))
  })
  
  # ── 7. ABA 5: VOZES ──────────────────────────────────────────────────────────
  
  output$card_wc <- renderUI({
    def <- input$p5_def
    card(
      card_header(paste0('Nuvem: ', LABELS_DEF[def])),
      wordcloud2Output(paste0('wc_def_', def), height = '360px')
    )
  })
  
  output$card_trigramas <- renderUI({
    def <- input$p5_def
    card(
      card_header(paste0('Trigramas: ', LABELS_DEF[def])),
      plotlyOutput(paste0('g_trigramas_', def), height = '360px')
    )
  })
  
  # Wordcloud e trigramas agora com outputId dinâmico
  observe({
    def <- input$p5_def
    col    <- def
    textos <- b5()[[col]]
    textos <- textos[!is.na(textos) & nzchar(textos)]
    
    output[[paste0('wc_def_', def)]] <- renderWordcloud2({
      dt_freq <- as.data.table(sort(table(tokenizar(textos)), decreasing = TRUE))
      setnames(dt_freq, c('word', 'freq'))
      dt_freq <- dt_freq[freq >= 3L & nchar(as.character(word)) > 2]
      wordcloud2(dt_freq, size = 0.55,
                 color = rep(paleta, length.out = nrow(dt_freq)),
                 backgroundColor = '#1a1a1a',
                 fontFamily = 'Inter', fontWeight = 'bold')
    })
    
    output[[paste0('g_trigramas_', def)]] <- renderPlotly({
      tok_list <- lapply(
        strsplit(stri_trans_general(tolower(textos), 'Latin-ASCII') |>
                   str_replace_all('[[:punct:]]', ' ') |> str_squish(), '\\s+'),
        function(x) x[nchar(x) > 2 & !x %in% sw_pt])
      trigramas <- unlist(lapply(tok_list, function(x) {
        n <- length(x); if (n < 3L) return(character(0))
        sapply(seq_len(n - 2L), function(i) paste(x[i:(i + 2L)], collapse = ' '))
      }))
      dt <- as.data.table(sort(table(trigramas), decreasing = TRUE))
      setnames(dt, c('categoria', 'n'))
      dt <- dt[n > 1L][1:min(20L, .N)]
      barh(dt[order(n)], 'n', 'categoria', paleta[3])
    })
  })  
  output$wc_def <- renderWordcloud2({
    col    <- input$p5_def
    textos <- b5()[[col]]
    textos <- textos[!is.na(textos) & nzchar(textos)]
    dt_freq <- as.data.table(sort(table(tokenizar(textos)), decreasing=TRUE))
    setnames(dt_freq, c('word','freq'))
    dt_freq <- dt_freq[freq>=3L & nchar(as.character(word))>2]
    wordcloud2(dt_freq, size=0.55, color=rep(paleta,length.out=nrow(dt_freq)),
               backgroundColor='#1a1a1a', fontFamily='Inter', fontWeight='bold')
  })
  
  output$g_trigramas <- renderPlotly({
    col    <- input$p5_def
    textos <- b5()[[col]]
    textos <- textos[!is.na(textos) & nzchar(textos)]
    tok_list <- lapply(
      strsplit(stri_trans_general(tolower(textos),'Latin-ASCII') |>
                 str_replace_all('[[:punct:]]',' ') |> str_squish(), '\\s+'),
      function(x) x[nchar(x)>2 & !x %in% sw_pt])
    trigramas <- unlist(lapply(tok_list, function(x) {
      n <- length(x); if (n<3L) return(character(0))
      sapply(seq_len(n-2L), function(i) paste(x[i:(i+2L)], collapse=' '))
    }))
    dt <- as.data.table(sort(table(trigramas), decreasing=TRUE))
    setnames(dt, c('categoria','n'))
    dt <- dt[n>1L][1:min(20L,.N)]
    barh(dt[order(n)], 'n','categoria', paleta[3])
  })
  
  output$g_fontes_nom <- renderPlotly({
    dt <- resolver_entidades(copy(b5()), 'var36', map36)
    EXCLUIR_FONTES <- c('Redes Sociais','WhatsApp/Telegram','Estudo','Comunidade',
                        'Sites em Geral','Reddit ou Discord','Podcasts',
                        'Audiovisual','Não se Aplica')
    dt <- dt[!alvo %in% EXCLUIR_FONTES][order(-N)][1:min(15L,.N)]
    setnames(dt, c('categoria','n'))
    barh(dt[order(n)], 'n','categoria', paleta[2])
  })
  
  output$g_eventos_nom <- renderPlotly({
    dt <- resolver_entidades(copy(b5()), 'var38', map38)
    dt <- dt[alvo!='Não se Aplica'][order(-N)][1:min(15L,.N)]
    setnames(dt, c('categoria','n'))
    barh(dt[order(n)], 'n','categoria', paleta[1])
  })
  
  output$tbl_abertas <- renderDT({
    col <- input$p5_aberta
    dt  <- b5()[!is.na(get(col)) & nzchar(get(col)),
                .(Gênero=genero, Orientação=orientacao,
                  `Faixa etária`=var4, Resposta=get(col))]
    datatable(dt, rownames=FALSE, style='bootstrap5',
              options=list(pageLength=8, scrollX=TRUE,
                           language=list(url='//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')))
  })
}