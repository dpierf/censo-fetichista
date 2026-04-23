# ui.R
# Censo Fetichista — CRCetim

ui <- page_navbar(
  title  = 'Censo Fetichista Brasileiro',
  theme  = tema_app, lang = 'pt-BR', id = 'navbar',
  fillable = FALSE,
  header = tags$head(tags$style(HTML('
    .card           { background:#242424; border:1px solid #333; border-radius:8px; }
    .card-header    { background:#2e2e2e; border-bottom:1px solid #333; color:#F4E9D3; font-weight:600; }
    .sidebar-content{ background:#1e1e1e; border-right:1px solid #333; padding:14px; }
    .nav-link       { color:#D4CAA9 !important; }
    .nav-link.active{ color:#BA9D7A !important; border-bottom:2px solid #BA9D7A; }
    .value-box      { border-radius:8px; }
    .selectize-input{ background:#2e2e2e !important; color:#F4E9D3 !important; border-color:#444 !important; }
    .form-check-label{ color:#D4CAA9; }
  '))),
  
  # ═══ ABA 1: DEMOGRÁFICO ═══════════════════════════════════════════════════
  nav_panel(title = tagList(bsicons::bs_icon('people-fill'), ' Demográfico'), value = 'p1',
            layout_sidebar(
              sidebar = sidebar_geral('p1'),
              
              layout_columns(col_widths = c(6,6),
                             value_box(title = 'Total de Respondentes', value = textOutput('vb_n'),         showcase = bsicons::bs_icon('person-fill')),
                             value_box(title = 'Período de Aplicação',    value = textOutput('vb_data_range'), showcase = bsicons::bs_icon('calendar'))
              ),
              
              layout_columns(col_widths = c(4,4,4),
                             card(card_header('Respondentes por Gênero'),            plotlyOutput('g_genero',     height = '340px')),
                             card(card_header('Respondentes por Orientação Sexual'), plotlyOutput('g_orientacao', height = '340px')),
                             card(card_header('Respondentes por Faixa Etária'),      plotlyOutput('g_idade',      height = '340px'))
              ),
              layout_columns(col_widths = c(4,4,4),
                             card(card_header('Respondentes por Raça / Cor'),   plotlyOutput('g_raca',   height = '320px')),
                             card(card_header('Respondentes por Escolaridade'), plotlyOutput('g_escola', height = '320px')),
                             card(card_header('Respondentes por Renda Mensal'), plotlyOutput('g_renda',  height = '320px'))
              ),
              layout_columns(col_widths = c(4,4,4),
                             card(card_header('Distribuição de Gênero por Faixa Etária'),            plotlyOutput('g_gen_idade',  height = '340px')),
                             card(card_header('Distribuição de Orientação Sexual por Faixa Etária'), plotlyOutput('g_ori_idade',  height = '340px')),
                             card(card_header('Distribuição de Raça/Cor por Faixa Etária'),          plotlyOutput('g_raca_idade', height = '340px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Distribuição de Renda por Faixa Etária'), plotlyOutput('g_renda_idade', height = '360px')),
                             card(card_header('Respondentes por Situação de Trabalho'), plotlyOutput('g_trabalho',    height = '360px'))
              ),
              layout_columns(col_widths = c(8,4),
                             card(card_header('Distribuição dos respondentes por UF'), leafletOutput('mapa_uf',  height = '380px')),
                             card(card_header('Respondentes por localização (Capital x Interior)'),          plotlyOutput('g_capital', height = '380px'))
              )
            )
  ),
  
  # ═══ ABA 1b: SAÚDE ════════════════════════════════════════════════════════
  nav_panel(title = tagList(bsicons::bs_icon('heart-pulse'), ' Saúde'), value = 'ps',
            layout_sidebar(
              sidebar = sidebar_geral('ps'),
              
              layout_columns(col_widths = c(4,4,4),
                             value_box(title = '% com Terapia',  value = textOutput('vb_ter'), showcase = bsicons::bs_icon('heart-pulse')),
                             value_box(title = '% Neurodivergentes',    value = textOutput('vb_nd'),  showcase = bsicons::bs_icon('lightning')),
                             value_box(title = '% PcD',          value = textOutput('vb_pcd'), showcase = bsicons::bs_icon('universal-access'))
              ),
              
              card(card_header('Respostas a quesitos de saúde por Faixa Etária'),
                   plotlyOutput('g_saude_idade', height = '380px')),
              
              layout_columns(col_widths = c(6,6),
                             card(card_header('Respostas a quesitos de saúde por Gênero'),   DTOutput('t_saude_gen')),
                             card(card_header('Respostas a quesitos de saúde por Raça/Cor'), DTOutput('t_saude_raca'))
              ),
              
              card(card_header('Respostas a quesitos de saúde por Faixa de Renda'),
                   plotlyOutput('g_saude_renda', height = '380px'))
            )
  ),
  
  # ═══ ABA 2: IDENTIDADE ════════════════════════════════════════════════════
  nav_panel(title = tagList(bsicons::bs_icon('shield-fill'), ' Identidade'), value = 'p2',
            layout_sidebar(
              sidebar = sidebar_geral('p2',
                                      hr(style = 'border-color:#444; margin:10px 0;'),
                                      h6('Filtros específicos', style = 'color:#BA9D7A; font-size:0.8rem; margin-bottom:6px;'),
                                      selectizeInput('p2_ide', 'Autoidentificação', opcoes_ide, multiple = TRUE,
                                                     options = list(placeholder = 'Todas', plugins = list('remove_button')))
              ),
              
              layout_columns(col_widths = c(5,7),
                             card(card_header('Autoidentificação'),        plotlyOutput('g_identidade', height = '380px')),
                             card(card_header('Cruzamento de autoidentificações'), plotOutput('g_venn', height = '380px'))
              ),
              layout_columns(col_widths = c(6, 6),
                             card(card_header('Autoidentificação por Faixa Etária'),
                                  plotlyOutput('g_id_idade',  height = '360px')),
                             card(card_header('Autoidentificação por Faixa de Renda'),
                                  plotlyOutput('g_id_renda', height = '360px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Relação com BDSM/Fetichismo'),              plotlyOutput('g_relacao',     height = '340px')),
                             card(card_header('Relação com BDSM/Fetichismo por Faixa Etária'),            plotlyOutput('g_rel_idade',   height = '340px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Relação com BDSM/Fetichismo por Gênero'),                  plotlyOutput('g_rel_gen',     height = '320px')),
                             card(card_header('Relação com BDSM/Fetichismo por Orientação Sexual'),       plotlyOutput('g_rel_ori',     height = '320px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Papéis e Posições'),                           plotlyOutput('g_posicoes',    height = '380px')),
                             card(card_header('Posições por Gênero'),                 plotlyOutput('g_pos_genero',  height = '380px'))
              ),
              layout_columns(col_widths = c(4,4,4),
                             card(card_header('Tempo que conhece Fetichismo'), plotlyOutput('g_t_fet',  height = '300px')),
                             card(card_header('Tempo que conhece BDSM'),       plotlyOutput('g_t_bdsm', height = '300px')),
                             card(card_header('Tempo que conhece Sadomasoquismo'),  plotlyOutput('g_t_sado', height = '300px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Posições por Faixa Etária'),  plotlyOutput('g_pos_idade',  height = '360px')),
                             card(card_header('Posições por Faixa de Renda'), plotlyOutput('g_pos_renda', height = '360px'))
              )
            )
  ),
  
  # ═══ ABA 3: FETICHES ══════════════════════════════════════════════════════
  nav_panel(title = tagList(bsicons::bs_icon('fire'), ' Fetiches'), value = 'p3',
            layout_sidebar(
              sidebar = sidebar_geral('p3',
                                      hr(style = 'border-color:#444; margin:10px 0;'),
                                      radioButtons('p3_modo', 'Exibir no dumbbell',
                                                   choices  = c('Interesse' = 'interesse', 'Prática' = 'pratica', 'Ambos' = 'ambos'),
                                                   selected = 'ambos', inline = TRUE)
              ),
              
              # Top-10 reativo (ponto 11)
              layout_columns(col_widths = c(6, 6),
                             card(card_header('Top 10 Fetiches de Interesse da Comunidade'),
                                  plotlyOutput('g_top10', height = '320px')),
                             conditionalPanel(
                               condition = "input.p3_gen.length > 0 || input.p3_ori.length > 0 ||
                 input.p3_ida.length > 0 || input.p3_uf.length > 0  ||
                 input.p3_ci.length  > 0 || input.p3_ren.length > 0 ||
                 input.p3_pap.length > 0",
                               card(card_header('Top 10 Global vs. Filtrado'),
                                    DTOutput('t_top10_comparativo', height = '320px'))
                             )
              ),
              
              card(card_header('Taxa de Prática entre Interessados (% que já praticou o fetiche de interesse)'),
                   plotlyOutput('g_taxa_pratica', height = '480px')),
              
              card(
                card_header('Interesse × Prática por Fetiche'),
                plotlyOutput('g_dumbbell', height = '520px')
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Top 10 fetiches de maior interesse de quem nunca os praticou'),  plotlyOutput('g_so_interesse', height = '340px')),
                             card(card_header('Top 10 fetiches mais praticados sem haver interesse explícito'), plotlyOutput('g_so_pratica',  height = '340px'))
              )
            )
  ),
  
  # ═══ ABA 4: TRAJETÓRIA ════════════════════════════════════════════════════
  nav_panel(title = tagList(bsicons::bs_icon('graph-up'), ' Trajetória'), value = 'p4',
            layout_sidebar(
              sidebar = sidebar_geral('p4'),
              
              card(card_header('Como descobriu o fetichismo e o BDSM'),
                   plotlyOutput('g_desc_descoberta', height = '400px')),
              layout_columns(col_widths = c(4,4,4),
                             card(card_header('Tempo de estudo'),         plotlyOutput('g_t_estudo',  height = '300px')),
                             card(card_header('Tempo de prática'),        plotlyOutput('g_t_pratica', height = '300px')),
                             card(card_header('Tempo de participação em eventos'),   plotlyOutput('g_t_eventos', height = '300px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Tempo de BDSM × Tempo de Fetichismo'), plotlyOutput('g_cruzado_bdsm_fet', height = '380px')),
                             card(card_header('Tempo de estudo × Tempo de prática'),  plotlyOutput('g_cruzado_est_prat', height = '380px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Fontes de Informação sobre Fetichismo/BDSM'),         plotlyOutput('g_fontes',     height = '340px')),
                             card(card_header('Relação com Eventos BDSM'),     plotlyOutput('g_rel_eventos', height = '340px'))
              ),
              
              # Primeira experiência (ponto 12)
              h5('Primeira Experiência', style = 'color:#BA9D7A; margin:20px 0 8px;'),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Faixa etária à época da 1ª experiência fetichista'), plotlyOutput('g_faixa_exp_fet',  height = '320px')),
                             card(card_header('Faixa etária à época da 1ª experiência BDSM'),       plotlyOutput('g_faixa_exp_bdsm', height = '320px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Com quem foi realizada a 1ª experiência fetichista'), plotlyOutput('g_origem_exp_fet',  height = '300px')),
                             card(card_header('Com quem foi realizada a 1ª experiência BDSM'),       plotlyOutput('g_origem_exp_bdsm', height = '300px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Ordem de ocorrência entre Fetichismo e BDSM'), plotlyOutput('g_status_exp', height = '300px')),
                             card(card_header('Ano estimado da 1ª experiência'),           plotlyOutput('g_ano_exp',    height = '300px'))
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Faixas etárias atual x da 1ª experiência fetichista'),
                                  plotlyOutput('g_cruzado_exp_fet_idade',  height = '380px')),
                             card(card_header('Faixas etárias atual x da 1ª experiência BDSM'),
                                  plotlyOutput('g_cruzado_exp_bdsm_idade', height = '380px'))
              )
            )
  ),
  
  # ═══ ABA 5: VOZES ═════════════════════════════════════════════════════════
  nav_panel(title = tagList(bsicons::bs_icon('chat-quote-fill'), ' Vozes'), value = 'p5',
            layout_sidebar(
              sidebar = sidebar_geral('p5',
                                      hr(style = 'border-color:#444; margin:10px 0;'),
                                      h6('Filtros específicos', style = 'color:#BA9D7A; font-size:0.8rem; margin-bottom:6px;'),
                                      radioButtons('p5_def', 'Definição de...',
                                                   choices = c('Sadomasoquista' = 'var20', 'Fetichista' = 'var21',
                                                               'Baunilha' = 'var22', 'Praticante de BDSM' = 'var23'),
                                                   selected = 'var20')
              ),
              
              layout_columns(col_widths = c(5,7),
                             uiOutput('card_wc'),
                             uiOutput('card_trigramas')
              ),
              layout_columns(col_widths = c(6,6),
                             card(card_header('Fontes e referências mais nomeadas'),   plotlyOutput('g_fontes_nom',  height = '340px')),
                             card(card_header('Eventos mais conhecidos/frequentados'), plotlyOutput('g_eventos_nom', height = '340px'))
              ),
              card(
                card_header('Respostas abertas — navegação individual'),
                layout_columns(col_widths = c(3,9),
                               radioButtons('p5_aberta', 'Pergunta',
                                            choices = c('Definições: Sadomasoquista'  = 'var20',
                                                        'Definições: Fetichista'      = 'var21',
                                                        'Definições: Baunilha'        = 'var22',
                                                        'Definições: Praticante BDSM' = 'var23',
                                                        '1ª Experiência: Fetichista'  = 'var32',
                                                        '1ª Experiência: BDSM'        = 'var33',
                                                        'Relação com a comunidade'    = 'var34'),
                                            selected = 'var34'),
                               DTOutput('tbl_abertas')
                )
              )
            )
  ),
  
  # ── Rodapé ─────────────────────────────────────────────────────────────────
  nav_spacer(),
  nav_item(tags$span(
    style = 'color:#756161; font-size:0.78rem; padding-right:12px;',
    paste0('n = ', N_TOTAL, ' respondentes · CRCetim · 2025')
  ))
)