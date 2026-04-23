# preprocessing.R
# Censo Fetichista — CRCetim

# ── 1. LEITURA ─────────────────────────────────────────────────────────────────

setwd('C:\\Users\\User\\OneDrive\\Pesquisa\\00_MeusProjetos\\CensoFetichista')

carregar_base <- function(caminho = 'dados/CensoFetichista_BaseBruta.xlsx') {
  wb   <- wb_load(caminho)
  base <- as.data.table(wb_read(wb, sheet = 'Dados'))
  dic  <- as.data.table(wb_read(wb, sheet = 'Dicionario'))
  list(base = base, dicionario = dic)
}

# ── 2. NORMALIZAÇÃO DEMOGRÁFICA ────────────────────────────────────────────────

normalizar_genero <- function(x) {
  x |> tolower() |>
    str_replace_all('\\bmas diria que agênero\\b', 'agênero') |>
    str_replace_all('\\btransmasculino\\b|\\btransfeminina\\b|\\bsou uma mulher trans sem hormônios\\b', 'trans m/f') |>
    str_replace_all('\\bgênero fluido\\b|\\bgênero fluído\\b', 'gênero fluido')
}

normalizar_orientacao <- function(x) {
  x |> tolower() |>
    str_replace_all('\\bhétero demissexual\\b|\\bdemissexual\\b|\\bdemisexual\\b', 'assexual') |>
    str_replace_all('\\bheteossexual\\b', 'heterossexual') |>
    str_replace_all('\\bpolisexual\\b', 'pansexual') |>
    str_replace_all('\\bmas interajo com homens semnproblema\\.\\b|\\bhétero\\-flexível\\b', 'bissexual') |>
    str_replace_all('\\bbi curious\\b|\\bbi\\+pragay\\b', 'bissexual')
}

normalizar_trabalho <- function(x) {
  x |> tolower() |>
    str_replace_all('\\bestudante.*?\\b|\\bmestrando\\b|\\bbolsista.*?\\b', 'estudante') |>
    str_replace_all('\\bmicroempresa\\b', 'empreendedor (mei)') |>
    str_replace_all('\\bmoro com os meus pais\\b', 'não remunerado') |>
    str_replace_all('\\bempresária joalheira\\b|\\bempresário\\b|\\bpj \\- ltda\\b|\\bsócio\\b', 'empregador') |>
    str_replace_all('\\bservidor publico.*?\\b|\\bárea administrativa\\b|\\bexecutivo\\b|\\bflex \\(clt \\+ mei\\)\\b', 'empregado clt') |>
    str_replace_all('\\bbaba\\b', 'trabalhador doméstico')
}

# ── 3. CATEGORIAS SIMPLIFICADAS PARA FILTROS ──────────────────────────────────

simplificar_genero <- function(g_norm) {
  gs       <- tolower(trimws(unlist(strsplit(g_norm, ',\\s*'))))
  especiais <- c('não binário', 'agênero', 'bigênero', 'gênero fluido', 'não-binário')
  mapeados  <- c('feminino', 'masculino', especiais)
  cats <- unique(c(
    if (any(grepl('feminino',  gs, fixed = TRUE))) 'Feminino',
    if (any(grepl('masculino', gs, fixed = TRUE))) 'Masculino',
    if (any(gs %in% especiais))                    'Não-binário'
  ))
  if (length(cats) == 0 || any(!gs %in% mapeados)) cats <- unique(c(cats, 'Outros'))
  cats
}

simplificar_orientacao <- function(o_norm) {
  os       <- tolower(trimws(unlist(strsplit(o_norm, ',\\s*'))))
  mapeados <- c('heterossexual', 'bissexual', 'pansexual',
                'gay', 'lésbica', 'lesbica', 'homossexual', 'assexual')
  cats <- unique(c(
    if (any(grepl('heterossexual',           os))) 'Heterossexual',
    if (any(grepl('bissexual',               os))) 'Bissexual',
    if (any(grepl('pansexual',               os))) 'Pansexual',
    if (any(grepl('gay|lésbica|homossexual', os))) 'Homossexual',
    if (any(grepl('assexual',               os))) 'Assexual'
  ))
  if (length(cats) == 0 || any(!os %in% mapeados)) cats <- unique(c(cats, 'Outros'))
  cats
}

# ── 4. EXTRAÇÃO DE PRIMEIRA EXPERIÊNCIA ───────────────────────────────────────

numeros_pt <- c('dois','tres','quatro','cinco','seis','sete','oito','nove','dez',
                'onze','doze','treze','quatorze','catorze','quinze','dezesseis',
                'dezessete','dezoito','dezenove','vinte','trinta','quarenta','cinquenta')
regex_num_pt <- paste0('\\b(', paste(numeros_pt, collapse = '|'), ')\\b')

extrair_idade_exp <- function(texto) {
  if (is.na(texto) || !nzchar(trimws(texto))) return(NA_real_)
  t <- tolower(texto)
  if (str_detect(t, 'atras|mais de|menos de|mais ou menos|\\bha\\b|ha uns|tem uns|faz uns'))
    return(NA_real_)
  n <- str_extract(texto, '(?i)\\b(?:aos|com)?\\s*(\\d{1,2})\\s*anos?\\b')
  if (!is.na(n)) return(as.numeric(str_extract(n, '\\d{1,2}')))
  e <- str_extract(t, paste0('\\b(?:aos|com)?\\s*', regex_num_pt, '\\s*anos?\\b'))
  if (!is.na(e)) {
    num_word <- str_remove_all(e, '\\b(aos|com|anos?)\\b|\\s')
    idx <- match(num_word, numeros_pt)
    if (!is.na(idx)) return(as.numeric(idx + 3L))
  }
  NA_real_
}

extrair_ano_exp <- function(texto) {
  if (is.na(texto) || !nzchar(trimws(texto))) return(NA_real_)
  if (str_detect(tolower(texto), 'ano passado')) return(2024)
  m <- str_extract(texto, '\\b(19[7-9][0-9]|20[0-4][0-9])\\b')
  if (!is.na(m)) return(as.numeric(m))
  NA_real_
}

classificar_origem_exp <- function(texto) {
  if (is.na(texto) || !nzchar(trimws(texto))) return(NA_character_)
  t <- tolower(texto)
  fcase(
    grepl('namorad|companhei|parceir|casad|esposa|esposo|marido',  t), 'Parceiro/a',
    grepl('amig|conhecid',                                         t), 'Amigo/a',
    grepl('festa|evento|swing|munch',                              t), 'Evento',
    grepl('gp|prodomme|dominador|programa',                        t), 'Profissional',
    grepl('sozinho|internet|redes|porn|chat',                      t), 'Autodescoberta',
    default = NA_character_
  )
}

faixa_idade_exp <- function(x) {
  fcase(x < 13, '< 13 anos', x < 18, '13–17 anos',
        x < 25, '18–24 anos', x < 30, '25–29 anos',
        !is.na(x), '30+ anos', default = NA_character_)
}

# ── 5. CONSTANTES DE VALORES VÁLIDOS ──────────────────────────────────────────

IDENTIDADES_VALIDAS <- c('Fetichista', 'Praticante de BDSM', 'Sadomasoquista')

POSICOES_VALIDAS <- c('Bottom', 'Brat', 'Dominante', 'Masoquista',
                      'Sádico', 'Submisso', 'Switcher', 'Tamer', 'Top')

FETICHES_VALIDOS <- c('Podolatria','Age Play','Humilhação','Cuckolding','Edge Play',
                      'FinDom','Fisting','CNC','Fluidos Corporais','Chuvas',
                      'Impact Play','Needle Play','Castidade','Bondage','Shibari',
                      'Forced Bi','Tease & Denial','Wax Play','Pelos Corporais',
                      'Crossdressing','Pet Play','Voyeurismo','Exibicionismo',
                      'Anime/Hentai','Anal Play','Látex')

FONTES_VALIDAS <- c('Redes Sociais','Sites e fóruns','Grupos Online','Podcasts',
                    'Não me mantenho informado','Grupos de estudo',
                    'Artigos científicos e/ou Livros','Revistas especializadas',
                    'Cursos (presenciais ou online)')

# ── 6. HELPERS DE MÚLTIPLA ESCOLHA ────────────────────────────────────────────

padronizar_multipla <- function(x, validos, delim = ',\\s*') {
  lapply(strsplit(x, delim), function(itens) {
    itens <- trimws(itens)
    itens[!itens %in% validos] <- 'Outros'
    unique(itens)
  })
}

calcular_fetiches_pareados <- function(dt) {
  dt[, interesse  := lapply(strsplit(var25, ',\\s*'), function(x) trimws(x)[trimws(x) %in% FETICHES_VALIDOS])]
  dt[, praticado  := lapply(strsplit(var26, ',\\s*'), function(x) trimws(x)[trimws(x) %in% FETICHES_VALIDOS])]
  dt[, ambos      := mapply(intersect, interesse, praticado)]
  dt[, so_interes := mapply(setdiff,   interesse, praticado)]
  dt[, so_pratica := mapply(setdiff,   praticado, interesse)]
  mk <- function(col, nm) {
    r <- dt[, .(var45, v = get(col))][, .(fetiche = unlist(v)), by = var45][, .N, by = fetiche]
    setnames(r, 'N', nm)
  }
  res <- Reduce(function(a, b) merge(a, b, by = 'fetiche', all = TRUE),
                list(mk('ambos','n_ambos'), mk('so_interes','n_so_interesse'), mk('so_pratica','n_so_pratica')))
  setnafill(res, fill = 0L, cols = c('n_ambos','n_so_interesse','n_so_pratica'))
  res[, n_interesse := n_ambos + n_so_interesse]
  res[, n_pratica   := n_ambos + n_so_pratica]
  res[order(-n_interesse)]
}

# ── 7. RESOLUÇÃO DE ENTIDADES NOMEADAS ────────────────────────────────────────

resolver_entidades <- function(dt, var, mapa) {
  dt[, txt := stri_trans_general(tolower(get(var)), 'Latin-ASCII')]
  dt[, txt := gsub('[^\\w\\s,;]', ' ', txt, perl = TRUE)]
  dt[, tks := strsplit(trimws(gsub('\\s+', ' ', txt)), '\\s*[,;]\\s*|\\s+e\\s+')]
  tok <- data.table(id = rep(seq_len(nrow(dt)), sapply(dt$tks, length)),
                    token = unlist(dt$tks), alvo = NA_character_)
  map_dt <- rbindlist(lapply(names(mapa), function(n)
    data.table(nome = n, variante = mapa[[n]]$variants, prioridade = mapa[[n]]$priority)
  ))[order(prioridade)]
  for (i in seq_len(nrow(map_dt))) {
    rx <- paste0('\\b', gsub('([.|()\\^{}+$*?\\[\\]])', '\\\\\\1', map_dt$variante[i]), '\\b')
    tok[grepl(rx, token, ignore.case = TRUE) & is.na(alvo), alvo := map_dt$nome[i]]
  }
  dt[, c('txt','tks') := NULL]
  tok[!is.na(alvo), .N, by = alvo][order(-N)]
}

# ── 8. PIPELINE PRINCIPAL ──────────────────────────────────────────────────────

preparar_base <- function(caminho = 'dados/CensoFetichista_BaseBruta.xlsx') {
  
  message('[ 1/7 ] Carregando...')
  lst  <- carregar_base(caminho)
  base <- lst$base[order(var44)]
  
  base[var29 == '6-10 anos', var29 := '6-9 anos']
  base[var30 == '6-10 anos', var30 := '6-9 anos']
  base[var31 == '6-10 anos', var31 := '6-9 anos']
  
  message('[ 2/7 ] Timestamp...')
  if (is.numeric(base$var44)) {
    base[, var44 := as.POSIXct(var44 * 86400, origin = '1899-12-30', tz = 'America/Sao_Paulo')]
  } else {
    base[, var44 := as.POSIXct(var44, tz = 'America/Sao_Paulo')]
  }
  base[, data_resposta := as.Date(var44)]
  
  message('[ 3/7 ] Demografias...')
  base[, genero     := normalizar_genero(var2)]
  base[, orientacao := normalizar_orientacao(var3)]
  base[, trabalho   := normalizar_trabalho(var9)]
  
  base[, genero_simples     := lapply(genero,     simplificar_genero)]
  base[, orientacao_simples := lapply(orientacao, simplificar_orientacao)]
  
  niveis_renda <- c('Não tenho renda','Até R$ 1.500','De R$ 1.500 a R$ 2.000',
                    'De R$ 2.000 a R$ 3.000','De R$ 3.000 a R$ 5.000',
                    'De R$ 5.000 a R$ 10.000','De R$ 10.000 a R$ 15.000',
                    'De R$ 15.000 a R$ 20.000','Acima de R$ 20.000')
  niveis_idade <- c('18 a 24 anos','25 a 29 anos','30 a 34 anos','35 a 39 anos',
                    '40 a 44 anos','45 a 49 anos','50 a 54 anos','55 a 59 anos',
                    '60 a 64 anos','65+ anos')
  NIVEIS_RENDA_CAT <- c('Não tenho renda','Até R$ 1.500','De R$ 1.500 a R$ 2.000',
                        'De R$ 2.000 a R$ 3.000','De R$ 3.000 a R$ 5.000',
                        'De R$ 5.000 a R$ 10.000','De R$ 10.000 a R$ 15.000',
                        'R$ 15.000 ou mais')
  
  base[, renda_f        := factor(var10, levels = niveis_renda, ordered = TRUE)]
  base[, renda_cat := fcase(
    var10 %in% c('De R$ 15.000 a R$ 20.000','Acima de R$ 20.000'), 'R$ 15.000 ou mais',
    !is.na(var10), var10,
    default = NA_character_
  )]
  base[, renda_cat_f := factor(renda_cat, levels = NIVEIS_RENDA_CAT, ordered = TRUE)]
  base[, faixa_etaria_f := factor(var4,  levels = niveis_idade, ordered = TRUE)]
  base[, faixa_agr := agrupar_faixa_etaria(var4)]
  base[, genero_cat := sapply(genero_simples, function(x) {
    cats <- x[x %in% c('Feminino','Masculino','Não-binário')]
    if (!length(cats)) 'Outros' else cats[1]
  })]
  
  base[, orientacao_cat := sapply(orientacao_simples, function(x) {
    cats <- x[x %in% c('Heterossexual','Bissexual','Pansexual','Homossexual','Assexual')]
    if (!length(cats)) 'Outros' else cats[1]
  })]
  
  
  message('[ 4/7 ] Múltipla escolha...')
  base[, identidade_lista := padronizar_multipla(var15, IDENTIDADES_VALIDAS)]
  base[, posicao_lista    := padronizar_multipla(var24, POSICOES_VALIDAS)]
  base[, fonte_lista      := padronizar_multipla(
    str_replace_all(var35,
                    c('Redes Sociais \\(Instagram, TikTok, Youtube, etc\\.\\)' = 'Redes Sociais',
                      'Grupos \\(WhatsApp, Telegram, etc\\.\\)'                = 'Grupos Online')),
    FONTES_VALIDAS)]
  
  base[, flag_dom    := sapply(posicao_lista, function(x) any(x %in% c('Top','Dominante','Tamer','Sádico')))]
  base[, flag_sub    := sapply(posicao_lista, function(x) any(x %in% c('Bottom','Submisso','Brat','Masoquista')))]
  base[, flag_switch := sapply(posicao_lista, function(x) 'Switcher' %in% x)]
  
  message('[ 5/7 ] Fetiches...')
  base[, interesse := lapply(strsplit(var25, ',\\s*'), function(x) trimws(x)[trimws(x) %in% FETICHES_VALIDOS])]
  base[, praticado := lapply(strsplit(var26, ',\\s*'), function(x) trimws(x)[trimws(x) %in% FETICHES_VALIDOS])]
  
  message('[ 6/7 ] Grupos...')
  base[, grupo_renda := fcase(
    var10 %in% c('Não tenho renda','Até R$ 1.500','De R$ 1.500 a R$ 2.000','De R$ 2.000 a R$ 3.000'), 'Baixa',
    var10 %in% c('De R$ 3.000 a R$ 5.000','De R$ 5.000 a R$ 10.000'), 'Média',
    var10 %in% c('De R$ 10.000 a R$ 15.000','De R$ 15.000 a R$ 20.000','Acima de R$ 20.000'), 'Alta',
    default = NA_character_)]
  base[, grupo_idade := fcase(
    var4 %in% c('18 a 24 anos','25 a 29 anos'), '18–29',
    var4 %in% c('30 a 34 anos','35 a 39 anos'), '30–39',
    var4 %in% c('40 a 44 anos','45 a 49 anos','50 a 54 anos','55 a 59 anos','60 a 64 anos','65+ anos'), '40+',
    default = NA_character_)]
  
  message('[ 7/7 ] Primeira experiência...')
  base[, `:=`(
    idade_exp_fet   = sapply(var32, extrair_idade_exp),
    ano_exp_fet     = sapply(var32, extrair_ano_exp),
    origem_exp_fet  = sapply(var32, classificar_origem_exp),
    idade_exp_bdsm  = sapply(var33, extrair_idade_exp),
    ano_exp_bdsm    = sapply(var33, extrair_ano_exp),
    origem_exp_bdsm = sapply(var33, classificar_origem_exp)
  )]
  base[, faixa_exp_fet  := faixa_idade_exp(idade_exp_fet)]
  base[, faixa_exp_bdsm := faixa_idade_exp(idade_exp_bdsm)]
  base[!is.na(ano_exp_fet) & !is.na(ano_exp_bdsm),
       status_exp := fcase(
         ano_exp_fet < ano_exp_bdsm,  'Fetichismo primeiro',
         ano_exp_fet > ano_exp_bdsm,  'BDSM primeiro',
         ano_exp_fet == ano_exp_bdsm, 'Mesmo ano',
         default = NA_character_)]
  
  message('Concluído: ', nrow(base), ' respondentes.')
  list(base = base, dicionario = lst$dicionario,
       resumo_fetiches = calcular_fetiches_pareados(copy(base)))
}

# ── 9. FILTRO UNIVERSAL ────────────────────────────────────────────────────────

agrupar_faixa_etaria <- function(x) {
  fcase(
    x == '18 a 24 anos', '18-24',
    x == '25 a 29 anos', '25-29',
    x == '30 a 34 anos', '30-34',
    x == '35 a 39 anos', '35-39',
    x == '40 a 44 anos', '40-44',
    x == '45 a 49 anos', '45-49',
    x %in% c('50 a 54 anos','55 a 59 anos','60 a 64 anos','65+ anos'), '50+',
    default = NA_character_
  )
}

filtrar_base <- function(base,
                         generos      = NULL,
                         orientacoes  = NULL,
                         idades       = NULL,
                         ufs          = NULL,
                         capital_int  = NULL,
                         grupos_renda = NULL,
                         papeis       = NULL,
                         identidades  = NULL) {
  dt <- base
  if (!is.null(generos))      dt <- dt[sapply(genero_simples,     function(x) any(x %in% generos))]
  if (!is.null(orientacoes))  dt <- dt[sapply(orientacao_simples, function(x) any(x %in% orientacoes))]
  if (!is.null(idades))       dt <- dt[var4 %in% idades]
  if (!is.null(ufs))          dt <- dt[var6 %in% ufs]
  if (!is.null(capital_int))  dt <- dt[var7 %in% capital_int]
  if (!is.null(grupos_renda)) dt <- dt[renda_cat %in% grupos_renda]
  if (!is.null(papeis))       dt <- dt[sapply(posicao_lista,   function(x) any(x %in% papeis))]
  if (!is.null(identidades))  dt <- dt[sapply(identidade_lista, function(x) any(x %in% identidades))]
  dt
}

# ── 10. HELPER DE CONTAGEM ────────────────────────────────────────────────────

contar_texto <- function(dt, col, delim = ',\\s*', excluir = c('', NA)) {
  vals <- trimws(unlist(strsplit(dt[[col]], delim)))
  vals <- vals[!vals %in% excluir]
  out  <- as.data.table(sort(table(vals), decreasing = TRUE))
  setnames(out, c('categoria', 'n'))
  out
}