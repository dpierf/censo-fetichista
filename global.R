# global.R
# Censo Fetichista — CRCetim

pkgs <- c('data.table', 'stringr', 'stringi', 'textclean', 'stopwords', 'ggvenn',
          'openxlsx2', 'shiny', 'bslib', 'plotly', 'leaflet', 'wordcloud2', 'DT',
          'ggplot2', 'ggVennDiagram', 'geobr', 'scales', 'bsicons', 'tidyr')

pkgs_faltando <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(pkgs_faltando)) install.packages(pkgs_faltando, quiet = TRUE)

suppressPackageStartupMessages({
  require(data.table) 
  require(stringr)
  require(stringi)
  require(textclean)  
  require(tidyr)
  require(stopwords)
  require(openxlsx2)
  require(shiny)      
  require(bslib)
  require(bsicons)
  require(plotly)     
  require(leaflet)
  require(sf)
  require(scales)
  require(wordcloud2) 
  require(DT)
  require(ggplot2)    
  require(ggVennDiagram)
  require(geobr)
})

source('preprocessing.R', encoding = 'UTF-8')

dados           <- preparar_base(caminho = 'dados/CensoFetichista_BaseBruta.xlsx')
base            <- dados$base
dicionario      <- dados$dicionario
resumo_fetiches <- dados$resumo_fetiches
rm(dados)

# ── CONSTANTES GLOBAIS ────────────────────────────────────────────────────────

N_TOTAL <- nrow(base)

NIVEIS_IDADE <- c('18 a 24 anos','25 a 29 anos','30 a 34 anos','35 a 39 anos',
                  '40 a 44 anos','45 a 49 anos','50 a 54 anos','55 a 59 anos',
                  '60 a 64 anos','65+ anos')
NIVEIS_IDADE_AGR <- c('18-24','25-29','30-34','35-39','40-44','45-49','50+')
NIVEIS_RENDA <- c('Não tenho renda','Até R$ 1.500','De R$ 1.500 a R$ 2.000',
                  'De R$ 2.000 a R$ 3.000','De R$ 3.000 a R$ 5.000',
                  'De R$ 5.000 a R$ 10.000','De R$ 10.000 a R$ 15.000',
                  'De R$ 15.000 a R$ 20.000','Acima de R$ 20.000')
NIVEIS_ESCOLA <- c('Ensino Fundamental','Ensino Médio','Ensino Superior',
                   'Pós-Graduação (MBA, Especialização)',
                   'Pós-Graduação (Mestrado, Doutorado)')
NIVEIS_TEMPO  <- c('Menos de 1 ano','1-2 anos','3-5 anos',
                   '6-9 anos','10+ anos','Não se Aplica')
NIVEIS_FAIXA_EXP <- c('< 13 anos','13–17 anos','18–24 anos','25–29 anos','30+ anos')
NIVEIS_RENDA_CAT <- c('Não tenho renda','Até R$ 1.500','De R$ 1.500 a R$ 2.000',
                      'De R$ 2.000 a R$ 3.000','De R$ 3.000 a R$ 5.000',
                      'De R$ 5.000 a R$ 10.000','De R$ 10.000 a R$ 15.000',
                      'R$ 15.000 ou mais')

LABELS_DEF <- c(
  var20 = 'Sadomasoquista',
  var21 = 'Fetichista',
  var22 = 'Baunilha',
  var23 = 'Praticante de BDSM'
)

ROTULOS_RENDA <- c(
  'Não tenho renda'          = 'Sem renda',
  'Até R$ 1.500'             = '< 1500',
  'De R$ 1.500 a R$ 2.000'   = 'R$ 1500 - 2000',
  'De R$ 2.000 a R$ 3.000'   = 'R$ 2000 - 3000',
  'De R$ 3.000 a R$ 5.000'   = 'R$ 3000 - 5000',
  'De R$ 5.000 a R$ 10.000'  = 'R$ 5000 - 10000',
  'De R$ 10.000 a R$ 15.000' = 'R$ 10000 - 15000',
  'R$ 15.000 ou mais'        = 'R$ 15000+'
)

ROTULOS_TEMPO <- c(
  'Menos de 1 ano' = '0-1',
  '1-2 anos'       = '1-2',
  '3-5 anos'       = '3-5',
  '6-9 anos'       = '6-9',
  '10+ anos'       = '10+',
  'Não se Aplica'  = 'N/A'
)

# ── OPÇÕES DE FILTROS ─────────────────────────────────────────────────────────

opcoes_gen   <- c('Feminino','Masculino','Não-binário','Outros')
opcoes_ori   <- c('Heterossexual','Bissexual','Pansexual','Homossexual','Assexual','Outros')
opcoes_ida   <- NIVEIS_IDADE
opcoes_uf    <- sort(unique(base$var6[!is.na(base$var6) & base$var6 != '']))
opcoes_ci    <- c('Capital','Interior')
opcoes_ren   <- NIVEIS_RENDA_CAT
opcoes_pap   <- c('Top','Dominante','Tamer','Sádico','Bottom','Submisso','Brat','Masoquista','Switcher','Outros')
opcoes_ide   <- c('Fetichista','Praticante de BDSM','Sadomasoquista','Outros')

# ── SHAPEFILE DOS ESTADOS ─────────────────────────────────────────────────────

shp_estados <- tryCatch(
  geobr::read_state(year = 2020, showProgress = FALSE),
  error = function(e) { message('Shapefile não carregado: ', e$message); NULL }
)

# ── PALETA E TEMA ─────────────────────────────────────────────────────────────

paleta <- c('#29ABE2','#8DC63F','#F72585','#FFC000','#F7941D',
            '#ED1C24','#2E6DB4','#9B59B6','#1ABC9C','#E67E22')
#paleta <- c('#756161','#BA9D7A','#C1A989','#D4CAA9','#F4E9D3')

COR_POSICAO <- c(
  'Dominante'  = paleta[1], 'Submisso'   = paleta[1],
  'Top'        = paleta[2], 'Bottom'     = paleta[2], 'Switcher' = paleta[2],
  'Sádico'     = paleta[3], 'Masoquista' = paleta[3],
  'Tamer'      = paleta[4], 'Brat'       = paleta[4],
  'Outros'     = paleta[5]
)

POSICOES_ORD <- sort(c('Bottom','Brat','Dominante','Masoquista',
                       'Sádico','Submisso','Switcher','Tamer','Top'))

COR_POSICAO_IND <- c(
  'Bottom'    = '#29ABE2',
  'Brat'      = '#8DC63F',
  'Dominante' = '#F72585',
  'Masoquista'= '#FFC000',
  'Sádico'    = '#F7941D',
  'Submisso'  = '#ED1C24',
  'Switcher'  = '#9B59B6',
  'Tamer'     = '#1ABC9C',
  'Top'       = '#E67E22'
)

COR_IDENTIDADE <- c(
  'Fetichista'         = '#29ABE2',
  'Praticante de BDSM' = '#8DC63F',
  'Sadomasoquista'     = '#F72585',
  'Outros'             = '#888888'
)

# Mistura duas ou mais cores hex por média RGB
blend_cores <- function(...) {
  cores    <- c(...)
  rgb_vals <- sapply(cores, function(c) col2rgb(c))
  media    <- rowMeans(rgb_vals)
  rgb(media[1], media[2], media[3], maxColorValue = 255)
}


COR_RENDA <- c(
  'Não tenho renda'          = '#2E6DB4',
  'Até R$ 1.500'             = '#29ABE2',
  'De R$ 1.500 a R$ 2.000'   = '#8DC63F',
  'De R$ 2.000 a R$ 3.000'   = '#FFC000',
  'De R$ 3.000 a R$ 5.000'   = '#F7941D',
  'De R$ 5.000 a R$ 10.000'  = '#ED1C24',
  'De R$ 10.000 a R$ 15.000' = '#F72585',
  'R$ 15.000 ou mais'        = '#9B59B6'
)

tema_app <- bs_theme(
  version = 5, bg = '#1a1a1a', fg = '#F4E9D3',
  primary = '#BA9D7A', secondary = '#756161',
  base_font = font_google('Inter'), heading_font = font_google('Inter')
)

# ── HELPER: LAYOUT PLOTLY ─────────────────────────────────────────────────────

layout_padrao <- function(p, titulo = '') {
  p |> layout(
    title  = list(text = titulo, font = list(color = '#F4E9D3', size = 13)),
    paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
    font   = list(color = '#D4CAA9', size = 11),
    xaxis  = list(gridcolor = '#333', zerolinecolor = '#444'),
    yaxis  = list(gridcolor = '#333', zerolinecolor = '#444'),
    legend = list(bgcolor = 'rgba(0,0,0,0)', font = list(color = '#D4CAA9'),
                  orientation = 'h', x = 0.5, xanchor = 'center',
                  y = 1.08, yanchor = 'bottom'),
    margin = list(t = 40, r = 20, b = 40, l = 20)
  )
}

# ── HELPER: SIDEBAR GERAL ─────────────────────────────────────────────────────
# Gera os 7 filtros obrigatórios + quaisquer extras específicos da aba.
# O argumento `...` aceita widgets adicionais que ficam abaixo dos filtros gerais.

sidebar_geral <- function(prefix, ..., width = 280) {
  sid <- function(id) paste0(prefix, '_', id)
  ph  <- function(txt) list(placeholder = txt, plugins = list('remove_button'))
  sidebar(
    width = width, class = 'sidebar-content', open = TRUE,
    h6('Filtros', style = 'color:#BA9D7A;font-weight:700;margin-bottom:8px;'),
    selectizeInput(sid('gen'), 'Gênero',              opcoes_gen, multiple = TRUE, options = ph('Todos')),
    selectizeInput(sid('ori'), 'Orientação Sexual',   opcoes_ori, multiple = TRUE, options = ph('Todas')),
    selectizeInput(sid('ida'), 'Faixa Etária',        opcoes_ida, multiple = TRUE, options = ph('Todas')),
    selectizeInput(sid('uf'),  'Estado (UF)',         opcoes_uf,  multiple = TRUE, options = ph('Todos')),
    selectizeInput(sid('ci'),  'Capital / Interior',  opcoes_ci,  multiple = TRUE, options = ph('Ambos')),
    selectizeInput(sid('ren'), 'Grupo de Renda',      opcoes_ren, multiple = TRUE, options = ph('Todos')),
    selectizeInput(sid('pap'), 'Papel / Posição',     opcoes_pap, multiple = TRUE, options = ph('Todos')),
    ...,
    actionButton(sid('rst'), 'Limpar filtros',
                 class = 'btn-outline-secondary btn-sm w-100 mt-2')
  )
}

# ── MAPAS DE ENTIDADES NOMEADAS ───────────────────────────────────────────────

map36 <- list(
  'Chicotadas'       = list(variants = c('chicotadas podcast','chicotadaspodcast','chicotadas','chicotada','chibatadas','chibatada','podcast chicotadas','chicotas','chicotads'), priority = 1),
  'Dom Barbudo'      = list(variants = c('dom barbudo','senhor barbudo','sr barbudo','barbudo','don barbudo','dombarbudo'), priority = 1),
  '@fe.tichista'     = list(variants = c('@fe.tichista','fe bonfim','fe tichista','fernanda bonfim','fe.tichista','a fetichista','fe.fetichista','instagram da fernanda','fe fetichista','fernanda'), priority = 1),
  'FetLife'          = list(variants = c('fetlife','fet life','fet live','fet'), priority = 1),
  'Kink Academy'     = list(variants = c('kink academy','kinkacademy'), priority = 1),
  'Kinkgram'         = list(variants = c('kinkgram','kink gram'), priority = 1),
  'Portal do Play'   = list(variants = c('portal do play','ms mahara','portaldoplay','@portaldoplay','ms.mahara','ms. mahara','mahara','mistress mahara','ms.mahra','portalplay','portao do play','ms mahra'), priority = 1),
  'RumVerso'         = list(variants = c('rumverso','@rumverso','rumcompassas','@rumcompassas'), priority = 1),
  'Senhor Asgard'    = list(variants = c('senhor asgard','asgard','sr asgard'), priority = 1),
  'Senhor Verdugo'   = list(variants = c('dom verdugo','senhor verdugo','sr verdugo','verdugo'), priority = 1),
  'Outros Dom/me'    = list(variants = c('dom','domme','mestre','mistress','cruel','sub','dommes','mestres','domm','mistrees','gladius','hex','ravena','dommcsp','akiranawa','luxor'), priority = 1),
  'Audiovisual'      = list(variants = c('filme','filmes','documentario','documentarios'), priority = 2),
  'Podcasts'         = list(variants = c('podcast','podcasts','pod cast','spotify'), priority = 2),
  'Reddit ou Discord'= list(variants = c('reddit','discord'), priority = 2),
  'Redes Sociais'    = list(variants = c('instagram','twitter','redes socias','medium','tiktok','youtube','forum','foruns','insta','influenciador','facebook','ig','instagran','rede social'), priority = 2),
  'Shibari'          = list(variants = c('shibari house','shibari study','shibari','shibari brasil','shibari studies'), priority = 2),
  'WhatsApp/Telegram'= list(variants = c('telegram','whatsapp','tele gram','whats app','whats','grupo do','grupo da','grupo','wpp'), priority = 2),
  'Comunidade'       = list(variants = c('grupos','amigos','conversas','comunidade','amizades','pessoas','amigues','evento','eventos','parceiro','parceira','parceire'), priority = 3),
  'Estudo'           = list(variants = c('livro','livros','curso','cursos','leituras','sm 101','texto','textos','estudos','pesquisa','artigo','artigos','101','scholar','scielo','pubmed'), priority = 3),
  'Sites em Geral'   = list(variants = c('sites','google','internet','site','blog','blogs'), priority = 4),
  'Não se Aplica'    = list(variants = c('nao se aplica','nao me atualizo','nao pesquiso','nao busco','nenhum','nao uso','nao entro','ninguem'), priority = 5)
)

map38 <- list(
  'Atados'           = list(variants = c('atados','atados no parque','atadas','atadas no parque','arados no parque'), priority = 1),
  'Chicotadas'       = list(variants = c('chicotadas','chicotadasfest','chicotadas fest','chicofest'), priority = 1),
  'Dominatrix'       = list(variants = c('dominatrix','domina','domina bar','dominatrix augusta','dominatrix sp','dominatrixx','dominabar','domminatrix'), priority = 1),
  'Fetish Meet'      = list(variants = c('fetish meet','fetishmeet','fetiche meet'), priority = 1),
  "Let's Play"       = list(variants = c('lets','lets play','letsplay','lets ssc','letsssc','let s play','let play','let s','letsparty'), priority = 1),
  'Love Cabaret'     = list(variants = c('love cabaret','cabaret','love cabare','lovecabaret'), priority = 1),
  'LoveNox'          = list(variants = c('lovenox','love nox'), priority = 1),
  'Lust'             = list(variants = c('lust','festa lust'), priority = 1),
  'Luxuria'          = list(variants = c('luxuria','projeto luxuria','heitor'), priority = 1),
  'Mr/Mrs Fetiche'   = list(variants = c('mr fetiche','mister fetiche','mister miss fetiche','missfetichebrasil','mr ms fetiche','concurso'), priority = 1),
  'Playroom'         = list(variants = c('playroom','play room','play bsb','playroom bsb'), priority = 1),
  'Shibari (evento)' = list(variants = c('shibari'), priority = 1),
  'The Office'       = list(variants = c('the office','office'), priority = 1),
  'YES'              = list(variants = c('yes','yes party','yesparty','yessm','yes sm party'), priority = 1),
  'Aphrodisiac'      = list(variants = c('aphrodisiac','afrodisiac','lunna','lunna queen'), priority = 2),
  'Brutus'           = list(variants = c('brutus','festa brutus','brutus party'), priority = 2),
  'Eventos Leather'  = list(variants = c('jantar leather','leather'), priority = 2),
  'Kinknic'          = list(variants = c('kinknic'), priority = 2),
  'Lotus Irish'      = list(variants = c('irish lotus','lotus irish','bar lotus','lotus'), priority = 2),
  'Noite Freak'      = list(variants = c('noite freak','freak'), priority = 2),
  'Bacharia'         = list(variants = c('bacharia','barcharia','festa da bacha'), priority = 3),
  'Circus'           = list(variants = c('circus','circus roleplay','ravena','doguinho','circusplay'), priority = 3),
  'DarkSide'         = list(variants = c('darkside','dark side'), priority = 3),
  'Débauche'         = list(variants = c('debauche'), priority = 3),
  'Fetichic'         = list(variants = c('fetichic'), priority = 3),
  'FetLab'           = list(variants = c('fetlab','fet lab'), priority = 3),
  'Studio SM'        = list(variants = c('studio sm','sm studio','sm studios','studiosm'), priority = 3),
  'Não se Aplica'    = list(variants = c('nao','nenhum','nenhuma','nunca'), priority = 4)
)