# Censo Fetichista Brasileiro: Dashboard Interativo

Dashboard interativo desenvolvido em R/Shiny para exploração dos dados do **Censo Fetichista Brasileiro**, pesquisa conduzida pelo **Centro de Referência Cetim (CRCetim)** com 729 respondentes.

---

## Sobre o projeto

O Censo Fetichista Brasileiro é a primeira pesquisa de mapeamento sistemático da comunidade fetichista no Brasil. Os dados foram coletados via questionário estruturado e analisados originalmente em relatório publicado em 2025. Este dashboard oferece uma camada de exploração dinâmica e interativa dos mesmos dados, permitindo filtros por perfil sociodemográfico e visualizações cruzadas que o formato estático não comporta.

O dashboard está organizado em seis abas:

- **Demográfico:** perfil de gênero, orientação sexual, faixa etária, raça/cor, escolaridade, renda e geográfico
- **Saúde:** indicadores de PcD, neurodivergência, psicoterapia e medicação, cruzados por perfil
- **Identidade:** autoidentificação, papéis de jogo, relação com BDSM e fetichismo, trajetória de envolvimento
- **Fetiches:** ranking de interesse e prática, análise de aspiração vs. experiência
- **Trajetória:** descoberta, aprendizado, primeiras experiências e relação com eventos
- **Vozes:** análise de respostas abertas, nuvem de palavras, trigramas e fontes nomeadas

---

## Autoria

**Relatório original:**
Bonfim, F. M. A.; Ferreira, G. C.; Canabarro, K.; Maria, P. F. *Censo Fetichista* [livro eletrônico]. Brasília, DF: Ed. dos Autores, 2025. ISBN 978-65-01-58342-6. Disponível em: https://crcetim.org/censofetichista

**Dashboard:**
Maria, P. F. (2026). *Censo Fetichista Brasileiro: Dashboard Interativo*. Disponível em: https://github.com/dpierf/censo-fetichista

---

## Licença e uso

O código e os dados disponibilizados neste repositório são públicos e podem ser reutilizados, desde que:

1. A autoria seja citada conforme o formato acima
2. Modificações no código ou nos dados sejam previamente autorizadas por Pier Francesco De Maria (dpierf@gmail.com) e/ou CRCetim
3. Usos derivados dos dados para outros fins (pesquisas, publicações, visualizações) incluam obrigatoriamente a citação do relatório original com ISBN

Para solicitações de uso, entre em contato:
- **E-mail:** crcetim@gmail.com
- **Instagram:** [@crcetim](https://instagram.com/crcetim)

---

## Como rodar localmente

**Requisitos:**
- R >= 4.5.0
- RStudio (recomendado)

**Passos:**

1. Clone ou baixe este repositório
2. Instale os pacotes necessários rodando no console do R:

```r
install.packages(c(
  'data.table', 'stringr', 'stringi', 'textclean', 'tidyr',
  'stopwords', 'openxlsx2', 'shiny', 'bslib', 'bsicons',
  'plotly', 'leaflet', 'sf', 'wordcloud2', 'DT', 'ggplot2',
  'ggvenn', 'geobr', 'scales', 'rsconnect'
))
```

3. Abra o arquivo `app.R` no RStudio e clique em **Run App**

**Estrutura do repositório:**

```
censo-fetichista/
├── app.R
├── global.R
├── ui.R
├── server.R
├── preprocessing.R
└── dados/
    └── CensoFetichista_BaseBruta.xlsx
```

---

## Desenvolvimento assistido por IA

O desenvolvimento deste dashboard contou com assistência do modelo Claude Sonnet 4.6 (Anthropic, 2025) para geração, revisão e depuração de código R/Shiny. Todo o código foi supervisionado, adaptado e validado pelos autores.

Anthropic. (2025). Claude Sonnet 4.6 [Modelo de linguagem]. https://www.anthropic.com


---

## Stack técnico

| Componente | Tecnologia |
|---|---|
| Linguagem | R >= 4.5.0 |
| Framework web | Shiny + bslib (Bootstrap 5) |
| Visualização | plotly, leaflet, ggvenn, wordcloud2 |
| Dados | data.table, openxlsx2 |
| Geografia | geobr, sf |
| Hospedagem | Posit Connect Cloud |
