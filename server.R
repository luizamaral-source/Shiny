## ---------------------------------------------------------------
## Minicurso: Introdução ao R para Análise de Dados de Imigração
## SEMUNI 2026 · UnB — Aula 3: Visualização de dados em painéis via Shiny
## Script consolidado com todos os códigos usados nos slides
## ---------------------------------------------------------------

## Pré-requisito da aula: o objeto df com os microdados CGIL/OBMigra
## já tratados na Aula 2.


## 0) Preparação da base — CGIL_CNIg_2024.csv -> imigracao_tratada.rds ---
## A base bruta vem com ; como separador e o estado por extenso
## (coluna uf_estrangeiro: "São Paulo", "Paraná", ...); aqui convertemos
## para sigla (uf: "SP", "PR", ...) e salvamos em .rds, que é o que o
## restante do script (readRDS) espera. Se preferir, pule este bloco e
## use direto o data.frame "raw" tratado — funciona igual dentro da sessão,
## só não fica salvo em disco para a próxima vez.

library(dplyr)
library(readr)

raw <- read_delim(
  "dados/CGIL_CNIg_2024.csv",
  delim = ";",
  locale = locale(encoding = "UTF-8", decimal_mark = ","),
  na = c("", "NA")
)

# nome do estado (por extenso) -> sigla
mapa_uf <- c(
  "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
  "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
  "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
  "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
  "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
  "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
  "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
  "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
  "Tocantins" = "TO"
)

df <- raw |>
  transmute(
    pais = pais,
    uf = unname(mapa_uf[uf_estrangeiro]),
    modalidade = modalidade,
    andamento = andamento,
    mes = mes,
    ano = ano,
    genero = genero,
    escolaridade = escolaridade
  )

dir.create("dados", showWarnings = FALSE)
stopifnot(is.data.frame(df))  # trava: evita salvar a função base df() por engano
saveRDS(df, "dados/imigracao_tratada.rds")


## 1) O app mínimo que funciona ---------------------------------------
## (salve este bloco sozinho como app.R para rodar com "Run App")

# app.R
library(shiny)

ui <- fluidPage(
  sliderInput("n", "Observações", min = 10, max = 500, value = 100),
  plotOutput("grafico")
)

server <- function(input, output) {
  output$grafico <- renderPlot({
    hist(rnorm(input$n))
  })
}

shinyApp(ui, server)


## 2) Inputs: os controles da ui ---------------------------------------
## (trechos ilustrativos — mostram a sintaxe de selectInput e sliderInput,
## não formam um app sozinhos)

# selectInput(
#   "pais",
#   "País de nacionalidade",
#   choices = sort(unique(df$pais))
# )
#
# sliderInput(
#   "ano",
#   "Ano de registro",
#   min = 2015,
#   max = 2025,
#   value = c(2019, 2025), sep = ""
# )


## 3) Outputs: pares ui / server ----------------------------------------
## plotOutput()  <-> renderPlot()   -> gráficos ggplot2 e base
## tableOutput() <-> renderTable()  -> tabelas pequenas
## textOutput()  <-> renderText()   -> totais e legendas
## Atenção: o nome entre aspas (ex. "grafico") precisa ser igual nos dois lados.


## 4) reactive(): filtre uma vez, use em vários outputs -------------------

# Evite — filtro repetido em cada output:
# output$grafico <- renderPlot({
#   d <- filter(df, pais == input$pais)
#   ggplot(d, ...)
# })
#
# output$total <- renderText({
#   d <- filter(df, pais == input$pais)
#   nrow(d)
# })

# Prefira — filtro único, reaproveitado (fica em cache até input$pais mudar):
# dados <- reactive({
#   filter(df, pais == input$pais)
# })
#
# output$grafico <- renderPlot({
#   ggplot(dados(), ...)
# })
#
# output$total <- renderText({
#   nrow(dados())
# })


## 5) Prática guiada — Painel de imigração com os dados CGIL/OBMigra -----

# retomando a Aula 2
library(shiny)
library(dplyr)
library(ggplot2)
df <- readRDS("dados/imigracao_tratada.rds")

ui <- fluidPage(
  titlePanel("Registros de imigrantes por UF"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pais", "País de nacionalidade", choices = sort(unique(df$pais)))
    ),
    mainPanel(
      textOutput("total"),
      plotOutput("barras", height = "420px")
    )
  )
)

server <- function(input, output) {
  dados <- reactive({ df |> filter(pais == input$pais) |> count(uf, sort = TRUE) })
  output$total <- renderText({ paste(sum(dados()$n), "registros") })
  output$barras <- renderPlot({
    ggplot(dados(), aes(reorder(uf, n), n)) +
      geom_col(fill = "#003366") + coord_flip() + theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)


## 6) Layout com bslib — mesmo server, aparência de painel ---------------

library(bslib)

ui <- page_sidebar(
  title = "Painel de imigração",
  theme = bs_theme(primary = "#003366"),
  sidebar = sidebar(selectInput("pais", "País", unique(df$pais))),
  card(card_header("Registros por UF"), plotOutput("barras"))
)

# server: o mesmo definido no bloco 5


## 7) Publicar no shinyapps.io --------------------------------------------

# 1. conta e token
# rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")

# 2. pasta arrumada: app.R e a pasta dados/ juntos, com caminhos relativos

# 3. deploy
# rsconnect::deployApp()


## 8) Mini-desafio (para a Aula 4) -----------------------------------------
## - Adicione um sliderInput de ano e inclua o ano no reactive()
## - Adicione uma tabela com as cinco UFs de maior registro, ao lado do gráfico
## - Publique o app no shinyapps.io e traga o link na Aula 4
