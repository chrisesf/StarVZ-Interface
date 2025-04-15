# 📊 StarVZ Plotly Shiny App

Uma interface Shiny para visualização interativa dos dados do StarVZ, com suporte a gráficos `ggplot2` e `plotly`.

---

## 🚀 Como rodar

```r
source("app.R")
shinyApp(ui = ui, server = server)
```

## 📦 Requisitos

```r
install.packages(c(
  "shiny", "shinyjs", "shinyFiles",
  "ggplot2", "plotly", "dplyr",
  "tidyr", "tibble", "fs"
))

```


## 📌 Passo a passo
```r
1. Rode o app
2. Escolha o diretório com os dados
3. Selecione o arquivo `.yaml`
4. Clique em **Carregar Dados**
5. Configure as opções desejadas
6. Clique em **Gerar Gráfico Interativo**
```
