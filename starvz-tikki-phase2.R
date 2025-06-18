#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  trace_dir = getwd()
  conf = NULL
} else if (length(args)==1) {
  trace_dir = args[1]
  conf = NULL
} else if (length(args)==2) {
  trace_dir = args[1]
  conf = args[2]
}

library(starvz)
library(tibble)
library(dplyr, warn.conflicts = FALSE)

phase2 <- function(direct2, config_file = NULL){

  if (!is.null(config_file)) {
    config <- starvz_read_config(config_file)
  } else {
    config <- starvz_read_config(file.path(direct2, "config.yaml"), warn = FALSE)
  }

  newdado2 <- starvz:::read_worker_csv(
    where = direct2,
    whichApplication = "",
    #app_states_fun = function(){
    #  tibble(Kernel = "abc", Color = "#e41a1c", Use = TRUE)
    #},
    app_states_fun = cholesky_colors,
    config = config,
    outlier_fun = starvz:::outlier_definition
    )

  # montando as colunas Position e Height
  newdado2$Application = newdado2$Application %>% mutate(Position = as.numeric(gsub("CPU", "", ResourceId)), Height = 1)

  # forcando uma versao
  newdado2$Version = "0.2.0"

  newdado2$config = config

  # DAG do REC
  dag_rec <- starvz:::read_dag(
    where = paste(direct2, "rec-dag/", sep="/"),
    Application = newdado2$Application %>% mutate(Application = TRUE),
    dfl = NULL # nao temos informacoes de links por eqto
  )

  newdado2$Dag = dag_rec

  # Nao temos links
  # o read_links retorna NULL o que faz quebrar depois
  #newdado2$Links = starvz:::read_links(where = "./", newdado2$ZERO)
  newdado2$Links = tibble(Nature = character(),
                          Container = character(),
                          Type = character(),
                          Start = double(),
                          End = double(),
                          Duration = double(),
                          Size = integer(),
                          Origin = character(),
                          Dest = character(),
                          Key = character(),
                          Tag = character(),
                          MPIType = character(),
                          Priority = integer(),
                          Handle = character())

  # Nao temos o Y, necessario para o starvz:::last, no StarPU ele eh obtido (indiretamente) do entities.csv gerado pelo paje. Porém, o entities.csv gerado do rastro paje do tikki nao parece ser suficiente...
  # Vamos ter que cria-lo a mao
  newdado2$Y = newdado2$Application %>%
    select(ResourceId, Height, Position) %>%
    unique %>%
    rename(Parent = ResourceId) %>%
    mutate(Type = "Worker State") %>%
    select(Parent, Type, Height, Position)

  # Compute last
  newdado2$Last = starvz:::compute_all_last(newdado2)

  # forcando uma versao
  newdado2$Version = "0.2.0"

  newdado2 <- starvz:::new_starvz_data(newdado2)

  return(newdado2)

}

dados <- phase2(trace_dir, config_file = conf)

starvz_plot(dados, save = TRUE)
