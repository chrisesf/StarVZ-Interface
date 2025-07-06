#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  trace_dir = getwd()
} else if (length(args)==1) {
  trace_dir = args[1]
}

library(readr)
library(dplyr, warn.conflicts = FALSE)
options(crayon.enabled = FALSE)
library(tidyr)
library(bit64)

suppressMessages(library(RColorBrewer))

phase1 <- function(direct, recursive = FALSE){
  file1 <- paste(direct, "tasks.csv", sep="/")

  suppressWarnings(
    classes = "vroom_parse_issue", # tasks.csv header lists an additional column
    df1 <- read_delim(file1, delim=",", col_names=TRUE,
                      col_types = cols(Resource = col_double(),
                                       Numa = col_integer(),
                                       Start = col_double(),
                                       End = col_double(),
                                       Duration = col_double(),
                                       Explicit = col_integer(),
                                       Aff = col_integer(),
                                       Strict = col_integer(),
                                       Tag = col_character(),
                                       Key0 = col_character(),
                                       Key1 = col_character(),
                                       Name = col_character(),
                                       TaskId = col_integer(),
                                       ParentTaskId = col_integer(),
                                       Work = col_character()))
    )

  if (recursive == FALSE) {
      df1 <- df1 %>% arrange(TaskId) %>% mutate(JobId = 1) %>% mutate(JobId = cumsum(JobId)-1)
  } else {
      # considering unique TaskId
      tmp_JobId <- df1 %>% select(TaskId) %>% unique %>% arrange(TaskId) %>% mutate(JobId = 1) %>% mutate(JobId = cumsum(JobId)-1)
      df1 <- df1 %>% left_join(tmp_JobId)

      # tentando ficar só com as folhas nos casos recursivos
      hasChildren <- unique(df1$ParentTaskId)
      df1 <- df1 %>% filter(!(TaskId %in% hasChildren))
  }

  data <- df1 %>%
    rename(Value = Name) %>%
    mutate(Nature = "State", ResourceId = paste0("CPU", Resource),
           Type = "Worker State",
           Params = NA, Footprint = NA,
           SubmitOrder = NA,
           GFlop = NA,
           Depth = NA,
           Size = NA,
           Iteration = as.integer(as.integer64(Tag) %/% as.integer64(2^32)),
           X = NA,
           Y = NA,
           Subiteration = as.integer(as.integer64(Tag) %%  as.integer64(2^32))) %>%
    select(-Resource, -Work, - Key0, -Key1,
           -Numa,
    ) %>%
    mutate(End = End/1000000,
           Start = Start/1000000,
           Duration = Duration/1000000) %>%
    select(Nature, ResourceId, Type, Start, End, Duration, Depth, Value, Size, Params, Footprint, Tag, JobId, SubmitOrder, GFlop, X, Y, Iteration, Subiteration)

  if (recursive) {
      data <- data %>% unique
  }
  
  write_csv(data, paste(direct, "paje.worker_state.csv.gz", sep="/"), quote = "none")

  # file2 <- paste(direct, "dag.csv", sep="/")
  # df2 <- read_delim(file2, delim=",", col_names=TRUE,
  #                   col_types = cols(Dependent = col_character(),
  #                                    JobId = col_character()))

  dag_rec_raw <- read_csv(paste(direct, "dag-do-rec-raw.csv", sep="/"),
                          col_types = cols(Name = col_factor(),
                                           JobId = col_character(),
                                           DependsOn = col_character())) %>%
    mutate(Value = factor(Name, levels = c("gemm", "potrf", "syrk", "trsm"))) %>%
    select(-Name) %>%
    separate_rows(DependsOn, sep = " ")

  dirdagrec <- paste(direct, "rec-dag/", sep="/")
  dir.create(dirdagrec, showWarnings = FALSE)

  dag_rec_raw %>%
    select(JobId, DependsOn) %>%
    rename(Dependent = DependsOn) %>%
    select(Dependent, JobId) %>%
    na.omit() %>%
    write_csv(paste0(dirdagrec, "dag.csv.gz"), quote = "none")

}


phase1(trace_dir)
