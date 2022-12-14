# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ============================================================

# LOAD LIBRARIES ***********************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
library(gt)
library(janitor)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5, 
        stringsAsFactors = FALSE)
mem_used()

# basic helper functions ************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  aa <- data.frame(aa)
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  ee <- list(dims = data.frame(row_n = nrow(aa), col_n = ncol(aa)), 
             obj_size = object.size(aa), 
             c_names = c(colnames(aa)), 
             dict = dd)
  return(ee)}

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# POST SCRIPT; alt to using paste0() all the time (i saw this on twitter)
'%ps%' <- function(lhs, rhs) {
  return_me <- paste0(lhs, rhs)
  return(return_me)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load data and prepare data for analysis -------------------

# load a rds file
loader_path1 <- paste0(getwd(), "/etl/ingot/dataframe.rds")
clockin()
raw_df <- readRDS(loader_path1)
clockout()
dim(raw_df)

# any last minute cleaning
dfa <- raw_df |>
  mutate(max_grain_ind = ifelse(geo_code == 'US' & 
                                  cat_code == 'TOTAL', 
                                TRUE, FALSE), 
         geo_grain_ind = ifelse(geo_code != 'US' & 
                                  geo_code != 'NO' & 
                                  geo_code != 'MW' & 
                                  geo_code != 'SO' & 
                                  geo_code != 'WE', 
                                TRUE, FALSE), 
         naics_grain_ind = ifelse(cat_code != 'TOTAL', 
                                  TRUE, FALSE))

# make a version of the data at a tidy grain of state
dfb <- dfa |>
  filter(geo_grain_ind == TRUE) |> 
  pivot_wider(names_from = dt_desc, values_from = measure_val) |> 
  clean_names()

# make a version of the data tidy but by naics
dfc <- dfa |>
  filter(naics_grain_ind == TRUE) |> 
  pivot_wider(names_from = dt_desc, values_from = measure_val) |> 
  clean_names()

# cleanup !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
rm(raw_df)
ls()
trash()
mem_used()

# date cleaning function for filtering in analysis -----------------

fun_dater <- function(arg_sdt, arg_edt) {
  x <- as.Date(arg_sdt)
  y <- as.Date(arg_edt)
  z <- paste0('Dates: ', x, ' to ', y, '; ')
  return_me <- list(start_date = x, 
                    end_date = y, 
                    date_text_str = z)
  return(return_me)
}

# ^ -----

# load the etl metadata for reference ------------------------

# load a csv file
loader_path1 <- paste0(getwd(), "/etl/etl_metadata.csv")
clockin()
etl_metadata <- read.csv(loader_path1, stringsAsFactors = FALSE)
clockout()
etl_metadata


# ^ -----

source(file = paste0('https://raw.githubusercontent.com/', 
                     'corb1999/cheatcraft/', 
                     'main/gg_helper/my_gg/my_gg_settings.R'))

# quick plot saving function -------------------------------------------

# require(lubridate)
qp <- function(pltname, pltpath_suffix = NA, plt_inch = 5) {
  plt_timestamp <- paste(year(Sys.time()), month(Sys.time()),  
                         day(Sys.time()), hour(Sys.time()), minute(Sys.time()), 
                         floor(second(Sys.time())), sep = "-")
  aa <- ifelse(is.na(pltpath_suffix), "", pltpath_suffix)
  plt_filepath <- paste0(getwd(), aa)
  plt_name <- paste0("plt_", pltname, "_", plt_timestamp, ".png")
  ggsave(filename = plt_name, plot = last_plot(), 
         path = plt_filepath, scale = 1, device = "png", 
         height = plt_inch, width = plt_inch * 1.61803399, units = "in")}


# test the plot saver +++++++++++++++++++++++++++++++++++++
# ggplot(data = mtcars, aes(mpg)) + geom_histogram()
# qp(pltname = "test")

# ^ -----
