# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("1999-01-01"), 
                                    author = "", 
                                    proj_name = "", 
                                    script_type = "etl", 
                                    notepad = paste0("")), 
                  seed_set = 6)
metadatar

# LOAD LIBRARIES **********************************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
# library(gt)
library(janitor)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5, 
        stringsAsFactors = FALSE)
mem_used()

# basic helper functions ***************************************************

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

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

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

# read raw dataset -----------------------------------------------

# function to download the zipped files from the site and then unzip them
fun_consume <- function(arg1, arg2, arg3) {
  download.file(url = arg1, 
                destfile = arg2)
  unzip(zipfile = arg2, exdir = arg3)
}

aa <- list('https://www.census.gov/econ/currentdata/datasets/BFS-mf.zip', 
           paste0(getwd(), '/etl/ore/zip_download'), 
           paste0(getwd(), '/etl/ore'))

fun_consume(aa[[1]], aa[[2]], aa[[3]])

# cleanup !!!!!!!!!!!!!!!!!!!!!!!!
rm(aa, fun_consume)
ls()
trash()
mem_used()


# set the filepath for the downloaded data
loader_path1 <- paste0(getwd(), "/etl/ore/BFS-mf.csv")

# read the lines of the csv in raw to find the dataframes w/
#   this horribly formatted messy file
clockin()
raw_lines <- readr::read_lines(file = loader_path1, n_max = 500)
clockout()

raw_lines <- cbind(as.data.frame(raw_lines), 
                   as.data.frame(seq(1, 500, 1)))
colnames(raw_lines) <- c('line_txt_val', 'line_number')


# fint the location in the csv where the data actually starts
interim <- raw_lines |> 
  filter(line_txt_val == 'per_idx,cat_idx,dt_idx,geo_idx,is_adj,val')
# interim[1, 2] - 1

# now that we figure our where the real data starts, read from there
clockin()
raw_df <- read.csv(loader_path1, stringsAsFactors = FALSE, 
                   skip = (interim[1, 2] - 1))
clockout()
data_dictionary(raw_df)

# with the main dataframe downloaded, fine the supporting dataframes

# raw_lines |> 
#   filter(line_txt_val == 'cat_idx,cat_code,cat_desc,cat_indent')



# ^ -----

# clean the dataset -------------------------------------------

# cleaning
clockin()
dfa <- raw_df %>% clean_names() %>% as_tibble()
clockout()

# cleanup !!!!!!!!!!!!!!!!!!!
rm(raw_df)
ls()
trash()

# ^ ----- 

# write the cleaned dataset ---------------------------------------

# write to rds
filename <- paste0(getwd(), "/etl/ingot/dataframe.rds")
clockin()
saveRDS(dfa, file = filename)
clockout()

# ^ ----- 

# summarize and record etl -------------------------------------

(interim <- list(a = Sys.info(), 
                 b = nrow(dfa), 
                 c = ncol(dfa), 
                 d = sizer(dfa)))

# create an etl summary object
etl_metadata <- data.frame(etl_runtime = metadatar$script_starttime, 
                           etl_user = interim$a[[8]], 
                           data_rows = interim$b, 
                           data_cols = interim$c, 
                           data_size = interim$d, 
                           etl_note = 'no notes')
etl_metadata
rm(interim)

# write to csv
filename <- paste0(getwd(), "/etl/etl_metadata.csv")
clockin()
write.csv(etl_metadata, file = filename, row.names = FALSE)
clockout()

# ^ ----- 