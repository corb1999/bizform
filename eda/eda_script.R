# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO =================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("1999-01-01"), 
                                    author = "", 
                                    proj_name = "", 
                                    script_type = "eda", 
                                    notepad = paste0("")), 
                  seed_set = 6)
metadatar

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

# source the script that will load the data and prep it for analysis
sourcerpath <- paste0(getwd(), '/eda/remodel/remodel_script.R')
clockin()
source(file = sourcerpath)
clockout()

# source another script with analysis functions to run
sourcerpath <- paste0(getwd(), '/eda/engine/engine_script.R')
clockin()
source(file = sourcerpath)
clockout()

# cleanup
ls()
trash()
mem_used()

# ^ ====================================
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::

# viz prep ---------------------------------------------------

dt_filters <- fun_dater('2015-01-01', 
                        '2022-08-01')

# unique(dfa$dt_desc)
# measurement <- 'High-Propensity Business Applications'
measurement <- 'Business Applications with Planned Wages'
# measurement <- 'Business Applications from Corporations'

plt_geo <- 'GA'

(pltname <- 'US Census Bureau Business Formation Data; ' %ps% 
    # other filter ::::::::::::::::::::::::::::::::
    plt_geo %ps% '; ' %ps% 
    'ALL NAICS; ' %ps% 
    # :::::::::::::::::::::::::::::::::::::::::::::
    dt_filters$date_text_str %ps% 
    '')

dfplt <- dfa |> 
  # choose measure to filter down to ::::::::::::::::::::
  filter(dt_desc == measurement) |> 
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::
  # other key filters :::::::::::::::::::::::::::::::::::
  filter(geo_code == plt_geo) |>
  filter(cat_code == 'TOTAL') |> 
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::
  filter(time_dt >= dt_filters$start_date, 
         time_dt <= dt_filters$end_date) |> 
  # turn off adj vals ::::::::::::::::::::::::::::::::::
  filter(is_adj == 0) |> 
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::
  filter(measure_val > 0)

# ^ -----

# run plots and visuals ------------------------------------

fun_plt_macro_trend1()
fun_plt_macro_trend2(arg_months = c(1, 6))

# fun_plt_macro_yoy1()

fun_plt_index1(arg_indexyr = 2015)

# ^ -----

# plot printer iterator ----------------------------------
# designed to print plots for a vector of states

geo_vec <- c('FL', 'GA', 'NC', 'SC', 
             'MD', 'VA', 'TN')

(pltname2 <- 'US Census Bureau Business Formation Data; ' %ps% 
    'ALL NAICS; ' %ps% 
    dt_filters$date_text_str %ps% 
    '')

dfb <- dfa |> filter(cat_code == 'TOTAL') |> 
  filter(dt_desc == measurement) |>
  filter(time_dt >= dt_filters$start_date, 
         time_dt <= dt_filters$end_date) |> 
  filter(is_adj == 0) |> filter(measure_val > 0)

fun_printer <- function(arg1) {
  aa <- pltname2 %ps% '_' %ps% arg1
  bb <- dfb |> filter(geo_code == arg1)
  # function to iterate :::::::::::::::::::
  # cc <- fun_plt_macro_trend1(arg_df = bb, arg_pltnm = aa)
  # cc <- fun_plt_macro_trend2(arg_df = bb, arg_pltnm = aa, 
  #                            arg_months = c(1, 8))
  cc <- fun_plt_index1(arg_df = bb, arg_pltnm = aa, 
                       arg_indexyr = 2015)
  # :::::::::::::::::::::::::::::::::::::::
  qp(pltname = ('census_nb_applications_' %ps% arg1), 
     pltpath_suffix = '/printer_tray')
}


clockin()
walk(.x = geo_vec, .f = fun_printer)
clockout()


# ^ -----
