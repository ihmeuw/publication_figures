#' @title Arrow Diagram helper functions
#' @description This script contains functions to prepare data and generate arrow diagrams for visualization.
#' @dependencies Requires the `data.table` and `reticulate` packages. Python dependencies are installed via Miniconda.
#' @usage Source this file and call `create_arrow_diagram()` to generate diagrams.

if (!("data.table" %in% installed.packages())) {
  install.packages("data.table")
}
library(data.table)

#' Create Arrow Diagram
#' This main function uses helper functions to prepare input data (prep_arrow_data()) and generate the 
#' arrow diagram and save it as a .pdf (make_arrow_chart())
#' @param user_repo Character. The root directory of the user’s repository.
#' @param inputs_folder Character. Path to the folder containing input data files relative to `user_repo`. 
#'        Default: 'publication_figures/Figures/Arrow_diagram/inputs'.
#' @param code_folder Character. Path to the folder containing code files relative to `user_repo`. 
#'        Default: 'publication_figures/Figures/Arrow_diagram'.
#' @param outputs_folder Character. Path to the folder to save outputs relative to `user_repo`. 
#'        Default: 'publication_figures/Figures/Arrow_diagram'.
#' @param input_df Character. Name of the input dataset file. Default: 'sample_data.csv'.
#' @param causes_df Character. Name of the causes metadata file. Default: 'cause_metadata.csv'.
#' @param year1 Numeric. The first year for the lefthand column. Default: 1990.
#' @param year2 Numeric. The second year for the middle column. Default: 2000.
#' @param year3 Numeric. The third year for the righthand column. Default: 2021.
#'
#' @returns None. Outputs are saved directly to the specified folders.
#'
#' @examples create_arrow_diagram(user_repo = "/path/to/repo")

create_arrow_diagram <- function(user_repo,
                                 inputs_folder = 'publication_figures/Figures/Arrow_diagram/inputs',
                                 code_folder = 'publication_figures/Figures/Arrow_diagram',
                                 outputs_folder = 'publication_figures/Figures/Arrow_diagram',
                                 input_df = 'sample_data.csv',
                                 causes_df = 'cause_metadata.csv',
                                 year1 = 1990,
                                 year2 = 2000,
                                 year3 = 2021){

  print(paste0("pulling code from ", file.path(user_repo, code_folder), "..."))
  
  prep_arrow_data(input_filepath = file.path(user_repo, inputs_folder, input_df), 
                  prepped_filepath = file.path(user_repo, inputs_folder, 'arrow_prep.csv'), 
                  causes_filepath = file.path(user_repo, inputs_folder, causes_df), 
                  year1 = year1, 
                  year2 = year2, 
                  year3 = year3)
  
  make_arrow_chart(user_repo = user_repo,
                   code_folder = code_folder,
                   outputs_folder = outputs_folder)
}

#' Prepare Data for Arrow Diagram
#'
#' This function reformats and processes user-provided input data (downloaded from the GBD Results tool) 
#' to prepare it for the arrow diagram. 
#' It handles cause-level filtering, ranking, and reshaping data for visualization.
#'
#' @param input_filepath Character. Full file path to the input dataset (CSV format).
#' @param prepped_filepath Character. Full file path to save the prepped dataset (CSV format).
#' @param causes_filepath Character. Full file path to the causes metadata file (CSV format).
#' @param year1 Numeric. The first year for the lefthand column of the arrow diagram.
#' @param year2 Numeric. The second year for the middle column of the arrow diagram.
#' @param year3 Numeric. The third year for the righthand column of the arrow diagram.
#'
#' @return None. Outputs are saved directly to the specified file path.
#'
#' @examples
#' prep_arrow_data(input_filepath = "inputs/sample_data.csv", 
#'                 prepped_filepath = "outputs/arrow_prep.csv", 
#'                 causes_filepath = "inputs/cause_metadata.csv", 
#'                 year1 = 1990, year2 = 2000, year3 = 2021)

prep_arrow_data <- function(input_filepath,
                            prepped_filepath,
                            causes_filepath,
                            year1, 
                            year2,
                            year3){
  print("reformatting data for arrow diagram...")
  # Preparing causes
  print(paste0("pulling input data from ", input_filepath))
  df <- fread(input_filepath)
  print(paste0("Pulling cause_names from ", causes_filepath))
  causes <- fread(causes_filepath)
  
  ## Select all level 3 causes along with other Covid related
  cids <- causes[level == 3 | cause_id == 1058, cause_id]
  causes[cause_outline %like% 'A', cause_type := 'A']
  causes[cause_outline %like% 'B', cause_type := 'B']
  causes[cause_outline %like% 'C', cause_type := 'C']
  causes[cause_outline %like% 'D', cause_type := 'D']
  
  ## Select unique names for diagram title
  lid <- unique(df$location_id)
  meas_id <- unique(df$measure_id)
  sid <- unique(df$sex_id)
  aname <- unique(df$age_name)

  ## Make values into text
  df[, text := paste0(trimws(format(round(val, digits = 1), big.mark = ',')), ' (',
                      trimws(format(round(lower, digits = 1), big.mark = ',')), ' to ',
                      trimws(format(round(upper, digits = 1), big.mark = ',')), ')')]
  
  ## Drop any not-estimated causes
  df <- na.omit(df)
  
  ## Set order for ranking
  setorder(df, -'val')
  
  df[, rank := 1:.N, by = 'year']
  
  df <- merge(df, causes[, .(cause_medium, cause_id, cause_type)], by = 'cause_id')
  
  df[year == year1, year_nm := 'year1']
  df[year == year2, year_nm := 'year2']
  df[year == year3, year_nm := 'year3']
  
  # Prepare residuals (causes sometimes below rank 25)
  df_all <- as.data.table(df)
  df <- as.data.table(df)
  
  df <- data.table::dcast(df, rank ~ year_nm, 
                          value.var = c('cause_medium', 'cause_type', 'text', 'cause_id'))
  
  df <- df[rank <= 25]
  
  ## Find top causes for residuals
  top_cids <- unique(c(unique(df$cause_id_year1), unique(df$cause_id_year2), unique(df$cause_id_year3)))
  
  resid <- df_all[cause_id %in% top_cids & rank > 25]
  
  resid_year1 <- resid[year == year1]
  resid_year2 <- resid[year == year2]
  resid_year3 <- resid[year == year3]
  
  
  ## Set residuals column names and subset to required columns
  setnames(resid_year1, 
           c('cause_medium', 'cause_type', 'rank', 'text', 'year', 'cause_id'), 
           c('cause_medium_year1', 'cause_type_year1', 'rank_year1', 'text_year1', 'year1', 'cause_id_year1'))
  setnames(resid_year2, 
           c('cause_medium', 'cause_type', 'rank', 'text', 'year', 'cause_id'), 
           c('cause_medium_year2', 'cause_type_year2', 'rank_year2', 'text_year2', 'year2', 'cause_id_year2'))
  setnames(resid_year3, 
           c('cause_medium', 'cause_type', 'rank', 'text', 'year', 'cause_id'), 
           c('cause_medium_year3', 'cause_type_year3', 'rank_year3', 'text_year3', 'year3', 'cause_id_year3'))
  
  resid_year1 = resid_year1[, c('cause_medium_year1', 'cause_type_year1', 'rank_year1', 
                                'text_year1', 'year1', 'cause_id_year1')]
  resid_year2 = resid_year2[, c('cause_medium_year2', 'cause_type_year2', 'rank_year2', 
                                'text_year2', 'year2', 'cause_id_year2')]
  resid_year3 = resid_year3[, c('cause_medium_year3', 'cause_type_year3', 'rank_year3', 
                                'text_year3', 'year3', 'cause_id_year3')]
  
  ## Order residuals datasets and bind into single dataset
  setorder(resid_year1, 'rank_year1')
  setorder(resid_year2, 'rank_year2')
  setorder(resid_year3, 'rank_year3')
  resid <- cbind(resid_year1, resid_year2)
  resid <- cbind(resid, resid_year3)
  
  df[, rank_year1 := rank]
  df[, rank_year2 := rank]
  df[, rank_year3 := rank]
  
  df[, year1 := year1]
  df[, year2 := year2]
  df[, year3 := year3]
  
  df <- rbind(df, resid, fill= T)
  
  df[, resid := 0]
  df[rank_year1 > 25 | rank_year2 > 25 | rank_year3 > 25, resid := 1]
  
  setorder(df, 'rank_year1')
  df[, location_id := lid]
  df[, measure_id := meas_id]
  df[, sex_id := sid]
  df[, age_name := aname]
  print(paste0("Saving prepped data to ", prepped_filepath))
  write.csv(df, prepped_filepath, row.names = F)
}

#' Install Reticulate and Dependencies for Arrow Diagram
#'
#' This function ensures that the necessary R package (`reticulate`) and Python dependencies 
#' (via Miniconda) are installed. It also creates a Conda environment named "arrow_chart" 
#' with required Python libraries (`reportlab`, `pandas`).
#'
#' @param user_repo Character. The root directory of the user’s repository.
#' @param code_folder Character. Path to the folder containing code files relative to `user_repo`.
#'
#' @return None. Installs dependencies and sets up the Python environment.
#'
#' @examples
#' install_reticulate(user_repo = "/path/to/repo", code_folder = "publication_figures/Figures/Arrow_diagram")

install_reticulate <- function(user_repo, code_folder){
  ## install reticulate only if needed
  if (!("reticulate" %in% installed.packages())) {
    print("Reticulate not installed. Installing reticulate package...")
    install.packages("reticulate")
  } else {print("reticulate already installed...")}
  
  ## install miniconda only if needed
  if (!dir.exists(reticulate::miniconda_path())) {
    print("Miniconda not installed. Installing miniconda package...")
    reticulate::install_miniconda()
  } else {print("miniconda already installed...")}
  
  ## create conda environment only if needed
  if (!("arrow_chart" %in% reticulate::conda_list()$name)) {
    print("Creating conda environment named 'arrow_chart' with python...")
    reticulate::conda_create(envname = "arrow_chart", python_version = "3.9", packages = c("reportlab", "pandas"))
  } else {print("arrow_chart conda environment already created...")}
  print("Ready to create arrow diagram.")
}

#' Generate Arrow Diagram using Python
#'
#' This function generates the arrow diagram figure by calling a Python script (`arrow_chart_gen.py`)
#' within the Conda environment "arrow_chart". It sets up the necessary working directories and
#' ensures the Python environment is loaded.
#'
#' @param user_repo Character. The root directory of the user’s repository.
#' @param code_folder Character. Path to the folder containing code files relative to `user_repo`.
#' @param outputs_folder Character. Path to the folder to save outputs relative to `user_repo`.
#'
#' @return None. Outputs the figure directly to the specified folder.
#'
#' @examples
#' make_arrow_chart(user_repo = "/path/to/repo", 
#'                  code_folder = "publication_figures/Figures/Arrow_diagram", 
#'                  outputs_folder = "publication_figures/Figures/Arrow_diagram")

make_arrow_chart <- function(user_repo, code_folder, outputs_folder){
  ## Load python conda environment 'arrow_chart'  
  print("loading python environment...")
  library(reticulate)
  conda_location <- subset(reticulate::conda_list(), name == 'arrow_chart')$python
  reticulate::use_python(conda_location)
  
  ## Save original working directory and set working directory
  original_wd <- getwd()
  setwd(file.path(user_repo, code_folder))
  
  print("creating figure...")
  source_python(file.path(user_repo, code_folder, "2_arrow_chart_gen.py"))
  print(paste0("Figure saved to ", file.path(user_repo, outputs_folder)))
  
  ## Restore working directory
  setwd(original_wd)
}
    