rm(list = ls())

## 1. Save INPUT DATA for input_df
## Data can be downloaded here 
## https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/898b96e8fdf5dd9ee7476b58e84066ba
## for input_df


## 2. UPDATE CODE DIRECTORY
## Update with location of directory where code folder/repo "publication_figures" was saved.
your_user_repo <- 'H:/repos'
code_folder <- 'publication_figures/Figures/Arrow_diagram'
source(file.path(your_user_repo, code_folder, '1_arrow_helper_functions.R'))


## 3. RUN FUNCTION
## 3A. Run install_reticulate()
## Note: Reticulate, miniconda, and a new conda environment need 1-3GB of space to install.
## The function will check what packages exist and only install packages as needed. 
## Installing these packages is a requirement for running create_arrow_diagram.
install_reticulate(user_repo = your_user_repo,
                   code_folder = code_folder)

## 3B. Run create_arrow_diagram()
# create_arrow_diagram(user_repo = your_user_repo)

# ## More detailed call:
create_arrow_diagram(user_repo = your_user_repo,
                     inputs_folder = 'publication_figures/Figures/Arrow_diagram/inputs',
                     outputs_folder = 'publication_figures/Figures/Arrow_diagram',
                     code_folder = 'publication_figures/Figures/Arrow_diagram',
                     input_df = 'sample_data.csv',
                     causes_df = 'cause_metadata.csv',
                     year1 = 1990,
                     year2 = 2000,
                     year3 = 2021)
