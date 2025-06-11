rm(list = ls())

## Load libraries
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(extrafont)
loadfonts()
options(scipen = 999)

## Define fonts
f1 <- "Times"
f2 <- "ScalaLancetPro"
f3 <- "Shaker 2 Lancet Regular"

## -----------------------------------------------------------------------------
#     SETTINGS
## -----------------------------------------------------------------------------
## Directories and file paths

## CODE DIRECTORY
## Update with path to code directory
user_repo <- "path/to/code/directory/"


## INPUT DATA
## Sample data can be downloaded from GBD Compare, Top right: "Download"

# http://ihmeuw.org/6j83

## Path to sample data
input_data <- fread(paste0(user_repo, "external_publications/Figures/Heatmap/Inputs/sample_data.csv"))
## OR 
## Update with path to your input data
#input_data <- fread("path/to/input/data.csv")

## OUTPUT DIRECTORY
## Update with path to output directory for final figure
out_dir <- "path/to/output/directory"
out_dir <- "~/ext_pubs/external_publications/Figures/Heatmap/"
output_filepath <- paste0(out_dir, "Heatmap.pdf")

## SORT BY
#we need to sort the heatmap according to a location

sort_loc = 'Angola'

# Adjust bands for the number of ranking

bands = c(1, 2, 3, 5, 9, 14)

## TITLE

title = "Ranking of level 2 cause DALYs per 100,000, All ages both sexes 2021"



## -----------------------------------------------------------------------------
#     RUN
## -----------------------------------------------------------------------------

df = input_data



#reformat data from Long to Wide https://www.statology.org/long-vs-wide-data/
df = dcast(df, Location+Year+Age+Sex+`Cause of death or injury`~Measure, value.var = 'Value')


## Changing making a sorting variable based on the location we want to sort on
setorder(df, `DALY rank`)
df[Location == sort_loc, sort := 1:.N]
df[is.na(sort), sort := 0]
df[, sort := max(sort), by = 'Cause of death or injury']

# Classic colors used on GBD Compare
colors = c('#D73027', '#FC8D59', '#FEE090', '#E0F3F8', '#91BFDB', '#4575B4')


#Categorizing our ranking into color labels
df[, color_bands := 1]
df[`DALY rank` >= bands[2], color_bands := 2]
df[`DALY rank` >= bands[3], color_bands := 3]
df[`DALY rank` >= bands[4], color_bands := 4]
df[`DALY rank` >= bands[5], color_bands := 5]
df[`DALY rank` >= bands[6], color_bands := 6]

# Make plot
heatmap = ggplot(df, aes(y = reorder(`Cause of death or injury`, -sort), x = Location, 
               fill = as.factor(color_bands), label = `DALY rank`)) +
  geom_tile() +
  geom_text(color = ifelse(df$color_bands %in% c(1, 6), '#FFFFFF', '#000000')) + 
  theme_bw() + scale_fill_manual('', values = colors) +
  scale_x_discrete(position = "top") +xlab('') + ylab('') +
  theme(axis.text.x = element_text(angle = -45, hjust = 1),
        legend.position = 'none') +
  ggtitle(title)

## Save as pdf
pdf(file = output_filepath, height = 6, width = 10)
plot(heatmap)
dev.off()