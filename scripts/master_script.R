# Load libraries and global variables
source(here("scripts/setup/01_libraries.R"))
source(here("scripts/css/css_helper.R"))
source(here("scripts/setup/02_variables.R"))

# Source functions
source(here("functions/data_functions.R"))
source(here("functions/analysis_functions.R"))
source(here("functions/visualization_functions.R"))

# Execute data pull
source(here("data_pull/pull_acs.R"))
source(here("data_pull/pull_pep.R"))

# Perform analysis
source(here("analysis/analysis_acs.R"))
source(here("analysis/analysis_pep.R"))

# Create visualizations
source(here("graphics/race_and_ethnicity.R"))
source(here("graphics/pop_by_age_and_household.R"))
source(here("graphics/ed_attainment_income_and_internet_access.R"))
source(here("graphics/poverty_and_vehicle_access.R"))
source(here("graphics/foreign_born_pop.R"))
source(here("graphics/homeownership.R"))
source(here("graphics/housing_costs_and_commuting.R"))

# Inline
source(here("inline/inline.R"))

# Define fig_path for the first knit (AWS URL)
fig_path_aws <- paste0("https://s3.amazonaws.com/files.datacenterresearch/who-lives/uploads/", format(Sys.Date(), "%Y"), "/")

# Define fig_path for the second knit (local path for upload)
fig_path_local <- paste0("wp-graphics/uploads/", format(Sys.Date(), "%Y"), "/", format(Sys.Date(), "%m"), "/")

# First knit with AWS URL
rmarkdown::render(here("WhoLivesMarkdown.Rmd"),
    output_file = here("WhoLivesMarkdown_part1.md"),
    params = list(fig_path = fig_path_aws)
)

# Second knit with local path
rmarkdown::render(here("WhoLivesMarkdown.Rmd"),
    output_file = here("WhoLivesMarkdown_part2.md"),
    params = list(fig_path = fig_path_local)
)
