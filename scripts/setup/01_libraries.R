library(censusapi)
library(tidyverse)
library(dplyr)
library(extrafont)
library(scales)
library(directlabels)
library(grid)
library(here)
library(knitr)
library(readxl)
library(xfun)
library(rmarkdown)
library(AzureAuth) ## to connect with credentials
library(AzureStor) ## to access the stored data

readRenviron(".env.test")
CLIENT_SECRET <- Sys.getenv("TEST_CLIENT_SECRET")

# library(xlsx)

# load("inputs/allparishesRawx.RData")


# Define Azure AD tenant ID, client ID, and client secret
tenant_id <- "749c1fba-2c29-4a0f-87da-877647bbbd86"
client_id <- "e51bd1ed-87d9-46bb-a94a-9f2c34dd4d7b"
client_secret <- CLIENT_SECRET
resource <- "https://datacenterdc2datalake.blob.core.windows.net/"

# Authenticate using the service principal
token <- get_azure_token(
  resource = resource,
  tenant = tenant_id,
  app = client_id,
  password = client_secret
)

# Create storage endpoint
ad_endp_tok2 <- storage_endpoint(resource, token = token)

# Connect to the storage container
cont_proj <- storage_container(ad_endp_tok2, "project")

tryCatch(
  {
    # Example: Read a CSV file from the storage container
    # df <- storage_read_csv(cont_proj, "who_lives/2022/inputs/WhoLives_longer.csv") %>% select(-1) ((make sure this is updated within datalake - querying_WhoLives_factconsolidated))
    # Example: Write a CSV file back to the storage container
    # storage_write_csv(df, cont_proj, "who_lives/2022/outputs/file_name")
  },
  error = function(e) {
    # Log the error and stop the process
    message("An error occurred: ", e$message)
    stop("Stopping script due to error")
  }
)
