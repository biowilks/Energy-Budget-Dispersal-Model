rm(list=ls())

# Load all packages ----------
library('workflowr')

#### Configure Git settings for workflowr project ----------
wflow_git_config(user.name = "biowilks", user.email = "biowilks@gmail.com")

# Start a new workflowr project in an existing directory
wflow_start(
  directory = "C:/Users/xr49abiw/Documents/Energy-Budget-Model",
  name = NULL,
  git = FALSE,
  existing = TRUE,
  overwrite = FALSE,
  change_wd = TRUE,
  disable_remote = FALSE,
  dry_run = FALSE,
  user.name = NULL,
  user.email = NULL
)

# Publish changes in the workflowr project
wflow_publish()

# Build the website after making edits in .Rmd files
wflow_build()

# View the updated website locally
wflow_view()

# Check the status of files in the workflowr project
wflow_status()

# Configure the Git remote for publishing the website ----------
wflow_git_remote(
  remote = "origin",
  user = "biowilks",
  repo = "Energy-Budget-Model",
  action = "set_url"
)
# Publish and push the website to GitHub
wflow_publish("analysis/*.Rmd", "Editing Energy Budget Model website")
wflow_git_push()
