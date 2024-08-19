library('workflowr')

wflow_git_config(user.name = "biowilks", user.email = "biowilks@gmail.com")


#### using workflowr to create all folders and files in directory####

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


wflow_publish()

#### Building website after making edits in .Rmd files ####
wflow_build()

##View updated website
wflow_view()

#check status of the files
wflow_status()

#publish updated files to website
wflow_publish(c("C:/Users/xr49abiw/Documents/Energy-Budget-Model/analysis/index.Rmd",
                "C:/Users/xr49abiw/Documents/Energy-Budget-Model/analysis/about.Rmd",
                "C:/Users/xr49abiw/Documents/Energy-Budget-Model/analysis/license.Rmd"))

#### Publishing website publicly ####
wflow_git_remote(
  remote = "origin",
  user = "biowilks",
  repo = "Energy-Budget-Model",
  action = "set_url"
)


wflow_use_github("biowilks", repository = "Energy-Budget-Model")
wflow_publish("analysis/*.Rmd", "Publishing Energy Budget Model website")
