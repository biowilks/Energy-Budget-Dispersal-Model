library('workflowr')

#### using workflowr to create all folders and files in directory####
wflow_git_config(user.name = "biowilks", user.email = "biowilks@gmail.com")

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

#### Publishing website publicly ####
wflow_git_remote(
  remote = "origin",
  user = "biowilks",
  repo = "Energy-Budget-Model",
  action = "set_url"
)

wflow_publish("analysis/*.Rmd", "Editing Energy Budget Model website")
wflow_git_push()
