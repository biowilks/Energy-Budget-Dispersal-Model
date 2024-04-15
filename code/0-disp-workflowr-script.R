library('workflowr')

wflow_git_config(user.name = "biowilks", user.email = "biowilks@gmail.com")


## adding workflowr files to existing directory

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
