gh::gh_whoami()

library(gitcreds)
library(usethis)
use_git_config(
  user.name = "Yawen Ma", 
  user.email = "y.ma24@lancaster.ac.uk"
  )
create_github_token()
gitcreds_set()
