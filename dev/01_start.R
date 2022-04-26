golem::fill_desc(
  pkg_name = "clustering.fun",
  pkg_title = "Clustering for Fun",
  pkg_description = "Interactive clustering of R data sets.",
  author_first_name = "Giovanni",
  author_last_name = "Colitti",
  author_email = "g.a.colitti@gmail.com",
  repo_url = "gacolitti/clustering.fun"
)     
golem::set_golem_options()
usethis::use_mit_license( "Giovanni Colitti" )
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )
usethis::use_git()
golem::use_recommended_tests()
golem::use_recommended_deps()
golem::use_favicon()
golem::remove_favicon()
golem::use_utils_ui()
golem::use_utils_server()
rstudioapi::navigateToFile( "dev/02_dev.R" )
