connect_db <- function() {
  DBI::dbConnect(RSQLite::SQLite(), "inst/sqlite/tree_database.sqlite",
                 loadable.extensions = T)
}

# con <- connect_db()



