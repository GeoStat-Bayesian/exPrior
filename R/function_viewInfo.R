## viewInfo ##

#'view information on wwhypda database, such as list of rock types,
#'parameters, and sites.
#'
#'\code{viewInfo} returns information on wwhypda database
#'@param password your password (optional)
#'@param db_name the name of the local wwhypda database (optional)
#'@return list of information on wwhypda database
#'@examples
#'info <- viewInfo()
#'@export
viewInfo <- function(password,db_name)
{

  if(missing(password)){
    password <- readline(prompt="password to local host: ")
  }
  if(missing(db_name)){
    db_name <-readline(prompt="name of local wwhypda database: ")
  }
  con <- wwhypdaConnect(password = password,db_name = db_name)

  all_rocks <- DBI::dbGetQuery(con, "select distinct rt_name from rock_type;")
  all_sites <- DBI::dbGetQuery(con, "select distinct site_name, region from site_info;")
  all_params <- DBI::dbGetQuery(con, "select distinct param_name from parameter;")

  RMySQL::dbDisconnect(con) # close connection

  return(
    list("parameters" = all_params,
         "rock_types" = all_rocks,
         "sites" = all_sites)
  )

}
