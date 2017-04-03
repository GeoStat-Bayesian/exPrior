## viewInfo ##

#'view information on wwhypda database, such as list of rock types,
#'parameters, and sites.
#'
#'\code{viewInfo} returns information on wwhypda database
#'@return list of information on wwhypda database
#'@examples
#'info <- viewInfo()
#'@export
viewInfo <- function()
{
  con <- wwhypdaConnect("h.*8+Xl)")

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
