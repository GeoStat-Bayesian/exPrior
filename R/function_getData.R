## getData ##

#'extract data from wwhypda database
#'
#'\code{getData} queries the wwhypda database according to specifications
#'of site name, rock type, and parameter of interest and returns data as a dataframe.
#'
#'@param rock_type a character indicating rock type. if left blank, data from all rock types returned.
#'@param param a character indicating parameter. if left blank, data from all parameters returned.
#'@param site a character indicating site. if left blank, data from all sites returned.
#'@param view_rocktypes view the types of rocks in the database
#'@param view_params view the parameters in the database
#'@param view_sites view the sites in the database
#'@return data queried from the wwhypda database as a dataframe
#'@examples
#'my_data <- getData(rock_type = "Sandstone", param = "porosity")
#'head(my_data)
#'
#'all_data <- getData()
#'
#'lagenthal <- getData(site = "Langenthal")
#'dim(lagenthal)
#'@export
getData <- function(rock_type=NULL,
                    param=NULL,
                    site=NULL,
                    view_rocktypes = FALSE,
                    view_params = FALSE,
                    view_sites = FALSE)
{

  # connect to wwhypda
  # ===========================================================================

  arg_password <- readline(prompt="password to local host: ")
  arg_database <- readline(prompt="name of local wwhypda database: ")

  con <- wwhypdaConnect(password = arg_password,db_name = arg_database)

  # sanity checks: ensure that rock type, parameter, and site are valid
  # ===========================================================================
  info <- viewInfo(password = arg_password,db_name = arg_database)

  if ( !(is.null(rock_type)) && !(rock_type %in% info$rock_types$rt_name) )
    stop (paste(rock_type, "not in database. run viewInfo() to see available rock types!"))

  if ( !(is.null(site)) && !(site %in% info$sites$site_name) )
    stop (paste(site, "not in database. run viewInfo() to see available sites!"))

  if ( !(is.null(param)) && !(param %in% info$parameters$param_name) )
    stop (paste(parameter, "not in database. run viewInfo() to see available parameters!"))


  # extract data
  # ===========================================================================
  basic_query <- "select distinct id_Measure, msr_value, id_smpl, id_ex_ty, param_name, key_Fract, key_rt, id_env,
  site_id, site_name, region, rt_name
  from measure
  join parameter as p on measure.id_par_msr = p.id_Parameter
  join sample as s on s.id_Sample = measure.id_smpl
  join rock_type as r on r.rt_id = s.key_rt
  join measure_group as mg on mg.id_Measure_group = s.key_Mgroup
  join site_info as si on si.site_id = mg.id_pnt;"

  basic_data <- DBI::dbGetQuery(con, basic_query)

  # specifying rock type, param, site
  # ===========================================================================

  if ( !(is.null(rock_type)) ){basic_data <- basic_data[which(basic_data$rt_name == rock_type),]}
  else {basic_data = basic_data}

  if ( !(is.null(param)) ){basic_data <- basic_data[which(basic_data$param_name == param),]}
  else {basic_data = basic_data}

  if ( !(is.null(site)) ){basic_data <- basic_data[which(basic_data$site_name == site),]}
  else {basic_data = basic_data}

  # include option to return info

  RMySQL::dbDisconnect(con) # close connection
  return (basic_data)
}
