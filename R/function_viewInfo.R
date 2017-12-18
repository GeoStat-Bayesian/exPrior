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
  con = dbConnect(SQLite(),
                  dbname="../data/wwhypda.sqlite")
  all_rocks <- DBI::dbGetQuery(con, "select distinct rt_name from rock_type;")
  all_sites <- DBI::dbGetQuery(con, "select distinct site_name, region from site_info;")
  all_params <- DBI::dbGetQuery(con, "select distinct param_name from parameter;")

  colnames(all_rocks) <- NULL; colnames(all_sites) <- NULL; colnames(all_params) <- NULL
  all_sites <- na.omit(all_sites)

  basic_query <- "select id_Measure, msr_value, id_smpl, id_coh, id_ex_ty,
id_int_mtd, id_qlt, quality_level, id_Parameter, 'code', param_name,
  units, 'MaxValue', MinValue, key_Fract, key_rt, rt_name, rt_description, rt_left, rt_right,
  rt_id_parent, key_Scale, id_src, id_rew, id_env,
  env_name, env_description, env_id_parent,
  id_pnt,
  site_id, site_name, region, ISO_code, country_name,
  key_Mgroup, mg_date, mg_comment, sample_comment

  from measure
  join parameter as p on measure.id_par_msr = p.id_Parameter
  join quality as q on measure.id_qlt = q.id_Quality
  join sample as s on s.id_Sample = measure.id_smpl
  join rock_type as r on r.rt_id = s.key_rt
  join measure_group as mg on mg.id_Measure_group = s.key_Mgroup
  join environment as ev on ev.env_id = mg.id_env
  join site_info as si on si.site_id = mg.id_pnt
  join country as co on co.ISO_code = si.iso_country
  order by id_Measure;"

  all_data <- dbGetQuery(con, basic_query)

  # get frequency table for all_params
  param_table <- all_data
  param_table <- param_table$param_name

  param_table <- as.data.frame(table(param_table))
  colnames(param_table) <- c("parameter", "n_measurements")

  # get frequency table for all_sites
  site_table <- all_data
  site_table <- as.data.frame(table(site_table$site_name))
  colnames(site_table) <- c("site_name", "n_measurements")

  # get frequency table for all_rocks
  rock_table <- all_data$rt_name
  rock_table <- as.data.frame(table(rock_table))
  colnames(rock_table) <- c("rock_type", "n_measurements")


  dbDisconnect(con) # close connection

  return(
    list("parameters" = param_table,
         "rockTypes" = rock_table,
         "sites" = site_table)
  )

}
