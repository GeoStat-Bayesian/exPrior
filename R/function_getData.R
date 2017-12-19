## getData ##
#'extract data from wwhypda database
#'
#'\code{getData} queries the wwhypda database according to specifications
#'of site name, rock type, and parameter of interest and returns data as a dataframe.
#'
#'@param rockType a character indicating rock type. if left blank, data from all rock types returned.
#'@param param a character indicating parameter. if left blank, data from all parameters returned.
#'@param viewInfo logical; if TRUE, calls viewInfor(), returns list of rock types, parameters, and sites
#'@param for_generalFromMeas logical; if TRUE returns a dataframe called meas to be used as input for function generalFromMeas()
#'@return data queried from the wwhypda database as a dataframe
#'@examples
#'my_data <- getData(rockType = "Sandstone, channel", param = "porosity")
#'head(my_data)
#'
#'all_data <- getData()
#'
#'data_info <- getData(viewInfo=TRUE)
#'
#'lagenthal <- getData(site = "Langenthal")
#'dim(lagenthal)
#'
#'porosity_of_sandstone <- getData(rockType = "Sandstone", param = "porosity")
#'
#'# this should raise an error:
#'site_unavailable <- getData(site = "U.C. Berkeley")
#'@export
getData <- function(rockType=NULL,
                    param=NULL,
                    site=NULL,
                    viewInfo = FALSE,
                    for_generalFromMeas = FALSE)
{
  # make sure RSQLite is loaded
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # connect to wwhypda sqlite
  # ===========================================================================

  con = dbConnect(SQLite(),
                  dbname="../data/wwhypda.sqlite")

  # sanity checks: ensure that rock type, parameter, and site are valid
  # ===========================================================================
  info <- rPrior::viewInfo()

  if (!(is.null(rockType)) && !(rockType %in% info$rockTypes[,1]))
    stop (paste(rockType, "not in database. use viewInfo()=TRUE to see available rock types!"))

  for(s in site)
  {
    if (!(is.null(s)) && !(s %in% info$sites[,1]))
      stop (paste(s, "not in database. use viewInfo()=TRUE to see available sites!"))
  }

  if (!(is.null(param)) && !(param %in% info$parameters[,1]))
    stop (paste(param, "not in database. use viewInfo()=TRUE to see available parameters!"))


  # extract data
  # ===========================================================================
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

  basic_data <- dbGetQuery(con, basic_query)

  # # all rock types
  # rocktypes <- dbGetQuery(con,
  #                         "select distinct rt_name from rockType;")
  # # all params
  # params <- dbGetQuery(con,
  #                      "select distinct param_name from parameter;")
  #
  # # all sites
  # sites <- dbGetQuery(con,
  #                     "select distinct site_name from site_info;")
  #
  # #sites <- unique(basic_data$site_name, na.rm=T)
  #

  # specifying rock type, param, site
  # ===========================================================================

  if ( !(is.null(rockType)) ){basic_data <- basic_data[which(basic_data$rt_name %in% rockType),]}
  if ( !(is.null(param)) ){basic_data <- basic_data[which(basic_data$param_name %in% param),]}
  if ( !(is.null(site)) ){basic_data <- basic_data[which(basic_data$site_name %in% site),]}

  #
  # ===========================================================================

  # modify type of site_id to characters
  basic_data$site_id <- as.character(basic_data$site_id)

  # modify name msr_value to val
  names(basic_data)[which(names(basic_data)=="msr_value")] <- "val"

  # add notation for parameters: K for conductivity, n for porosity etc
  basic_data$param_not <- basic_data$param_name
  basic_data$param_not[which(basic_data$param_not == "hydraulic conductivity")] <- "K"
  basic_data$param_not[which(basic_data$param_not == "porosity")] <- "n"
  basic_data$param_not[which(basic_data$param_not == "effective porosity")] <- "ne"

  # close connection
  dbDisconnect(con) # close connection

  if(viewInfo)
  {
    return(list(
      "data" = basic_data,
      "info" = info
    ))
  }
  if(for_generalFromMeas)
  {
    library(dplyr)
    meas <- basic_data %>%
      select(val, site_name) %>%
      #mutate(val = log(val)) %>% # if hydraulic conductivity
      rename(site_id = site_name)
    return(meas)
  }
  else
  {
    return(basic_data)
  }

}

