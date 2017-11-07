

# extracts data from wwhypda

df_wwhypda_K <-
  rPrior::getData(param = "hydraulic conductivity",
                   password = '<password>',db_name = 'db_wwhypda')
devtools::use_data(df_wwhypda_K)
