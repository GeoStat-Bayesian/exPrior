## wwhypdaConnect ##

#'extract data from wwhypda database
#'
#'\code{wwhypdaConnect} connects to locally-stored wwhypda database with your password
#'@param password your password
#'@param db_name the name of the local wwhypda database
#'@return a connection to locally-stored wwhypda database
#'@examples
#'wwhypdaConnect(password='mypassword',db_name='mydbname')
#'@export
wwhypdaConnect <- function(password,db_name)
{
  return (RMySQL::dbConnect(RMySQL::MySQL(),
                    dbname = db_name,
                    username = "root",
                    host = "localhost",
                    password = password)) #CHANGE HERE
}

