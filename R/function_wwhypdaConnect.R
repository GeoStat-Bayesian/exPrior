## wwhypdaConnect ##

#'extract data from wwhypda database
#'
#'\code{wwhypdaConnect} connects to locally-stored wwhypda database with your password
#'@param password your password
#'@return a connection to locally-stored wwhypda database
#'@examples
#'wwhypdaConnect('mypassword')
#'@export
wwhypdaConnect <- function(password)
{
  return (RMySQL::dbConnect(RMySQL::MySQL(),
                    dbname = "wwhypda_berkeley",
                    username = "root",
                    host = "localhost",
                    password = password)) #CHANGE HERE
}

