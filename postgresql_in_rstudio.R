# To access a postgresql server using RStudio and a built-in odbc:
# - Install the postgresql odbc file for your OS (this is to be installed in the OS and not from
#   within R). E.g. for Windows, download the latest ODBC 
#   driver from https://www.postgresql.org/ftp/odbc/versions/msi/
# - Install the DBI and odbc libraries for R
# - In RStudio, click the Connections tab
# - Create a New Connection. You should see the installed PostgreSQL drivers. 
#   Pick the first one on the list (PostgreSQL ANSI(x64))
# - Replace the Connection string with something like:

con <- DBI::dbConnect(odbc::odbc(),
                      Driver    = "PostgreSQL ANSI(x64)", 
                      Server    = "<server name>",
                      Database  = "<database name>",
                      UID       = "<user name>",
                      PWD       = rstudioapi::askForPassword("password"))

# - Click Test to check the connnection
# - If the Test is successful, click OK
# - You should see a list of available tables in the Connection pane

