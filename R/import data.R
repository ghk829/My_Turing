library(DBI)
library(rJava)
options(java.parameters = "-Xmx4g")
library(RJDBC)
library(dplyr)
# library : hive-jdbc, hadoop-common, hive-exec,hive-service,httpclient,
#httpcore,log4j-api,log4j-slf4j,slf4j-api
hive.class.path = list.files(path=c("F:/lib"), pattern="jar", full.names=T);
hadoop.lib.path = list.files(path=c("F:/hadoop/share/hadoop/common"), pattern="jar", full.names=T);
hadoop.class.path = list.files(path=c("F:/hadoop/share/hadoop/tools/lib"), pattern="jar", full.names=T);
class.path = c(hive.class.path, hadoop.lib.path, hadoop.class.path); 
.jinit(classpath = class.path)
drv <- JDBC("org.apache.hive.jdbc.HiveDriver")
conn <- dbConnect(drv, "jdbc:hive2://100.100.100.21:10000/", "", "")
# Query
db_qry <- dbGetQuery(conn, 
                     "select * from testhivedrivertable limit 100")

db_qry # data_frame
