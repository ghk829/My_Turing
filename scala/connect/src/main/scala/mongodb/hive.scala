package mongodb

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.sql.SQLContext
import com.mongodb.spark._
import org.apache.spark.sql.Row

object hive {
  def main(args:Array[String]){
    val conf = new SparkConf().setAppName("simeple").setMaster("local[*]")
    val sc = new SparkContext(conf)
    val sqlContext = new org.apache.spark.sql.SQLContext(sc)
    val url = "jdbc:hive2://100.100.100.21:10000"
    val user = "root"
    val password = "root123"
    val dbtable = "testhivedrivertable"
    val data = sqlContext.read.format("jdbc").options(Map("url" -> url,
    "user" -> user,                                                               
    "password" -> password,                                                       
    "dbtable" -> dbtable)).load()
    data.registerTempTable("names")
    import sqlContext.implicits._
    println(data.sqlContext.sql("select * from names"))
  }
}