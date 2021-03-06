package mongodb

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import com.mongodb.spark._
import org.apache.spark.sql.SparkSession
import com.mongodb.spark._
import com.mongodb.spark.config._
import org.bson.Document
import org.apache.spark.sql.SQLContext
// it's aim to connect mongodb and make a fatjar with maven 
object mongodb {
    def main(args: Array[String]){
      val conf = new SparkConf().setAppName("simeple").setMaster("local[*]").set("spark.mongodb.input.uri","mongodb://192.168.111.128/mongodb_tutorial.output")
      .set("spark.mongodb.output.uri","mongodb://192.168.111.128/mongodb_tutorial.output1")
      val sc = new SparkContext(conf)
      val rdd = MongoSpark.load(sc)
      val output = rdd.map(x=>(x.getInteger("test"),1)).reduceByKey(_+_)
      val sqlContext = new SQLContext(sc) 
      import sqlContext.implicits._
      val df = output.toDF()
      MongoSpark.save(df) 
}
}