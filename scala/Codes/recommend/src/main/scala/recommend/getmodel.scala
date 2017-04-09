package recommend
import java.io.File

import scala.io.Source

import org.apache.log4j.Logger
import org.apache.log4j.Level

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd._
import org.apache.spark.mllib.recommendation.{ALS, Rating, MatrixFactorizationModel}
object getmodel {
  def main(args:Array[String]){
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    Logger.getLogger("org.eclipse.jetty.server").setLevel(Level.OFF)
  val conf = new SparkConf()
      .setAppName("MovieLensALS").setMaster("local[*]")
      .set("spark.executor.memory", "2g")
    val sc = new SparkContext(conf)
      val movieLensHomeDir = "C:/Users/evalue/git/movie_lens/data"
   val myRatings = loadRatings("C:/Users/evalue/git/movie_lens/data/myratings.txt")
  val movies = sc.textFile(new File(movieLensHomeDir, "u.item").toString).map { line =>
      val fields = line.split("\t")
      // format: (movieId, movieName)
      (fields(0).toInt, fields(1))
    }.collect().toMap
  val sameModel = MatrixFactorizationModel.load(sc, "./data/results")
  
    val myRatedMovieIds = myRatings.map(_.product).toSet
    val candidates = sc.parallelize(movies.keys.filter(!myRatedMovieIds.contains(_)).toSeq)
    println("candidates's list")
    candidates.foreach(println _)
  val recommendations = sameModel
      .predict(candidates.map((196, _)))
      .collect()
      .sortBy(- _.rating)
      .take(50)
    var i = 1
    println("Movies recommended for you:")
    recommendations.foreach { r =>
      println("%2d".format(i) + ": " + movies(r.product))
      i += 1
    }
    
    
}
  def loadRatings(path: String): Seq[Rating] = {
    val lines = Source.fromFile(path).getLines()
    val ratings = lines.map { line =>
      val fields = line.split("\t")
      Rating(fields(0).toInt, fields(1).toInt, fields(2).toDouble)
    }.filter(_.rating > 0.0)
    if (ratings.isEmpty) {
      sys.error("No ratings provided.")
    } else {
      ratings.toSeq
      }}}