package test
import breeze.linalg._
import breeze.plot._
import scala.math._
// It's aim to visualize data, but, I prefer to use plotly instead of breeze-viz
object test {
  def main(args:Array[String]){
  val f = Figure()
  val p = f.subplot(0)
  val x = linspace(1.0,4*Pi,100)
  val cosx = x map(xs => cos(xs))
  val sincosx = x map(xs=> sin(cos(xs)))
  p += plot(x, cosx :^ 2.0)
  p += plot(x, sincosx :^ 3.0, '.')
  p.xlabel = "x axis"
  p.ylabel = "y axis"
  f.saveas("lines.png") 
  }
}
// For Using plotly
//import co.theasi.plotly._

/*implicit val server = new writer.Server {
  val credentials = writer.Credentials("<username>", "<api_key>")
  val url = "https://api.plot.ly/v2/"
}*/