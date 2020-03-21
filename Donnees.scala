import scala.math.sqrt
import scala.math.pow

class Donnees(private val valeurs: Array[Double], private val classe: String){

  def calculerDistance(centroide: Donnees): Double ={
    val n = this.valeurs.length
    var res: Double = 0
    for (i <- 0 until n) res = res + pow(this.valeurs(i) - centroide.getValeur(i), 2)
    sqrt(res)
  }

  def getClasse(): String = {
    this.classe
  }

  def getValeur(i: Int): Double = {
    this.valeurs(i)
  }

  def getValeurs(): Array[Double] = {
    this.valeurs
  }

  def getLength(): Int = {
    this.valeurs.length
  }


  override def toString(): String = {
    var str = "Valeurs: "
    for (value <- this.valeurs) str += value.toString + " "
    str + "Classe: "+this.classe
  }
}