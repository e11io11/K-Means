import scala.io.Source
import scala.math.pow
import scala.math.sqrt
import scala.util.Random.nextInt

class Kmeans(){
    private var matriceDonnees: MatriceDonnees = _

    def initMatriceDonnees(f ng): Unit={
        var m : Array[Donnees] = Array()
        for (line <- Source.fromFile(file).getLines) {
            val arr = line.split(",")
            m = m :+ new Donnees(arr.dropRight(1).map(_.toDouble), arr(arr.length-1))
        }
        this.matriceDonnees = new MatriceDonnees(m)
    }


    def calculerKmeans(k: Int): Array[Int] = {
        var clusters: Array[Cluster] = Array()
        val n = this.matriceDonnees.getDonnees(0).getLength()
        val m = this.matriceDonnees.getLength()
        for (i <- 0 until k) {
            var centroide: Array[Double] = Array()
            for (j <- 0 until n) {
                centroide = centroide :+ this.matriceDonnees.getDonnees(nextInt(m)).getValeur(j)
            }
            clusters = clusters :+ new Cluster(new Donnees(centroide, ""))
        }
        var count = 0
        while (count<10) {
            //println("ITERATION : "+count)
            for (cluster <- clusters) cluster.resetIndiceDonnees
            for (i <- 0 until m) {
                var distances: Array[Double] = Array()
                for (j <- 0 until k) {
                    //println("k = "+j+" distance = "+this.matriceDonnees.getDonnees(i).calculerDistance(clusters(j).getCentroide))
                    distances = distances :+ this.matriceDonnees.getDonnees(i).calculerDistance(clusters(j).getCentroide)                    
                }
                clusters(indiceMin(distances)).ajouterIndice(i)
            }
            //for(cluster <- clusters) println("\n"+cluster.getIndiceDonnees.mkString(" "))
            //updating centroides
            for (i <- 0 until k) {
                var centroide: Array[Double] = Array()
                var clusterData = clusters(i).getIndiceDonnees
                if (clusterData.length > 0) {
                    for (j <- 0 until n) {
                        var moyenne: Double = 0
                        for (indice <- clusterData) moyenne += this.matriceDonnees.getDonnees(indice).getValeur(j)
                        centroide = centroide :+ moyenne/clusterData.length
                    }
                    clusters(i).setCentroide(new Donnees(centroide, ""))
                }
            }
            count+=1
        }
        var res: Array[Int] = Array()
        for (i <- 0 until m) {
            var j=0
            while (!clusters(j).getIndiceDonnees.contains(i)) {
                j+=1
            }
            res :+= j
        }

        res   
    } 


    def indiceMin(tab: Array[Double]): Int = {
        var min = tab(0)
        var imin = 0
        for (i <- 1 until tab.length){
            if (tab(i) < min){
                imin = i
                min = tab(i)
            }
        }
        imin
    }

    def getDonnees(i: Int): Donnees = {
        this.matriceDonnees.getDonnees(i)
    }

    def viewMatriceDonnees(): Unit = {
        println(this.matriceDonnees)
    }

    def calculerMoyenne(i: Int): Double = {
        var res: Double = 0
        for (j <- 0 until this.matriceDonnees.getLength) res += this.matriceDonnees.getDonnees(j).getValeur(i)
        res/this.matriceDonnees.getLength 
    }

    def calculerVariance(i: Int): Double = {
        val moyenne = calculerMoyenne(i)
        var res: Double = 0
        for (j <- 0 until this.matriceDonnees.getLength) res += pow(this.matriceDonnees.getDonnees(j).getValeur(i) - moyenne, 2).toDouble
        res/this.matriceDonnees.getLength 
    }

    def calculerEcartType(i: Int): Double = {
        sqrt(calculerVariance(i))
    }

    def afficherStats(): Unit = {
        println("STATISTIQUES :")
        val n = this.matriceDonnees.getDonnees(0).getLength()
        for (i <- 0 until n){
            println("\nvariable "+(i+1).toString()+" :")
            println("moyenne = "+calculerMoyenne(i))
            println("variance = "+calculerVariance(i))
            println("ecart type = "+calculerEcartType(i))
        }
    }
    
}