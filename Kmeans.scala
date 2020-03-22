import scala.io.Source
import scala.math.pow
import scala.math.sqrt
import scala.util.Random.nextInt
import scala.collection.mutable.Map

class Kmeans(){
    private var matriceDonnees: MatriceDonnees = _

    def initMatriceDonnees(file: String): Unit={
        var m : Array[Donnees] = Array()
        for (line <- Source.fromFile(file).getLines) {
            val arr = line.split(",")
            //on cree un objet donnee avec les valeurs numerique et la classe
            m = m :+ new Donnees(arr.dropRight(1).map(_.toDouble), arr(arr.length-1))
        }
        this.matriceDonnees = new MatriceDonnees(m)
    }


    def calculerKmeans(k: Int): Array[Int] = {
        // n = nombre de variables d'une ligne de donnees; m = nombre de lignes de donnees
        val n = this.matriceDonnees.getDonnees(0).getLength()
        val m = this.matriceDonnees.getLength()

        //creation des clusters et centroides
        var clusters: Array[Cluster] = Array()
        for (i <- 0 until k) {
            var centroide: Array[Double] = Array()
            for (j <- 0 until n) {
                //on prend aleatoirement une valeur dans la matrice
                centroide = centroide :+ this.matriceDonnees.getDonnees(nextInt(m)).getValeur(j)
            }
            clusters = clusters :+ new Cluster(new Donnees(centroide, ""))
        }

        var count = 0
        while (count<1000) {
            //on vide les clusters de leur donnees
            for (cluster <- clusters) cluster.resetIndiceDonnees

            //affectation des donnees aux clusters en fonction de leur distance au centroide
            for (i <- 0 until m) {
                //on calcul la distance d'une donnees a chaque centroide, puis determine la plus petite
                var distances: Array[Double] = Array()
                for (j <- 0 until k) {
                    distances = distances :+ this.matriceDonnees.getDonnees(i).calculerDistance(clusters(j).getCentroide)                    
                }
                clusters(indiceMin(distances)).ajouterIndice(i)
            }

            //mise a jour des centroides
            for (i <- 0 until k) {
                var centroide: Array[Double] = Array()
                var clusterData = clusters(i).getIndiceDonnees
                if (clusterData.length > 0) {  //on regarde si le cluster n'est pas vide
                    //on calcule la moyenne des donnees du cluster pour chaques variables
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

        //affichage
        var affichage = new Plot(2, 3, this)
        affichage.afficher(clusters)

        //construction du tableau resultat
        var res: Array[Int] = Array()
        for (i <- 0 until m) {
            var j=0
            while (!clusters(j).getIndiceDonnees.contains(i)) {
                j+=1
            }
            res :+= j
        }
        //la case i de res contient le numero du cluster auquel la donnee d'indice i appartient
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

    def getMatriceDonnees(): MatriceDonnees = {
        this.matriceDonnees
    }

    def getDonnees(i: Int): Donnees = {
        this.matriceDonnees.getDonnees(i)
    }

    def viewMatriceDonnees(): Unit = {
        println(this.matriceDonnees)
    }

    //Pour les fonction suivantes, i correspond a l'indice du groupe de variable sur lequel on veux effectuer les calcul
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


    def calculerCovariance(i: Int, j: Int): Double = {
        val moyenneI = calculerMoyenne(i)
        val moyenneJ = calculerMoyenne(j)
        val n = this.matriceDonnees.getLength
        var res: Double = 0
        for (k <- 0 until n) res += (this.getDonnees(k).getValeur(i) - moyenneI)*(this.getDonnees(k).getValeur(j) - moyenneJ)
        res/n
    }


    def calculerCoeffCorrelation(i: Int, j: Int): Double = {
        calculerCovariance(i, j)/(calculerEcartType(i)*calculerEcartType(j))
    }


    def comparerResultats(res: Array[Int]): Unit = {
        val n = res.length
        val distinctClasses = this.matriceDonnees.getClasses.distinct
        val clusterId: Array[Int] = res.distinct
        //ClusterData va contenir le nombre d'éléments de chaque classes dans chaque clusters
        var clusterData: Map[Int, Map[String, Int]] = Map()
        //ClusterSize va contenir le nombre d'éléments total de chaque clusters
        var clusterSize: Map[Int, Int] = Map()

        for (id <- clusterId) {
            //On construit clusterData
            var m: Map[String, Int] = Map()
            for (classeName <- distinctClasses) {
                m += (classeName -> 0)
            }
            clusterData += (id -> m)

            //On construit clusterSize
            clusterSize += (id -> 0)
        }

        //On parcours la matrice pour remplir clusterData et clusterSize 
        for(i <- 0 until n) {
            //On remplit clusterSize
            clusterSize(res(i)) = clusterSize(res(i)) + 1
            //On remplit clusterData
            val donnees = getDonnees(i)
            clusterData(res(i))(donnees.getClasse) += 1
            //println(s"cluster ${res(i)} ${donnees.getClasse} = ${clusterData(res(i))(donnees.getClasse)}")
        }


        //Calcul et affichage des pourcentages d'apparition de chaque classe pour chaque clusters
        for((cluster, data) <- clusterData) {
            println(s"\nCluster numéro $cluster contient :")
            for((classe, n) <- data) {
                val pourcent = (n/clusterSize(cluster).toDouble)*100
                println(s"${pourcent}% de $classe")
            }
        }     
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
        println("")
        for (i <- 0 until n) {
            for (j <- i+1 until n) {
                println(s"coefficient de correlation des variables ${i+1} et ${j+1} = ${calculerCoeffCorrelation(i, j)}")
            }
        }
    }
    
}