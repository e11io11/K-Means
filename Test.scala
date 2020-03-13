object Test{
    def main(args: Array[String]): Unit = {
        val k = new Kmeans()
        k.initMatriceDonnees("iris.data")
        //k.viewMatriceDonnees

        /*
        val d0 = k.getDonnees(0)
        val d1 = k.getDonnees(1)
        println(d0)
        println(d1)
        println(d0.calculerDistance(d1))
        println(k.calculerMoyenne(0))
        println(k.calculerVariance(0))
        println(k.calculerEcartType(0))
        */

        //k.afficherStats()

        println("["+k.calculerKmeans(3).mkString(", ")+"]")
    }
}