object TestStats {
    def main(args: Array[String]): Unit = {
        val k = new Kmeans()
        k.initMatriceDonnees("iris.data")
        k.afficherStats()
    }
}