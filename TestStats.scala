object TestStats {
    def main(args: Array[String]): Unit = {
        val kmeans = new Kmeans()
        kmeans.initMatriceDonnees("iris.data")
        kmeans.afficherStats()
    }
}