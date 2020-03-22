object TestKmeans {
    def main(args: Array[String]): Unit = {
        val kmeans = new Kmeans()
        kmeans.initMatriceDonnees("iris.data")

        val res = kmeans.calculerKmeans(3)
        println("res = ["+res.mkString(", ")+"]")
        kmeans.comparerResultats(res)
    }
}