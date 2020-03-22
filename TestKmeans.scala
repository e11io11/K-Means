object TestKmeans {
    def main(args: Array[String]): Unit = {
        val k = new Kmeans()
        k.initMatriceDonnees("iris.data")

        val res = k.calculerKmeans(3)
        println("res = ["+res.mkString(", ")+"]")
        k.comparerResultats(res)
    }
}