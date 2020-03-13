class Cluster(private var centroide: Donnees){
    private var indiceDonnees: Array[Int] = Array()

    def ajouterIndice(i: Int): Unit = {
        this.indiceDonnees = this.indiceDonnees :+ i
    }

    def getCentroide(): Donnees = {this.centroide}

    def setCentroide(centroide: Donnees): Unit = {
        this.centroide = centroide
    }

    def getIndiceDonnees(): Array[Int] = {
        this.indiceDonnees
    }

    def getIndiceDonneesVal(i: Int): Int = {
        this.indiceDonnees(i)
    }

    def resetIndiceDonnees(): Unit = {
        this.indiceDonnees = Array()
    }
}