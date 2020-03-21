import scala.io.Source

class MatriceDonnees(private var matrice: Array[Donnees]){
    override def toString() : String = {
        var str = ""
        for (donnees <- this.matrice) str += donnees.toString +"\n"
        str
    }

    def getDonnees(i: Int): Donnees = {
        this.matrice(i)
    }

    def getLength(): Int = {
        this.matrice.length
    }

    def getClasses(): Array[String] = {
        var classes: Array[String] = Array()
        for(donnees <- matrice){
            classes :+= donnees.getClasse
        }
        classes
    } 
}