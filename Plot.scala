import java.awt.Color

import javax.swing._

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.XYPlot
import org.jfree.data.xy.XYDataset
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection


class Plot (val var1: Int, val var2: Int, val kmeans: Kmeans) extends JFrame{
    //var1 et var2 sont les indices des variables que l'on souhaite utiliser pour le graphique

    def afficher(clusters: Array[Cluster]): Unit = {
        var dataset = new XYSeriesCollection()
        
        for (i <- 0 until clusters.length) {
            //ajout du centroide du cluster i
            var centroide = new XYSeries(s"centroide du cluster $i")
            centroide.add(clusters(i).getCentroide.getValeur(var1), clusters(i).getCentroide.getValeur(var2))
            dataset.addSeries(centroide)

            //ajout des donnees du cluster i
            var donnees = new XYSeries(s"Donnees du cluster $i")
            for (j <- clusters(i).getIndiceDonnees) {
                donnees.add(kmeans.getDonnees(j).getValeur(var1), kmeans.getDonnees(j).getValeur(var2))
            }
            dataset.addSeries(donnees)
        }

        var chart = ChartFactory.createScatterPlot("K-Means", s"Variable $var1", s"Variable $var2", dataset)
        var panel = new ChartPanel(chart)

        this.setContentPane(panel)

        this.setSize(1000, 1000)
        this.setLocationRelativeTo(null)
        this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
        this.setVisible(true)
    }
}