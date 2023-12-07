import Oraculo._
import org.scalameter._
import ReconstCadenas._

package object Benchmark {
    //type Oraculo = Seq[Char] => Boolean
    type Algoritmo = (Int, Oraculo) => Seq[Char]

    def compararAlgoritmos(a1:Algoritmo,a2:Algoritmo)(tam:Int,o:Oraculo):(Double,Double,Double) = {
        
        val timeA1 = config(
            KeyValue(Key.exec.minWarmupRuns -> 20),
            KeyValue(Key.exec.maxWarmupRuns -> 60),
            KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure {a1(tam,o)}

        val timeA2 = config(
            KeyValue(Key.exec.minWarmupRuns -> 20),
            KeyValue(Key.exec.maxWarmupRuns -> 60),
            KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure {a2(tam,o)}

        

        val speedUp = timeA1.value / timeA2.value
        
        (timeA1.value,timeA2.value,speedUp)
    }

    def comparar2(a1:Algoritmo,a2:Algoritmo)(tam:Int,o:Oraculo):(Double,Double,Double) = {
        
        
        val inicio1 = System.nanoTime()
        a1(tam,o)
        val fin1 = System.nanoTime()
        val tiempo1 = (fin1 - inicio1)
        val tiempoSeg1 = tiempo1/1e9d
        
        val inicio2 = System.nanoTime()
        a2(tam,o)
        val fin2 = System.nanoTime()
        val tiempo2 = (fin2 - inicio2)
        val tiempoSeg2 = tiempo2/1e9d

        

        val speedUp = tiempoSeg1 / tiempoSeg2
        
        (tiempoSeg1,tiempoSeg2,speedUp)
    }

    def compararSola(a1:Algoritmo)(tam:Int,o:Oraculo):(Double) = {
        
        
        val inicio1 = System.nanoTime()
        a1(tam,o)
        val fin1 = System.nanoTime()
        val tiempo1 = (fin1 - inicio1)
        val tiempoSeg1 = tiempo1/1e9d

        
        (tiempoSeg1)
    }
}
