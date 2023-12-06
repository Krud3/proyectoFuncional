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
}
