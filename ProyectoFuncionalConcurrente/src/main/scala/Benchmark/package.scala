import Oraculo._
import org.scalameter._
import ReconstCadenas._

package object Benchmark {
    //type Oraculo = Seq[Char] => Boolean
    type Algoritmo = (Int, Oraculo) => Seq[Char]

    /**
      * Funcion encargada de comparar dos algoritmos
      * 
      * @param a1 Algoritmo 1
      * @param a2 Algoritmo 2
      * @param tam Tamaño de la cadena a reconstruir
      * @param o Oraculo
      * @return Tupla con el tiempo de ejecucion de cada algoritmo y el speedUp
      */
    def comparar2(a1:Algoritmo,a2:Algoritmo)(tam:Int,o:Oraculo):(Double,Double,Double) = {
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

    /**
      * Funcion encargada de comparar un algoritmo solo
      *
      * @param a1  Algoritmo 1
      * @param tam Tamaño de la cadena a reconstruir
      * @param o  Oraculo
      * @return Tiempo de ejecucion del algoritmo
      */
    def compararSola(a1:Algoritmo)(tam:Int,o:Oraculo):(Double) = {
        val timeA1 = config(
            KeyValue(Key.exec.minWarmupRuns -> 20),
            KeyValue(Key.exec.maxWarmupRuns -> 60),
            KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure {a1(tam,o)}

        val result = timeA1.value 
        result
    }
}
