import Oraculo._
import Benchmark._
import ReconstCadenas._
import scala.util.Random
import ReconstCadenasPar._
import java.io.{BufferedWriter, FileWriter}

object Main {
    def main(args: Array[String]): Unit = {       

        val resultadosIngenuoMejorado = automaticBenchSmallIngenua(reconstruirCadenaIngenuo,List(),1,16)
        guardarResultadosEnArchivoIngenuo(resultadosIngenuoMejorado, "Ingenuo1to16.txt")
        println("termino ingenuo ")
        val resultadosMejoradoTurbo =automaticBenchLarge2(reconstruirCadenaMejorado,reconstruirCadenaTurbo,List(),0,1024)
        guardarResultadosEnArchivo(resultadosMejoradoTurbo, "MejoradovsTurbo0to1024.txt")
        //resultadosMejoradoTurbo.foreach(println)
        println("termino mejorado turbo")
        /*val resultadosTurboTurboMejorado =automaticBenchLarge(reconstruirCadenaTurbo,reconstruirCadenaTurboMejorado,List(),2,512)
        guardarResultadosEnArchivo(resultadosTurboTurboMejorado, "TvsTM2to512.txt")
        //resultadosTurboTurboMejorado.foreach(println)
        println("termino turbo mejorado")
        val resultadosTurboMejoradoTurboAcelerada =automaticBenchLarge(reconstruirCadenaTurboMejorado,reconstruirCadenaTurboAcelerada,List(),2,2048)
        guardarResultadosEnArchivo(resultadosTurboMejoradoTurboAcelerada, "TMvsTA2to2048.txt")
        //resultadosTurboMejoradoTurboAcelerada.foreach(println)
        println("termino turbo mejorado turbo acelerada")*/
    }

    val costoOraculo = 1
    val random = new Random()

    def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {

        if (s.length==long) s
        else {
            val indiceAzar=random.nextInt(4)
            secAlAzar(long,alfabeto(indiceAzar)+:s)
        }
    }

    def automaticBenchSmall(a1:Algoritmo,a2:Algoritmo,resultados:List[(Double,Double,Double)],aumentador:Int, max:Int): List[(Double,Double,Double)] = {
        if(aumentador > max){
            resultados
        } else {
            val s = secAlAzar(aumentador,Seq())
            val tam = s.length
            val o = crearOraculo(costoOraculo)(s)
            val resultado = compararAlgoritmos(a1,a2)(tam, o)
            println(resultado)
            automaticBenchSmall(a1,a2,resultados++List(resultado),aumentador+1,max)
        }        
    }
    
    def automaticBenchSmall2(a1:Algoritmo,a2:Algoritmo,resultados:List[(Double,Double,Double)],aumentador:Int, max:Int): List[(Double,Double,Double)] = {
        if(aumentador > max){
            resultados
        } else {
            val s = secAlAzar(aumentador,Seq())
            val tam = s.length
            val o = crearOraculo(costoOraculo)(s)
            val resultado = comparar2(a1,a2)(tam, o)
            println(resultado)
            automaticBenchSmall2(a1,a2,resultados++List(resultado),aumentador+1,max)
        }        
    }

    def automaticBenchSmallIngenua(a1:Algoritmo,resultados:List[(Double)],aumentador:Int, max:Int): List[(Double)] = {
        if(aumentador > max){
            resultados
        } else {
            val s = secAlAzar(aumentador,Seq())
            val tam = s.length
            val o = crearOraculo(costoOraculo)(s)
            val resultado = compararSola(a1)(tam, o)
            println(resultado)
            automaticBenchSmallIngenua(a1,resultados++List(resultado),aumentador+1,max)
        }        
    }
    def automaticBenchLarge(a1:Algoritmo,a2:Algoritmo,resultados:List[(Double,Double,Double)],aumentador:Int, max:Int): List[(Double,Double,Double)] = {
        println(aumentador)
        println(max)
        if(aumentador > max){
            resultados
        } else {
            val s = secAlAzar(aumentador,Seq())
            val tam = s.length
            val o = crearOraculo(costoOraculo)(s)
            val resultado = compararAlgoritmos(a1,a2)(tam, o)
            println(resultado)
            automaticBenchLarge(a1,a2,resultados++List(resultado),math.pow(aumentador,2).toInt,max)
        }        
    }

    def automaticBenchLarge2(a1:Algoritmo,a2:Algoritmo,resultados:List[(Double,Double,Double)],aumentador:Int, max:Int): List[(Double,Double,Double)] = {
        println(aumentador)
        println(max)
        val potencia = math.pow(2,aumentador).toInt
        if(potencia > max){
            resultados
        } else {
            val s = secAlAzar(potencia,Seq())
            val tam = s.length
            val o = crearOraculo(costoOraculo)(s)
            val resultado = comparar2(a1,a2)(tam, o)
            println(resultado)
            automaticBenchLarge2(a1,a2,resultados++List(resultado),aumentador + 1,max)
        }        
    }

    def guardarResultadosEnArchivo(resultados: List[(Double, Double, Double)], nombreArchivo: String): Unit = {
        val archivo = new BufferedWriter(new FileWriter(nombreArchivo))
        try {
            resultados.foreach { resultado =>
                archivo.write(resultado.toString())
                archivo.newLine()
                }
        } finally {
            archivo.close()
        }
    }

    def guardarResultadosEnArchivoIngenuo(resultados: List[(Double)], nombreArchivo: String): Unit = {
        val archivo = new BufferedWriter(new FileWriter(nombreArchivo))
        try {
            resultados.foreach { resultado =>
                archivo.write(resultado.toString())
                archivo.newLine()
                }
        } finally {
            archivo.close()
        }
    }

}
