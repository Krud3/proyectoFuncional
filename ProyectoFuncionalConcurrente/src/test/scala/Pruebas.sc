import Oraculo._
import Benchmark._
import ReconstCadenas._
import scala.util.Random
import ReconstCadenasPar._

val costoOraculo = 1
val random = new Random()

//funcion encargada de generar secuencias al azar
def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {

    if (s.length==long) s
    else {
        val indiceAzar=random.nextInt(4)
        secAlAzar(long,alfabeto(indiceAzar)+:s)
    }
}

//funcion encargada de hacer un bench automatico para dos funciones iterando en potencias de 2
def automaticBenchPotencias(a1:Algoritmo,a2:Algoritmo,resultados:List[(Double,Double,Double)],aumentador:Int, max:Int): List[(Double,Double,Double)] = {
    val potencia = math.pow(2,aumentador).toInt
    if(potencia > max){
        resultados
    } else {
        val s = secAlAzar(potencia,Seq())
        val tam = s.length
        val o = crearOraculo(costoOraculo)(s)
        val resultado = comparar2(a1,a2)(tam, o)
        automaticBenchPotencias(a1,a2,resultados++List(resultado),aumentador + 1,max)
    }        
}

//funcion encargada de hacer un bench automatico para una sola funcion iterando de 1 en 1
def automaticBenchIndividual(a1:Algoritmo,resultados:List[(Double)],aumentador:Int, max:Int): List[(Double)] = {
    if(aumentador > max){
        resultados
    } else {
        val s = secAlAzar(aumentador,Seq())
        val tam = s.length
        val o = crearOraculo(costoOraculo)(s)
        val resultado = compararSola(a1)(tam, o)
        automaticBenchIndividual(a1,resultados++List(resultado),aumentador+1,max)
    }        
}

/**
     * Funcion auxiliar encargada de comparar dos secuencias de caracteres
     * @param seq1 Secuencia de caracteres
     * @param seq2 Secuencia de caracteres
     * @return True si son iguales, False si no lo son
     */
def compareSeqs(seq1:Seq[Char],seq2:Seq[Char]):Boolean = {
    if (seq1.length != seq2.length) false
    else {
        if (seq1.isEmpty) true
        else {
            if (seq1.head == seq2.head) compareSeqs(seq1.tail,seq2.tail)
            else false
        }
    }
}

///---------------------------------------ZONA DE PRUEBAS---------------------------------------///

val sec1=Seq('a', 'c', 'c', 'a')
val sec2 = Seq('a', 'c', 'g', 'c', 'a')
// TENER CUIDADO DON EL TAMANIO DE SEC3 PORQUE PUEDE AFECTAR A LAS TURBO EN ADELANTE
// Se puede usar valores grandes de la forma 2^n pero comentar las funciones lentas
val sec3=secAlAzar(4,Seq())
val or_1=crearOraculo(costoOraculo)(sec1)
val or_2=crearOraculo(costoOraculo)(sec2)
val or_3=crearOraculo(costoOraculo)(sec3)

val ing1 = reconstruirCadenaIngenuo(sec1.length, or_1)
val ing2 = reconstruirCadenaIngenuo(sec2.length, or_2)
val ing3 = reconstruirCadenaIngenuo(sec3.length, or_3)


val ingPar1 = reconstruirCadenaIngenuoPar(4)(sec1.length, or_1)
val ingPar2 = reconstruirCadenaIngenuoPar(4)(sec2.length, or_2)
val ingPar3 = reconstruirCadenaIngenuoPar(4)(sec3.length, or_3)

val mejo1 = reconstruirCadenaMejorado(sec1.length, or_1)
val mejo2 = reconstruirCadenaMejorado(sec2.length, or_2)
val mejo3 = reconstruirCadenaMejorado(sec3.length, or_3)

val mejoPar1 = reconstruirCadenaMejoradoPar(1)(sec1.length, or_1)
val mejoPar2 = reconstruirCadenaMejoradoPar(1)(sec2.length, or_2)
val mejoPar3 = reconstruirCadenaMejoradoPar(1)(sec3.length, or_3)

val turbo1 = reconstruirCadenaTurbo(sec1.length, or_1)
val turbo3 = reconstruirCadenaTurbo(sec3.length, or_3)

val turboPar1 = reconstruirCadenaTurboPar(1)(sec1.length, or_1)
val turboPar3 = reconstruirCadenaTurboPar(1)(sec3.length, or_3)

val turboMejo1 = reconstruirCadenaTurboMejorado(sec1.length, or_1)
val turboMejo3 = reconstruirCadenaTurboMejorado(sec3.length, or_3)

val turboMejoPar1 = reconstruirCadenaTurboMejoradoPar(1)(sec1.length, or_1)
val turboMejoPar3 = reconstruirCadenaTurboMejoradoPar(1)(sec3.length, or_3)

val turboAce1 = reconstruirCadenaTurboAcelerada(sec1.length, or_1)
val turboAce3 = reconstruirCadenaTurboAcelerada(sec3.length, or_3)

val turboAcePar1 = reconstruirCadenaTurboAceleradaPar(2)(sec1.length, or_1)
val turboAcePar3 = reconstruirCadenaTurboAceleradaPar(2)(sec3.length, or_3)

///COMPARACION RESULTADOS///
/**************para comparar resultados usar compareSeqs**************/
compareSeqs(ing1,sec1)
compareSeqs(ing2,sec2)
compareSeqs(ing3,sec3)

compareSeqs(ingPar1,sec1)
compareSeqs(ingPar2,sec2)
compareSeqs(ingPar3,sec3)

compareSeqs(mejo1,sec1)
compareSeqs(mejo2,sec2)
compareSeqs(mejo3,sec3)

compareSeqs(mejoPar1,sec1)
compareSeqs(mejoPar2,sec2)
compareSeqs(mejoPar3,sec3)

compareSeqs(turbo1,sec1)
compareSeqs(turbo3,sec3)

compareSeqs(turboPar1,sec1)
compareSeqs(turboPar3,sec3)

compareSeqs(turboMejo1,sec1)
compareSeqs(turboMejo3,sec3)

compareSeqs(turboMejoPar1,sec1)
compareSeqs(turboMejoPar3,sec3)

compareSeqs(turboAce1,sec1)
compareSeqs(turboAce3,sec3)

compareSeqs(turboAcePar1,sec1)
compareSeqs(turboAcePar3,sec3)

///BENCHMARKS///

//ajustar tamanio al deseado para el benchmark
val tamanio = 4
val maximaPotencia = 8 //ajustar al deseado para el benchmark

//Benchmarks para reconstruirCadenaIngenuo

automaticBenchIndividual(reconstruirCadenaIngenuo,List(),1,tamanio)
automaticBenchIndividual(reconstruirCadenaIngenuoPar(4),List(),1,tamanio)


//Benchmarks para reconstruirCadenaMejorado vs reconstruirCadenaTurbo
automaticBenchPotencias(reconstruirCadenaMejorado,reconstruirCadenaTurbo,List(),0,maximaPotencia)

//Benchmarks para reconstruirCadenaTurboMejorado vs reconstruirCadenaTurboAcelerada
automaticBenchPotencias(reconstruirCadenaTurboMejorado,reconstruirCadenaTurboAcelerada,List(),0,maximaPotencia)

//Benchmarks para reconstruirCadenaMejorado vs reconstruirCadenaMejoradoPar
automaticBenchPotencias(reconstruirCadenaMejorado,reconstruirCadenaMejoradoPar(1),List(),0,maximaPotencia)

//Benchmarks para reconstruirCadenaMejoradoPar vs reconstruirCadenaTurboPar
automaticBenchPotencias(reconstruirCadenaMejoradoPar(1),reconstruirCadenaTurboPar(1),List(),0,maximaPotencia)

//Benchmarks para reconstruirCadenaTurboMejoradoPar vs reconstruirCadenaTurboAceleradoPar
automaticBenchPotencias(reconstruirCadenaTurboMejoradoPar(1),reconstruirCadenaTurboAceleradaPar(2),List(),0,maximaPotencia)
