import common._ 
import Oraculo._
import ArbolSufijos._
import ReconstCadenas._
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.CollectionConverters._
package object ReconstCadenasPar {

    /**
      * Metodo encargado de validar las subcadenas de una lista de subcadenas
      * hasta que se encuentre la que cumpla con la condicion del oraculo
      *
      * @param n longitud de las subcadenas
      * @param o oraculo que se va a utilizar para validar las subcadenas
      * @param subCadenas lista de subcadenas que se van a validar
      * @return la subcadena que cumpla con la condicion del oraculo
      */
    def validarCadenasOraculo(o:Oraculo, subCadenas:Seq[Seq[Char]]): Seq[Char] = subCadenas match {            
        case Seq() => Seq()
        case x::xs => if (o(x)) x else validarCadenasOraculo(o, xs)            
    }

    def reconstruirCadenaIngenuoPar(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        } 
        
        else if (n < 4){
            reconstruirCadenaIngenuo(n, o)
        }
        else {
            def subcadenas(alfabeto: Seq[Char], longitud: Int): List[Seq[Char]] = {
                if (longitud == 0)
                    List(Seq.empty[Char])
                else {
                    val subcadenas_anteriores = subcadenas(alfabeto, longitud - 1)
                    val lista_subcadenas = for (
                        un_caracter <- alfabeto; una_subcadena <- subcadenas_anteriores
                        ) yield un_caracter +: una_subcadena
                    lista_subcadenas.toList
                }
            }
            val subcadenasListas = subcadenas(alfabeto, n)
            val sublistaSize = 4
            val sublistas = subcadenasListas.grouped(sublistaSize).toList

            val tareasParalelas = sublistas.map(sublista => task {
                validarCadenasOraculo(o, sublista)
                }
            )
            val resultados = tareasParalelas.map(_.join())
            resultados.filter(_.nonEmpty)(0)
        }
    }

    def reconstruirCadenaMejoradoPar(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        }
        else {
            def subcadenas(alfabeto: Seq[Char], longitud: Int): ParSeq[Seq[Char]] = {
                if (longitud == 0)
                    ParSeq(Seq.empty[Char])
                else {
                    val subcadenas_validas_anteriores = subcadenas(alfabeto, longitud - 1)
                    (for (
                            un_caracter <- alfabeto;
                            una_subcadena <- subcadenas_validas_anteriores;
                            values = un_caracter +: una_subcadena
                            if o(values)
                        ) yield values).par
                    
                }
            }
        subcadenas(alfabeto, n)(0)
        }
    }

    def reconstruirCadenaTurboPar(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        }
        else if (n == 1){
            reconstruirCadenaTurbo(n, o)
        }
        else {
        def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): ParSeq[Seq[Char]] = {
            if (longitud == 1)
                alfabeto.par.map(Seq(_))
            else {
                val subcadenas_validas_anteriores = subcadenasTurbo(alfabeto, longitud / 2)
                (for (
                        sub_cadena_1 <- subcadenas_validas_anteriores;
                        sub_cadena_2 <- subcadenas_validas_anteriores;
                        values = sub_cadena_1 ++ sub_cadena_2
                        if o(values)
                    ) yield values).par
                
            }
        }
        val cadenas = subcadenasTurbo(alfabeto, n)
        cadenas(0)
        }
    }

    def reconstruirCadenaTurboMejoradoPar(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
        Seq.empty[Char]
        }
        else if (n == 1){
            reconstruirCadenaTurboMejorado(n, o)
        }
        else {
            def son(s: Seq[Char], subsecuencias: ParSeq[Seq[Char]], l: Int): Boolean = {
                if (s.length == l + 1) {
                    true
                } else {
                    val test = s.slice(1, 1 + l)
                    if (subsecuencias.seq.contains(test)) {
                        son(s.drop(1), subsecuencias, l)
                    } else {
                        false
                    }
                }
            }
            def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): ParSeq[Seq[Char]] = {
                    if (longitud == 1)
                    (for(un_caracter <- alfabeto; if(o(Seq(un_caracter)))) yield Seq(un_caracter)).par
                    else {
                    val subcadenas_validas_anteriores = subcadenasTurbo(alfabeto, longitud / 2)
                    (for (
                        sub_cadena_1 <- subcadenas_validas_anteriores;
                        sub_cadena_2 <- subcadenas_validas_anteriores;
                        values = sub_cadena_1 ++ sub_cadena_2
                        if (son(values, subcadenas_validas_anteriores, longitud / 2))
                        if o(values)
                    ) yield values).par
                    
                    }
                }
        val cadenas = subcadenasTurbo(alfabeto, n)
        cadenas(0)
        }
    }

    def reconstruirCadenaTurboAceleradaPar(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        }
        else if (n <= 2){
            reconstruirCadenaTurboMejorado(n, o)
        }
        else {
            def son(s: Seq[Char], t: Trie, l: Int): Boolean = {
                if (s.length == l + 1) {
                    true
                } else {
                    val test = s.slice(1, 1 + l)
                    if (pertenece(test, t)) {
                        son(s.drop(1), t, l)
                    } else {
                        false
                    }
                }
            }
            def subCaddenasAlternativa(secuenciaInicial: ParSeq[Seq[Char]],arbolInicial : Trie, aumentador:Int): Seq[Char]= {
                if(n == aumentador){
                    secuenciaInicial(0) 
                }
                else{
                    val arbolNuevo: Trie = secuenciaInicial.foldLeft(arbolInicial) { (arbolActual, secuencia) =>
                            adicionar(secuencia, arbolActual)}
                    val lista_combinaciones = (for(
                        sub_cadena_1 <- secuenciaInicial;
                        sub_cadena_2 <- secuenciaInicial;
                        valor = sub_cadena_1 ++ sub_cadena_2
                        if(son(valor, arbolNuevo, aumentador))
                        if(o(valor))
                    )yield valor).par
                    subCaddenasAlternativa(lista_combinaciones, arbolNuevo, aumentador*2)
                }
            }
            val secuenciaInicial= (for(un_caracter <- alfabeto; if(o(Seq(un_caracter)))) yield Seq(un_caracter)).par
            val arbolInicial = arbolDeSufijos(secuenciaInicial.seq)
            val cadenas = subCaddenasAlternativa(secuenciaInicial,arbolInicial, 1)
            cadenas
        }
    }
}