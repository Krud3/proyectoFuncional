import common._ 
import Oraculo._
import ReconstCadenas._
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.CollectionConverters._
package object ReconstCadenasPar {

    /**
      * Metodo encargado de reconstruir una cadena a partir de un oraculo.
      * Utilizando fuerza bruta, llegando a si a todas las combinaciones
      * del alfabeto para el tamaño dado y luego comparar con el oraculo.
      * Se realiza de forma paralela, dividiendo el trabajo en sublistas
      * de tamaño 4.
      * @param n Tamaño de la cadena a reconstruir
      * @param o Oraculo
      * @return Cadena reconstruida
      */
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
                sublista.find(o(_))
                }
            )
            //CAMBIAR EL FIND PLEASE
            val resultados = tareasParalelas.map(_.join())
            resultados.find(_.isDefined).flatten.getOrElse(Seq.empty[Char])
        }
    }
                        //  {["a",'c','t'],["a",'c','t'],["a",'c','t']}
    /*
    def validarCadenasOraculo(o:Oraculo, subCadenas:ParSeq[Seq[Char]]): Seq[Char] = subCadenas match {            
        case Seq() => Seq()
        case x::xs => if (o(x)) x else validarCadenasOraculo(o, xs)            
    }*/
    val umbral = 10
    def reconstruirCadenaMejoradoPar(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        }
        else {
            def subcadenas(alfabeto: Seq[Char], longitud: Int): ParSeq[Seq[Char]] = {
                //if(longitud < umbral)
                    //reconstruirCadenaMejorado(n, o)
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

}