import common._ 
import Oraculo._
import ReconstCadenas._
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

            val resultados = tareasParalelas.map(_.join())
            resultados.find(_.isDefined).flatten.getOrElse(Seq.empty[Char])
        }
    }

}