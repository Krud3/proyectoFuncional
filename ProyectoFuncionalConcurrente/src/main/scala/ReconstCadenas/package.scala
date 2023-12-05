import ArbolSufijos._
import Oraculo._
package object ReconstCadenas {

    /**
      * Metodo encargado de reconstruir una cadena a partir de un oraculo.
      * Utilizando fuerza bruta, llegando a si a todas las combinaciones
      * del alfabeto para el tamaño dado y luego comparar con el oraculo
      * @param n Tamaño de la cadena a reconstruir
      * @param o Oraculo
      * @return Cadena reconstruida
      */
    def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        }
        else {
            def subcadenas(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
                if (longitud == 0)
                Seq.empty[Char] :: Nil
                else {
                val subcadenas_anteriores = subcadenas(alfabeto, longitud - 1)
                val lista_subcadenas = for (
                    un_caracter <- alfabeto; una_subcadena <- subcadenas_anteriores
                ) yield un_caracter +: una_subcadena
                lista_subcadenas
            }
        }
        val cadenas = subcadenas(alfabeto, n)
        cadenas.find(o(_)).get
        }
    }

    /**
      * Metodo encargado de reconstruir una cadena a partir de un oraculo.
      * Utiliza la propiedad de que si s = s1 * s2 entonces s1 y s2
      * tambien son subsecuencias de s.
      * Entonces se va construyendo la cadena de a un caracter a la vez
      * y se va verificando si es subsecuencia del oraculo.
      * @param n Tamaño de la cadena a reconstruir
      * @param o Oraculo
      * @return Cadena reconstruida
      */
    def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        }
        else {
            def subcadenas(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
                if (longitud == 0)
                    Seq.empty[Char] :: Nil
                else {
                    val subcadenas_validas_anteriores = subcadenas(alfabeto, longitud - 1)
                    val lista_combinaciones = for (
                            un_caracter <- alfabeto;
                            una_subcadena <- subcadenas_validas_anteriores;
                            values = un_caracter +: una_subcadena
                            if o(values)
                        ) yield values
                    lista_combinaciones
                }
            }
        subcadenas(alfabeto, n)(0)
        }
    }

    /**
      * Metodo encargado de reconstruir una cadena a partir de un oraculo.
      * Utiliza la propiedad de que si s = s1 ++ s2 entonces s1 y s2
      * tambien son subsecuencias de s.
      * Entonces se va construyendo la cadena a partir de la concatenacion de
      * las dos cadenas anteriores y se va verificando si es subsecuencia del oraculo.
      * @param n Tamaño de la cadena a reconstruir
      * @param o Oraculo
      * @return Cadena reconstruida
      */
    def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        }
        else {
        def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
            if (longitud == 1)
                for(un_caracter <- alfabeto; if(o(Seq(un_caracter)))) yield Seq(un_caracter)
            else {
                val subcadenas_validas_anteriores = subcadenasTurbo(alfabeto, longitud / 2)
                val lista_combinaciones = for (
                        sub_cadena_1 <- subcadenas_validas_anteriores;
                        sub_cadena_2 <- subcadenas_validas_anteriores;
                        values = sub_cadena_1 ++ sub_cadena_2
                        if o(values)
                    ) yield values
                lista_combinaciones
            }
            }
        val cadenas = subcadenasTurbo(alfabeto, n)
        cadenas(0)
        }
    }

    /**
      * Metodo encargado de reconstruir una cadena a partir de un oraculo.
      * Utiliza la propiedad de que si s = s1 ++ s2 entonces s1 y s2
      * tambien son subsecuencias de s.
      * Pero se aplica un filtro para no generar todas las combinaciones
      * entre las cadenas anteriores, haciendo el algoritmo mas efectivo.
      * @param n Tamaño de la cadena a reconstruir
      * @param o Oraculo
      * @return Cadena reconstruida
      */
    def reconstruirCadenaTurboMejorado(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
        Seq.empty[Char]
        }
        else {
            // Funcion que verifica si una secuencia es subsecuencia de otra
            def son(s: Seq[Char], subsecuencias: Seq[Seq[Char]], l: Int): Boolean = {
                if (s.length == l + 1) {
                    true
                } else {
                    val test = s.slice(1, 1 + l)
                    if (subsecuencias.contains(test)) {
                        son(s.drop(1), subsecuencias, l)
                    } else {
                        false
                    }
                }
            }
            //Funcion encargada de generar las cadenas validas anteriores para concatenarlas
            // y verificar si son subsecuencias de la cadena anterior y del oraculo
            def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
                    if (longitud == 1)
                        for(un_caracter <- alfabeto; if(o(Seq(un_caracter)))) yield Seq(un_caracter)
                    else {
                    val subcadenas_validas_anteriores = subcadenasTurbo(alfabeto, longitud / 2)
                    val lista_combinaciones = for (
                        sub_cadena_1 <- subcadenas_validas_anteriores;
                        sub_cadena_2 <- subcadenas_validas_anteriores;
                        values = sub_cadena_1 ++ sub_cadena_2
                        if (son(values, subcadenas_validas_anteriores, longitud / 2))
                        if o(values)
                    ) yield values
                    lista_combinaciones
                    }
                }
        val cadenas = subcadenasTurbo(alfabeto, n)
        cadenas(0)
        }
    }
    /**
      * Metodo encargado de reconstruir una cadena a partir de un oraculo.
      * Utiliza la propiedad de que si s = s1 ++ s2 entonces s1 y s2
      * tambien son subsecuencias de s.
      * Pero se aplica un filtro para no generar todas las combinaciones
      * entre las cadenas anteriores, haciendo el algoritmo mas efectivo.
      * Pero ahora en vez de compararlo con la lista de subsecuencias anteriores
      * se compara con el arbol de sufijos de las subsecuencias anteriores.
      * @param n Tamaño de la cadena a reconstruir
      * @param o Oraculo
      * @return Cadena reconstruida
      */
    def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
        if (n < 1) {
            Seq.empty[Char]
        }
        else {
            // Funcion que verifica si una secuencia es subsecuencia de otra
            // pero ahora se compara con el arbol de sufijos de las subsecuencias anteriores
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
            //Funcion encargada de generar las cadenas validas a partir de una secuencia inicial y un arbol inicial
            def subCaddenasAlternativa(secuenciaInicial: Seq[Seq[Char]],arbolInicial : Trie, aumentador:Int): Seq[Char]= {
                if(n == aumentador){
                    secuenciaInicial(0) 
                }
                else{
                    val arbolNuevo: Trie = secuenciaInicial.foldLeft(arbolInicial) { (arbolActual, secuencia) =>
                            adicionar(secuencia, arbolActual)}
                    val lista_combinaciones = for(
                        sub_cadena_1 <- secuenciaInicial;
                        sub_cadena_2 <- secuenciaInicial;
                        valor = sub_cadena_1 ++ sub_cadena_2
                        if(son(valor, arbolNuevo, aumentador))
                        if(o(valor))
                    )yield valor
                    subCaddenasAlternativa(lista_combinaciones, arbolNuevo, aumentador*2)
                }
            }
            val secuenciaInicial= for(un_caracter <- alfabeto; if(o(Seq(un_caracter)))) yield Seq(un_caracter)
            val arbolInicial = arbolDeSufijos(secuenciaInicial)
            val cadenas = subCaddenasAlternativa(secuenciaInicial,arbolInicial, 1)
            cadenas
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
}
