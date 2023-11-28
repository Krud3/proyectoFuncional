import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {

  // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
  // y devuelve la secuencia reconstruida
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    if (n < 1) {
      Seq.empty[Char]
    }
    else {
      def subcadenas(alfabeto: Seq[Char], longitud: Int): List[Seq[Char]] = {
        if (longitud == 0)
          Seq.empty[Char] :: Nil
        else {
          val subcadenas_anteriores = subcadenas(alfabeto, longitud - 1)
          val lista_subcadenas = for (
            un_caracter <- alfabeto; una_subcadena <- subcadenas_anteriores
          ) yield un_caracter +: una_subcadena
          lista_subcadenas.toList
        }
      }
      val cadenas = subcadenas(alfabeto, n)
      cadenas.find(o(_)).get
    }
  }

  // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
  // y devuelve la secuencia reconstruida
  // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 tambien son subsecuencias de s
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    if (n < 1) {
      Seq.empty[Char]
    }
    else {
      def subcadenas(alfabeto: Seq[Char], longitud: Int): List[Seq[Char]] = {
        if (longitud == 0)
          Seq.empty[Char] :: Nil
        else {
          val subcadenas_validas_anteriores = subcadenas(alfabeto, longitud - 1)
          val lista_combinaciones = for (
            un_caracter <- alfabeto;
            una_subcadena <- subcadenas_validas_anteriores
            if o(un_caracter +: una_subcadena)
          ) yield un_caracter +: una_subcadena
          lista_combinaciones.toList
        }
      }
      val cadenas = subcadenas(alfabeto, n)
      cadenas.find(o(_)).get
    }
  }

  // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
  // y devuelve la secuencia reconstruida
  // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 tambien son subsecuencias de s
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    if (n < 1) {
      Seq.empty[Char]
    }
    else {
      def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): List[Seq[Char]] = {
        if (longitud == 1)
          (for (un_caracter <- alfabeto) yield un_caracter +: Nil).toList
        else {
          val subcadenas_validas_anteriores = subcadenasTurbo(alfabeto, longitud / 2)
          val lista_combinaciones = for (
            sub_cadena_1 <- subcadenas_validas_anteriores;
            sub_cadena_2 <- subcadenas_validas_anteriores
            if o(sub_cadena_1 ++ sub_cadena_2)
          ) yield sub_cadena_1 ++ sub_cadena_2
          lista_combinaciones
        }
      }
      val cadenas = subcadenasTurbo(alfabeto, n)
      cadenas.find(o(_)).get
    }
  }

  // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
  // y devuelve la secuencia reconstruida
  // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 tambien son subsecuencias de s
  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    if (n < 1) {
      Seq.empty[Char]
    }
    else {
      def son(s: Seq[Char], subsecuencias: Seq[Seq[Char]]): Boolean = {
        val l = s.length / 2
        val bools = (for (
          i <- 0 to l
        ) yield subsecuencias.contains(s.slice(i, i + l))).toSet
        !bools.contains(false)
      }
      def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): List[Seq[Char]] = {
        if (longitud == 1)
          (for (un_caracter <- alfabeto) yield un_caracter +: Nil).toList
        else {
          val subcadenas_validas_anteriores = subcadenasTurbo(alfabeto, longitud / 2)
          val lista_combinaciones = for (
            sub_cadena_1 <- subcadenas_validas_anteriores;
            sub_cadena_2 <- subcadenas_validas_anteriores
            // if (son(sub_cadena_1 ++ sub_cadena_2, subcadenas_validas_anteriores) && o(sub_cadena_1 ++ sub_cadena_2)) ???
            if (son(sub_cadena_1 ++ sub_cadena_2, subcadenas_validas_anteriores))
            if o(sub_cadena_1 ++ sub_cadena_2)
          ) yield sub_cadena_1 ++ sub_cadena_2
          lista_combinaciones
        }
      }
      val cadenas = subcadenasTurbo(alfabeto, n)
      cadenas.find(o(_)).get
    }
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    if (n < 1) {
      Seq.empty[Char]
    }
    else {
      def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): List[Seq[Char]] = {
        if (longitud == 1)
          (for (un_caracter <- alfabeto) yield un_caracter +: Nil).toList
        else {
          val subcadenas_validas_anteriores = subcadenasTurbo(alfabeto, longitud / 2)
          val lista_combinaciones = for (sub_cadena_1 <- subcadenas_validas_anteriores; sub_cadena_2 <- subcadenas_validas_anteriores if o(sub_cadena_1 ++ sub_cadena_2)) yield sub_cadena_1 ++ sub_cadena_2
          lista_combinaciones
        }
      }

      val cadenas = subcadenasTurbo(alfabeto, n)
      val t = arbolDeSufijos(cadenas)
      val primera_cadena = cadenas.find(o(_)).get

      primera_cadena
    }
  }
}