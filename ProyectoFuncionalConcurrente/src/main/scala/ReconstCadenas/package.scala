import Oraculo._

package object ReconstCadenas {


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////------------------------------------------------------FUNCIONES AUXILIARES-----------------------------------------------///////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////    
    /**
      * Genera todas las posibles subcadenas de longitud n, segun el alfabeto
      * @param n longitud de las subcadenas
      */
    def posiblesSubCadenas(n: Int): Seq[Seq[Char]] = {
        if (n == 0) Seq(Seq())
        else
            for {
                subCadena <- posiblesSubCadenas(n - 1)
                caracter <- alfabeto
            } yield caracter +: subCadena
    }
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
    /**
      * Metodo encargado de generar las cadenas candidatas a ser validadas por el oraculo
      * a partir de las cadenas anteriores
      *
      * @param cadenasAnteriores lista de cadenas anteriores
      * @return lista de cadenas candidatas, el tamanio de la lista es 1 + el tamanio de la lista de cadenas anteriores
      */
    def generarCadenasCandidatas(cadenasAnteriores: Seq[Seq[Char]]): Seq[Seq[Char]] = {
        cadenasAnteriores.flatMap(cadena => alfabeto.map(caracter => cadena :+ caracter))
    }
    def filtrarCadenas(cadenas: Seq[Seq[Char]], oraculo: Oraculo): Seq[Seq[Char]] = {
        cadenas.filter(oraculo)
    }
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////-------------------------------------------------------------DESARROLLO--------------------------------------------------///////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
    /*
    * Solucion 2.3.1 ------ Solucion Ingenua
    */
    def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
        val posiblesCadenas = posiblesSubCadenas(n)
        posiblesCadenas.find(o).getOrElse(Seq())
    }

    def reconstruirCadenaIngenuo2(n: Int, o: Oraculo): Seq[Char] = {
        (for {
            subCadena <- posiblesSubCadenas(n)
            if o(subCadena) 
        } yield subCadena)(0)
    }

    def reconstruirCadenaIngenuo3(n: Int, o: Oraculo): Seq[Char] = {
        val posiblesCadenas = posiblesSubCadenas(n)
        posiblesCadenas.filter(o).head
    }
    //favorito
    /**
      * Solucion 2.3.1 ------ Solucion Ingenua
      * Teniendo en cuenta todas las posibles subcadenas de longitud n, 
      * se va a ir descartando las que no cumplan con la condicion del oraculo
      * hasta que se encuentre la que cumpla
      * @param n longitud de las subcadenas
      * @param o oraculo que se va a utilizar para validar las subcadenas
      * @return la subcadena que cumpla con la condicion del oraculo
      */
    def reconstruirCadenaIngenuo4(n: Int, o: Oraculo): Seq[Char] = {
        val posiblesCadenas = posiblesSubCadenas(n)
        validarCadenasOraculo(o, posiblesCadenas)
    }

    /*
    * Solucion 2.3.2 ------ Solucion mejorada
    */

    /**
      * Solucion 2.3.2 ------ Solucion mejorada
      * Teniendo en cuenta todas las posibles subcadenas de longitud n,
      * se va a ir descartando las que no cumplan con la condicion del oraculo
      * hasta que se encuentre la que cumpla o se acaben las subcadenas
      *
      * @param n longitud de las subcadenas
      * @param oraculo que se va a utilizar para validar las subcadenas
      * @return la subcadena que cumpla con la condicion del oraculo
      */
    def reconstruirCadenaMejorado(n: Int, oraculo: Oraculo): Seq[Char] = {
        def iterar(k: Int, cadenasActuales: Seq[Seq[Char]]): Seq[Char] = {
            if (k > n) validarCadenasOraculo(oraculo, cadenasActuales)
            else {
                val nuevasCadenas = generarCadenasCandidatas(cadenasActuales)
                val cadenasFiltradas = filtrarCadenas(nuevasCadenas, oraculo)
                iterar(k + 1, cadenasFiltradas)
                }
        }
        iterar(1, Seq(Seq.empty[Char]))
    }

    def reconstruirCadenaTurbo(n: Int, oraculo: Oraculo): Seq[Char] = {
        //recibe la longitud de la secuencia que hay que recontruir (n, potencia de 2) y un oraculo para esa secuencia
        //y devuelve la secuencia reconstruida
        //Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
        def iterar(k: Int, cadenasActuales: Seq[Seq[Char]]): Seq[Char] = {
            if (k > n) validarCadenasOraculo(oraculo, cadenasActuales)
            else {
                val nuevasCadenas = generarCadenasCandidatas(cadenasActuales)
                val cadenasFiltradas = filtrarCadenas(nuevasCadenas, oraculo)
                iterar(k + 1, cadenasFiltradas)
                }
        }
        iterar(1, Seq(Seq.empty[Char]))
    }

}
