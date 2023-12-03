package object ArbolSufijos {
    abstract class Trie
    case class Nodo(car:Char, marcada:Boolean,
                    hijos: List[Trie]) extends Trie
    case class Hoja(car:Char, marcada:Boolean) extends Trie
    def raiz(t:Trie): Char = {
        t match {
        case Nodo(c,_,_) => c
        case Hoja(c,_) => c
        }
    }
    def cabezas(t:Trie): Seq[Char] = {
        t match {
        case Nodo(_,_,lt) => lt.map(t => raiz(t))
        case Hoja(c,_) => Seq[Char](c)
        }
    }

    /**
      * Determina si una secuencia de caracteres pertenece a un trie
      * @param s Secuencia de caracteres
      * @param t Trie
      * @return true si la secuencia pertenece al trie, false en caso contrario
      */
    def pertenece(s: Seq[Char], t: Trie): Boolean = {
        t match {
            case Nodo(_, b, hijos) if s.isEmpty => b // Retorna el valor booleano del nodo si la secuencia está vacía
            case Hoja(_, marcada) if s.isEmpty => marcada
            case Nodo(_, _, hijos) =>
            hijos.find(hijo => raiz(hijo) == s.headOption.getOrElse(return false))
                .exists(hijo => pertenece(s.tail, hijo))
            case Hoja(_, marcada) => (s.length < 1) && marcada
        }
    }

    /**
      * Se encarga de adicionar una secuencia de caracteres a un trie
      * @param s Secuencia de caracteres
      * @param t Trie al que se le va a adicionar la secuencia
      * @return Trie con la secuencia de caracteres adicionada
      */
    def adicionar(s: Seq[Char], t: Trie): Trie = {
        if (pertenece(s, t) || s.isEmpty) {
            t
        }
        else if (s.length == 1) t match {
            case Nodo(a, b, lt) => {
                if (cabezas(t).contains(s(0))) {
                    val hijoElegido = lt.find(hijo => raiz(hijo) == s(0)).get
                    val newHijo = hijoElegido match {
                        case Nodo(a, _, lt) => Nodo(a, true, lt)
                        case Hoja(a, _) => Hoja(a, true)
                    }
                    val newlt = newHijo +: lt.filterNot(_ == hijoElegido)
                    Nodo(a, b, newlt)
                } else {
                    val newlt = Hoja(s(0), true) +: lt
                    Nodo(a, b, newlt)
                }
            }
            case Hoja(a, b) => {
                val newlt = Hoja(s(0), true) +: Nil
                Nodo(a, b, newlt)
            }
        }
        else {
            t match {
                case Nodo(a, b, lt) => {
                    if (cabezas(t).contains(s(0))) {
                        val hijoElegido = lt.find(hijo => raiz(hijo) == s(0)).get
                        val newlt = adicionar(s.drop(1), hijoElegido) +: lt.filterNot(_ == hijoElegido)
                        Nodo(a, b, newlt)
                    }
                    else {
                        val newHijo = Nodo(s(0), false, List())
                        val newlt = adicionar(s.drop(1), newHijo) +: lt
                        Nodo(a, b, newlt)
                    }
                }
                case Hoja(a, b) => {
                    val newNodo = Nodo(s(0), false, List())
                    val newlt = adicionar(s.drop(1), newNodo) +: Nil
                    Nodo(a, b, newlt)
                }
            }
        }
    }

    /**
      * Metodo encargado de crear un arbol de sufijos a partir de una secuencia de secuencias de caracteres
      * @param ss Secuencia de secuencias de caracteres
      * @return Arbol de sufijos
      */
    def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
        def adicionarSufijos(sec: Seq[Char], t: Trie): Trie = {
            if (sec.length == 0) {
                t
            } else {
                val t2 = adicionarSufijos(sec.drop(1), t)
                adicionar(sec, t2)
            }
            }

            ss.foldLeft(Nodo('_', false, List())) {
            (trie, s) => val trieNuevo = adicionarSufijos(s, trie)
            trieNuevo match {
                case Nodo(a, b, c) => Nodo(a, b, c)
            }
        }
    } 

    /**
      * Metodo encargado de imprimir el arbol de sufijos dado para una visualizacion de este
      * @param t Arbol de sufijos
      * @param prefijo Prefijo para la visualizacion del arbol
      */
    def imprimirTrie(t: Trie, prefijo: String = ""): Unit = {
        t match {
            case Nodo(car, marcada, hijos) =>
                // Imprime el carácter de este nodo
                println(prefijo + (if (car == '_') "" else car) + (if (marcada) "*" else ""))
                // Imprime todos los hijos recursivamente, aumentando el prefijo
                hijos.foreach(hijo => imprimirTrie(hijo, prefijo + car))
            case Hoja(car, marcada) =>
                // Imprime el carácter de la hoja y marca si es el final de una palabra
                println(prefijo + car + (if (marcada) "*" else ""))
        }
    }
}
