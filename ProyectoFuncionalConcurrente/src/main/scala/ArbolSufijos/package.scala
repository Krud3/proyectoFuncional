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
            case Nodo(_, marcada, _) if s.isEmpty => marcada // Retorna el valor booleano del nodo si la secuencia está vacía
            case Hoja(_, marcada) if s.isEmpty => marcada
            case Nodo(_, _, hijos) =>
            hijos.filter(hijo => raiz(hijo) == s.head)
                .exists(hijo => pertenece(s.tail, hijo))
            case _ => false
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
            case Nodo(c, b, lt) => {
                if (cabezas(t).contains(s(0))) {
                    val hijoElegido = lt.filter(hijo => raiz(hijo) == s(0))(0)
                    val newHijo = hijoElegido match {
                        case Nodo(c, _, lt) => Nodo(c, true, lt)
                        case Hoja(c, _) => Hoja(c, true)
                    }
                    val newlt = newHijo +: lt.filterNot(_ == hijoElegido)
                    Nodo(c, b, newlt)
                } else {
                    val newlt = Hoja(s(0), true) +: lt
                    Nodo(c, b, newlt)
                }
            }
            case Hoja(c, b) => {
                val newlt = Hoja(s(0), true) +: Nil
                Nodo(c, b, newlt)
            }
        }
        else {
            t match {
                case Nodo(c, b, lt) => {
                    if (cabezas(t).contains(s(0))) {
                        val hijoElegido = lt.filter(hijo => raiz(hijo) == s(0))(0)
                        val newlt = adicionar(s.drop(1), hijoElegido) +: lt.filterNot(_ == hijoElegido)
                        Nodo(c, b, newlt)
                    }
                    else {
                        val newHijo = Nodo(s(0), false, List())
                        val newlt = adicionar(s.drop(1), newHijo) +: lt
                        Nodo(c, b, newlt)
                    }
                }
                case Hoja(c, b) => {
                    val newNodo = Nodo(s(0), false, List())
                    val newlt = adicionar(s.drop(1), newNodo) +: Nil
                    Nodo(c, b, newlt)
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
                case Nodo(c, b, lt) => Nodo(c, b, lt)
            }
        }
    } 
}
