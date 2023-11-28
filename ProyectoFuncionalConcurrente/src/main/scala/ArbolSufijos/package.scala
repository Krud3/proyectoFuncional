import ArbolSufijos.Trie

import scala.util.matching.Regex.Match

package object ArbolSufijos {
  // Definiendo otra estructura para manipular Seq[Seq[Char]]
  abstract class Trie
  case class Nodo (car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
  case class Hoja (car: Char, marcada: Boolean) extends Trie
  def raiz(t: Trie): Char = {
    t match {
    case Nodo (c, _, _) => c
    case Hoja (c, _) => c
    }
  }
  def cabezas(t: Trie): Seq[Char] = {
    t match {
    case Nodo (_, _, lt) => lt.map (t => raiz (t) )
    case Hoja (c, _) => Seq[Char] (c)
    }
  }

  // Devuelve true si la secuencia s es reconocida por el trie t, y false si no.
  def perteneceErr(s: Seq[Char], t: Trie): Boolean = {
    t match {
      case Nodo(_, b, lt) => {
        if (cabezas(t).contains(s(0))) {
          val hijoElegido = lt.find(nodo => raiz(nodo) == s(0)).get
          perteneceErr(s.drop(1), hijoElegido)
        }
        else {
          false
        }
      }
      case Hoja(c, b) => Seq[Char](c) == s(0) && (s.length < 2) && b
    }
  }

  // Devuelve true si la secuencia s es reconocida por el trie t, y false si no
  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    if (s.length < 1) t match {
      case Nodo(_, b, _) => b
      case Hoja(_, b) => b
    }
    else t match {
      case Nodo(_, _, lt) => {
        if (cabezas(t).contains(s(0))) {
          val hijoElegido = lt.find(hijo => raiz(hijo) == s(0)).get
          pertenece(s.drop(1), hijoElegido)
        }
        else {
          false
        }
      }
      case Hoja(_, b) => (s.length < 1) && b
    }
  }

// Adiciona una secuencia de uno o mas caracteres a un trie.
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

  //recibe una secuencia de secuencias ss y devuelve el trie correspondiente al arbol de sufijos de ss.
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    ss.foldLeft(Nodo('_', false, List())) { (trie, s) =>
      // la función anónima ahora devuelve un objeto de tipo Trie
      val trieNuevo = adicionar(s, trie)
      trieNuevo match {
        case Nodo(a, b, c) => Nodo(a, b, c)
      }
    }
  }
}
