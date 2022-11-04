package graph

import scala.io.Source

/*
type Weight = Int
type WeightedEdge = (Vertex, Weight)
type WeightedGraph = Seq[Seq[WeightedEdge]]
*/

object G1 {
  type Vertex = Int
  type Edge = Vertex
  type Graph = Array[Seq[Edge]]

  def load(path: String): Graph = {
    val lines = Source.fromFile(path).getLines()
    val n = lines.next().toInt
    val g: Graph = Array.fill(n)(Seq())
    for (i <- Range(0, n)) {
      g(i) = lines.next() match {
        case "" => Seq()
        case line => line.split(",").map(n => n.toInt).toSeq
      }
    }
    g
  }

  def output(g: Graph): Unit = {
    for (i <- Range(0, g.length)) {
      println(s"$i: ${g(i)}")
    }
  }
}

object Main1 extends App {
  import G1._

  output(load("data/graph3.csv"))
}

object G2 {
  type Vertex = G1.Vertex
  type Edge = G1.Edge
  type Graph = G1.Graph

  def load(path: String): G1.Graph = {
    val g = G1.load(path)
    for (i <- Range(0, g.length)) {
      g(i) = g(i).sorted  // 頂点番号4に注目
    }
    g
  }

  def output(g: G1.Graph): Unit = G1.output(g)

  def output_matrix(g: G1.Graph): Unit = {
    val n = g.length
    for (i <- Range(0, n)) {
      print(s"$i ")
      for (j <- Range(0, n)) {
        print(if (is_connected(g, i, j)) 'x' else ' ')
      }
      println()
    }
  }

  def is_connected(g: G1.Graph, i: Int, j: Int): Boolean = {
    val neighbors_i = g(i)
    try {
      var k1 = 0; var k2 = neighbors_i.length - 1
      while (k1 <= k2) {
        val m = k1 + (k2 - k1) / 2
        val v = neighbors_i(m)
        if (v < j) k1 = m + 1
        else if (j < v) k2 = m - 1
        else throw new Exception()
      }
      false
    } catch {
      case e: Exception => true
    }
  }
}

object Main2 extends App {
  import G2._

  val g = load("data/graph3.csv")
  output(g)
  output_matrix(g)
}

object G3 {
  type Vertex  = G2.Vertex
  type Edge  = G2.Edge
  type Graph = G2.Graph

  def load(path: String): G2.Graph = G2.load(path)
  def output(g: G2.Graph): Unit = G2.output(g)
  def is_connected(g: G2.Graph, i: Int, j: Int): Boolean = G2.is_connected(g, i, j)
  def output_matrix(g: G2.Graph): Unit = G2.output_matrix(g)

  type Vertices = Seq[Vertex]

  def is_walk(g: Graph, s: Vertices): Boolean = {
    s.length <= 1 ||
    Range(0, s.length - 1).forall(i => is_connected(g, s(i), s(i + 1)))
  }

  def is_path(g: Graph, s: Vertices): Boolean = {
    is_walk(g, s) && Set(s).size == s.length
  }

  def is_cycle(g: Graph, s: Vertices): Boolean = {
    s.length <= 1 ||
    is_walk(g, s) && s(0) == s(s.length-1)
  }
}

object Main3 extends App {
  import G3._

  val g = load("data/graph3.csv")
  output(g)
  output_matrix(g)

  for (s <- List(Seq(), Seq(0),
       Seq(0, 1, 2, 3, 4, 5, 6, 7),
       Seq(0, 5, 2, 7, 3, 0),

       Seq(0, 5, 2, 7),
       Seq(0, 5, 2, 7, 3, 0, 7),
       Seq(0, 5, 2, 7, 3, 0, 7, 6),

       Seq(0, 5, 2, 7, 3, 0, 2))) {
         println(s"$s is a walk? ${is_walk(g, s)}")
       }

  for (s <- List(Seq(), Seq(0),
       Seq(0, 1, 2, 3, 4, 5, 6, 7),
       Seq(0, 5, 2, 7, 3, 0),

       Seq(0, 5, 2, 7),
       Seq(0, 5, 2, 7, 3, 0, 7),
       Seq(0, 5, 2, 7, 3, 0, 7, 6),

       Seq(0, 5, 2, 7, 3, 0, 2))) {
         println(s"$s is a path? ${is_path(g, s)}")
       }

  for (s <- List(Seq(), Seq(0),
       Seq(0, 1, 2, 3, 4, 5, 6, 7),
       Seq(0, 5, 2, 7, 3, 0),

       Seq(0, 5, 2, 7),
       Seq(0, 5, 2, 7, 3, 0, 7),
       Seq(0, 5, 2, 7, 3, 0, 7, 6),

       Seq(0, 5, 2, 7, 3, 0, 2, 0),
       Seq(0, 5, 2, 7, 3, 0, 7, 0))) {
         println(s"$s is a cycle? ${is_cycle(g, s)}")
       }
}
