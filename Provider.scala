package parsermacro

import scala.meta._
import scala.meta.dialects.Scala211

object Provider {

  implicit class RichTokens(val tokens: Tokens) extends AnyVal {
    def clean: Tokens =
      tokens filterNot (t => t.code == " " || t.code == "")

    def splitWith(p: Token => Boolean): Seq[Tokens] =
      tokens span (t => !p(t)) match {
        case (Seq(), Seq()) => Seq()
        case (Seq(), rst) => Tokens(rst: _*) splitWith p
        case (fst, Seq()) => Seq(fst)
        case (fst, rst) => Seq(fst) ++ (Tokens(rst.tail: _*) splitWith p)
      }

    def withBoFEoF: Tokens = {
      val Seq(bof, eof) = "".tokens
      Tokens((bof +: tokens :+ eof): _*)
    }
  }

  def For(gens: Tokens, body: Tokens): Tree = macro {

    def createEnumerators(generators: Seq[(Token.Ident, Term)]): Tree => Tree =
      generators match {
        case Seq() =>
          throw new Exception("No generators?")

        case Seq((id, expr)) =>
          val name = internal.ast.Pat.Var.Term(internal.ast.Term.Name(id.code)) // There has to be a better way
          (body: Tree) => s"($expr).map($name => $body)".parse[Term] // Didn't manage to use quasiquotes here

        case Seq((id, expr), rest @ _*) =>
          val name = internal.ast.Pat.Var.Term(internal.ast.Term.Name(id.code))
          (body: Tree) => {
            val inner = createEnumerators(rest)(body)
            s"($expr).flatMap($name => $inner)".parse[Term]
          }
      }

    def createForeach(generators: Seq[(Token.Ident, Term)]): Tree => Tree =
      generators match {
        case Seq() =>
          identity

        case Seq((id, expr), rest @ _*) =>
          val name = internal.ast.Pat.Var.Term(internal.ast.Term.Name(id.code))
          (body: Tree) => {
            val inner = createForeach(rest)(body)
            s"($expr).foreach($name => $inner)".parse[Term]
          }
      }

    // This is really strange. If we use the token quasiquote to do deconstruction, then the
    // classfile is broken!!!
    // java.lang.ClassCastException: scala.reflect.internal.Types$ClassNoArgsTypeRef cannot be cast to scala.reflect.internal.Symbols$Symbol
    //  at scala.reflect.internal.pickling.UnPickler$Scan.readSymbolRef(UnPickler.scala:654)
    //  at scala.reflect.internal.pickling.UnPickler$Scan.readSymbol(UnPickler.scala:291)
    //  at scala.reflect.internal.pickling.UnPickler$Scan.readSymbolRef(UnPickler.scala:649)
    //  ...
    //  error: error while loading Provider, class file './parsermacro/Provider.class' is broken
    //  (class java.lang.RuntimeException/error reading Scala signature of Provider.class: scala.reflect.internal.Types$ClassNoArgsTypeRef cannot be cast to scala.reflect.internal.Symbols$Symbol)
    val generators: Seq[(Token.Ident, Term)] = {
      gens.clean splitWith (t => t.code == "\n" || t.code == ";") map {
        case (ident: Token.Ident) +: (_: Token.`<-`) +: expr =>
        //case toks"${ident: Token.Ident}<-..$expr" =>
          (ident, Tokens(expr: _*).withBoFEoF.parse[Term])

        case other =>
          throw new Exception(s"Could not recognize syntax: $other")
      }
    }

    val res: Tree = body.clean match {
      case (_: Token.`yield`) +: rest =>
        createEnumerators(generators)(Tokens(rest: _*).withBoFEoF.parse[Stat])

      case _ =>
        createForeach(generators)(body.parse[Stat])
    }

    println("#" * 70)
    println(res)
    println("#" * 70)
    res

  }
}
