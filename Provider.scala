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

    def positionsOf(p: Token => Boolean): Seq[Int] =
      tokens.zipWithIndex collect { case (t, i) if p(t) => i }

    // Just like splitAt, but discards the token at `index`.
    def cutAt(index: Int): (Tokens, Tokens) = {
      val (left, right) = tokens.splitAt(index)
      (left, Tokens(right.tail: _*))
    }

  }

  def splitExprAndGuard(tokens: Tokens): (Option[Term], Option[Term]) = {
    try ((Some(tokens.withBoFEoF.parse[Term]), None))
    catch {
      case _: Exception =>
        val positions = tokens positionsOf (_.isInstanceOf[Token.`if`])
        (positions foldLeft (None: Option[Term], None: Option[Term])) {
          case ((expr, None), idx) =>
            val (potExpr, potGuard) = tokens cutAt idx
            try ((Some(potExpr.withBoFEoF.parse[Term]), Some(potGuard.withBoFEoF.parse[Term])))
            catch { case _: Exception => (expr, None) }

          case ((expr, guard), _) => (expr, guard)
        }

    }
  }

  def For(gens: Tokens, body: Tokens): Tree = macro {

    def makeName(token: Token) =
      token.code.parse[Pat] // Here's the better way :D

    def createEnumerators(generators: Seq[(Token.Ident, Option[Term], Term)]): Tree => Tree =
      generators match {
        case Seq() =>
          abort("No generators")

        case Seq((id, Some(guard), expr)) =>
          val name = makeName(id)
          //(body: Tree) => q"($expr).filter(($name => $guard)).map(($name => $body))"
          (body: Tree) => s"($expr).filter($name => $guard).map($name => $body)".parse[Term] // Didn't manage to use quasiquotes here

        case Seq((id, None, expr)) =>
          val name = makeName(id)
          (body: Tree) => s"($expr).map($name => $body)".parse[Term]

        case Seq((id, Some(guard), expr), rest @ _*) =>
          val name = makeName(id)
          (body: Tree) => {
            val inner = createEnumerators(rest)(body)
            s"($expr).filter($name => $guard).flatMap($name => $inner)".parse[Term]
          }

        case Seq((id, None, expr), rest @ _*) =>
          val name = makeName(id)
          (body: Tree) => {
            val inner = createEnumerators(rest)(body)
            s"($expr).flatMap($name => $inner)".parse[Term]
          }
      }

    def createForeach(generators: Seq[(Token.Ident, Option[Term], Term)]): Tree => Tree =
      generators match {
        case Seq() =>
          identity

        case Seq((id, Some(guard), expr), rest @ _*) =>
          val name = internal.ast.Pat.Var.Term(internal.ast.Term.Name(id.code))
          (body: Tree) => {
            val inner = createForeach(rest)(body)
            s"($expr).filter($name => $guard).foreach($name => $inner)".parse[Term]
          }

        case Seq((id, None, expr), rest @ _*) =>
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
    val generators: Seq[(Token.Ident, Option[Term], Term)] = {
      gens.clean splitWith (t => t.code == "\n" || t.code == ";") map {

        //case toks"${ident: Token.Ident}<-..$expr" =>
        case (ident: Token.Ident) +: (_: Token.`<-`) +: expr =>
          splitExprAndGuard(Tokens(expr: _*)) match {
            case (None, _) => abort(expr.head.position, "Could not parse '${exprs.map(_.code).mkString}'")
            case (Some(expr), guard) => (ident, guard, expr)
          }

        case other =>
          abort(other.head.position, s"Could not recognize syntax: $other")
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
