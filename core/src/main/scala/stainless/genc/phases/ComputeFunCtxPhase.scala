/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package genc
package phases

import extraction.throwing.trees._

import scala.collection.mutable.{ Map => MutableMap }

/*
 * Compute, for each function definition, its context; i.e. roughly the set of free variable of its body.
 *
 * NOTE we are overapproximating the set of free variable by capturing any value defined "upstream"
 *      in the AST.
 *
 * NOTE in C99 there's the concept of strict aliasing (cf. §6.5/7), but since we don't do any weird
 *      cast operation in our translation, the overapproximation mentioned above is not an issue.
 */
class ComputeFunCtxPhase(using override val context: inox.Context) extends LeonPipeline[Symbols, FunCtxDB](context) {
  import context.{given, _}

  val name = "Function context computer"

  private given givenDebugSection: DebugSectionGenC.type = DebugSectionGenC

  def run(syms: Symbols): FunCtxDB = {

    val db = MutableMap[LocalFunDef, Seq[VarInfo]]()

    def toVarInfo(vd: ValDef) = VarInfo(vd, vd.tpe, vd.flags.contains(IsVar))

    def processFunction(fd: LocalFunDef, env: Seq[VarInfo]): Unit = {
      reporter.debug(s"Registering ${fd.id.name} with ${env map { _.vd.id } mkString ", "}.")
      db += fd -> env

      // Recurse on body with an extended context
      val env2 = env ++ (fd.params map toVarInfo)
      rec(fd.fullBody, env2)
    }

    def rec(expr: Expr, env: Seq[VarInfo]): Unit = expr match {
      case Let(binder, value, rest) =>
        rec(value, env) // binder not yet available here!
        val env2 = env :+ VarInfo(binder, binder.tpe, isVar = false)
        rec(rest, env2)

      case LetVar(binder, value, rest) =>
        rec(value, env) // binder not yet available here!
        val env2 = env :+ VarInfo(binder, binder.tpe, isVar = true)
        rec(rest, env2)

      case LetRec(fds, rest) =>
        // Register the nested functions, and recurse
        fds foreach { fd => processFunction(fd, env) }
        rec(rest, env)

      // Because technically a function could be defined in a block which is itself an argument,
      // we recurse on arguments as well!
      // This also includes Terminal-like expression and therefore stop recursion when needed.
      case Operator(args, _) => args foreach { arg => rec(arg, env) }

      case _ => reporter.fatalError(s"NOT YET IMPLEMENTED: env computation for ${expr.getClass}")
    }

    // Process every top level function to register function contexts for their inner functions;
    for (fd <- syms.functions.values) rec(fd.fullBody, fd.params map toVarInfo)

    db.toMap // Make it immutable
  }

}
