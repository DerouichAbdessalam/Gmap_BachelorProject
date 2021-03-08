/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package extraction
package imperative

trait ReturnElimination
  extends oo.CachingPhase
    with IdentitySorts
    with SimplyCachedFunctions
    with SimpleFunctions
    with oo.IdentityTypeDefs
    with oo.IdentityClasses
    with utils.SyntheticSorts { self =>

  val s: Trees
  val t: Trees

  implicit val sPrinterOpts: s.PrinterOptions = s.PrinterOptions.fromContext(context)
  implicit val tPrinterOpts: t.PrinterOptions = t.PrinterOptions.fromContext(context)

  protected class TransformerContext(val symbols: s.Symbols) {
    private val deconstructor = s.getDeconstructor(t)

    // the set of expressions containing a return expression
    val exprHasReturn = collection.mutable.Map[Identifier, collection.mutable.Set[s.Expr]]()

    def addExpression(id: Identifier, e: s.Expr): Unit = {
      if (exprHasReturn.contains(id))
        exprHasReturn(id) += e
      else
        exprHasReturn(id) = collection.mutable.Set(e)
    }

    object ReturnFinder extends transformers.Traverser {
      override val trees: self.s.type = self.s

      // holds the identifier of the current LocalFunDef
      // initially equal to the top-level `fid`
      type Env = Identifier

      override def traverse(expr: s.Expr, currentId: Env): Unit = expr match {
        case s.LetRec(lfds, rest) =>
          lfds.foreach(lfd => traverse(lfd.fullBody, lfd.id))
          traverse(rest, currentId)
          if (exprHasReturn.contains(currentId) && exprHasReturn(currentId)(rest))
            addExpression(currentId, expr)

        case s.Return(e) =>
          addExpression(currentId, expr)

        case s.Operator(es, _) =>
          es.map(traverse(_, currentId))

          if (exprHasReturn.contains(currentId) && es.exists(exprHasReturn(currentId)))
            addExpression(currentId, expr)
      }
    }

    for (fd <- symbols.functions.values) ReturnFinder.traverse(fd.fullBody, fd.id)
  }

  /* Extract functional result value. Useful to remove side effect from conditions when moving it to post-condition */
  def getFunctionalResult(expr: t.Expr): t.Expr = t.exprOps.postMap {
    case t.Block(_, res) => Some(res)
    case _ => None
  }(expr)

  // when cf: ControlFlow[A, A]
  // optimisation for `cf match { case Return(retValue) => retValue case Proceed(value) => value }`
  def unwrap(expr: t.Expr): t.Expr = expr match {
    case ControlFlowSort.Return(e) => e
    case ControlFlowSort.Proceed(e) => e
    case t.Let(vd, e, body) => t.Let(vd, e, unwrap(body)).setPos(expr)
    case t.LetVar(vd, e, body) => t.LetVar(vd, e, unwrap(body)).setPos(expr)
    case t.LetRec(fds, rest) => t.LetRec(fds, unwrap(rest)).setPos(expr)
    case t.Assert(cond, err, body) => t.Assert(cond, err, unwrap(body)).setPos(expr)
    case t.Assume(cond, body) => t.Assume(cond, unwrap(body)).setPos(expr)
    case t.IfExpr(cond, e1, e2) => t.IfExpr(cond, unwrap(e1), unwrap(e2)).setPos(expr)
    case t.MatchExpr(scrut, cases) => t.MatchExpr(scrut, cases.map {
      case mc @ t.MatchCase(pat, optGuard, rhs) =>
      t.MatchCase(pat, optGuard, unwrap(rhs)).copiedFrom(mc)
    }).setPos(expr)
    case t.Block(es, last) => t.Block(es, unwrap(last)).setPos(expr)
    case _ =>
      context.reporter.internalError(expr.getPos,
        s"In ReturnElimination phase, ControlFlow unwrapping not supported for ${expr.asString}"
      )
  }

  override protected def getContext(symbols: s.Symbols) = new TransformerContext(symbols)

  protected def extractFunction(tc: TransformerContext, fd: s.FunDef): t.FunDef = {
    implicit val symboms = tc.symbols

    object SimpleWhileTransformer extends transformers.TreeTransformer {
      override val s: self.s.type = self.s
      override val t: self.t.type = self.t

      override def transform(e: s.Expr): t.Expr = e match {
        // we allow `return` inside local function definitions
        case s.LetRec(lfds, rest) =>
          t.LetRec(
            lfds.map(lfd => new ReturnTransformer(s.Inner(lfd)).getResult.asInstanceOf[t.Inner].fd),
            transform(rest)
          ).setPos(e)

        case s.Return(_) =>
          context.reporter.fatalError(e.getPos, "Keyword `return` is not allowed here")

        case wh @ s.While(cond, body, optInv) =>
          val transformedCond = transform(cond)
          val transformedBody = transform(body)
          val transformedInv = optInv.map(transform)

          val id = FreshIdentifier(fd.id.name + "While")
          val tpe = t.FunctionType(Seq(), t.UnitType().copiedFrom(wh)).copiedFrom(wh)

          val specced = t.exprOps.BodyWithSpecs(transformedBody)
          val measure = specced.getSpec(t.exprOps.MeasureKind).map(_.expr)

          val ite =
            t.IfExpr(
              transformedCond,
              t.ApplyLetRec(id, Seq(), tpe, Seq(), Seq()).copiedFrom(wh),
              t.UnitLiteral().copiedFrom(wh)
            ).copiedFrom(wh)

          val newBody =
            t.Block(
              Seq(specced.body),
              ite
            ).copiedFrom(wh)

          val newPost =
            t.Lambda(
              Seq(t.ValDef.fresh("_unused", t.UnitType().copiedFrom(wh)).copiedFrom(wh)),
              t.and(
                t.Not(getFunctionalResult(transformedCond).copiedFrom(cond)).copiedFrom(cond),
                transformedInv.getOrElse(t.BooleanLiteral(true).copiedFrom(wh))
              ).copiedFrom(wh)
            ).copiedFrom(wh)

          val fullBody = t.exprOps.withPostcondition(
            t.exprOps.withPrecondition(
              t.exprOps.withMeasure(newBody, measure).copiedFrom(wh),
              Some(t.andJoin(transformedInv.toSeq :+ getFunctionalResult(transformedCond)))
            ).copiedFrom(wh),
            Some(newPost)
          ).copiedFrom(wh)

          t.LetRec(
            Seq(t.LocalFunDef(id, Seq(), Seq(), t.UnitType().copiedFrom(wh), fullBody, Seq()).copiedFrom(wh)),
            t.IfExpr(
              transformedCond,
              t.ApplyLetRec(id, Seq(), tpe, Seq(), Seq()).copiedFrom(wh),
              t.UnitLiteral().copiedFrom(wh)
            ).copiedFrom(wh)
          ).copiedFrom(wh)

        case s.Block(es, last) =>
          super.transform(e)

        case _ => super.transform(e)
      }
    }

    class ReturnTransformer(fa: s.FunAbstraction) extends TransformerWithType {
      override val s: self.s.type = self.s
      override val t: self.t.type = self.t
      override val symbols: s.Symbols = tc.symbols

      def exprHasReturn(e: s.Expr): Boolean =
        tc.exprHasReturn.contains(fa.id) &&
        tc.exprHasReturn(fa.id)(e)

      val specced = s.exprOps.BodyWithSpecs(fa.fullBody)
      val retType = fa.returnType
      val retTypeChecked = SimpleWhileTransformer.transform(retType)
      val topLevelPost = specced.getSpec(s.exprOps.PostconditionKind)

      def getResult: t.FunAbstraction = {
        val newBody =
          specced.bodyOpt.map { body =>
            if (tc.exprHasReturn.contains(fa.id))
              unwrap(transform(body, retType)).setPos(body)
            else
              transform(body, retType)
          }

        val newBodyWithSpecs = t.exprOps.BodyWithSpecs(
          specced.lets.map {
            case (vd0, e0, pos0) =>
              (SimpleWhileTransformer.transform(vd0), SimpleWhileTransformer.transform(e0), pos0)
          },
          specced.specs.map(spec => spec.map(t)(SimpleWhileTransformer.transform)),
          t.UnitLiteral() // replaced with the `withBody` call below
        ).withBody(newBody, retTypeChecked).reconstructed

        fa.to(t)(
          fa.id,
          fa.tparams.map(SimpleWhileTransformer.transform),
          fa.params.map(SimpleWhileTransformer.transform),
          SimpleWhileTransformer.transform(fa.returnType),
          newBodyWithSpecs,
          fa.flags.map(SimpleWhileTransformer.transform)
        )
      }

      private def proceedOrTransform(expr: s.Expr, currentType: s.Type): t.Expr = {
        val currentTypeChecked = SimpleWhileTransformer.transform(currentType)
        if (exprHasReturn(expr)) transform(expr, currentType)
        else ControlFlowSort.proceed(retTypeChecked, currentTypeChecked, SimpleWhileTransformer.transform(expr))
      }

      private def proceedOrTransform(mc: s.MatchCase, currentType: s.Type): t.MatchCase = {
        val s.MatchCase(pattern, optGuard, rhs) = mc
        t.MatchCase(
          SimpleWhileTransformer.transform(pattern),
          optGuard.map(SimpleWhileTransformer.transform),
          proceedOrTransform(rhs, currentType)
        ).setPos(mc)
      }

      override def transform(expr: s.Expr, currentType: s.Type): t.Expr = expr match {
        case wh @ s.While(cond, body, optInv) if exprHasReturn(body) =>

          val id = FreshIdentifier(fd.id.name + "While")
          val loopType = ControlFlowSort.controlFlow(SimpleWhileTransformer.transform(retType), t.UnitType())
          val tpe = t.FunctionType(Seq(), loopType.copiedFrom(wh)).copiedFrom(wh)

          val specced = s.exprOps.BodyWithSpecs(body)
          val measure = specced.getSpec(s.exprOps.MeasureKind).map(spec =>
            SimpleWhileTransformer.transform(spec.expr)
          )

          val ite =
            t.IfExpr(
              SimpleWhileTransformer.transform(cond),
              t.ApplyLetRec(id, Seq(), tpe, Seq(), Seq()).copiedFrom(wh),
              ControlFlowSort.proceed(retTypeChecked, t.UnitType(), t.UnitLiteral()).copiedFrom(wh)
            ).copiedFrom(wh)

          val newBody =
            ControlFlowSort.andThen(retTypeChecked, t.UnitType(), t.UnitType(),
              transform(specced.body, s.UnitType()),
              _ => ite,
              wh.getPos
            )

          val optInvChecked = optInv.map(SimpleWhileTransformer.transform)
          val condChecked = SimpleWhileTransformer.transform(cond)

          val cfWhileVal = t.ValDef.fresh("cfWhile", loopType.copiedFrom(wh)).copiedFrom(wh)
          val newPost =
            t.Lambda(
              Seq(cfWhileVal),
              ControlFlowSort.buildMatch(retTypeChecked, t.UnitType(), cfWhileVal.toVariable,
                // when the while loop returns, we check that the while loop invariant and the
                // postcondition of the top-level function hold
                v => t.and(
                  topLevelPost.map { case s.exprOps.Postcondition(s.Lambda(Seq(postVd), postBody)) =>
                    t.exprOps.replaceFromSymbols(
                      Map(SimpleWhileTransformer.transform(postVd) -> v),
                      SimpleWhileTransformer.transform(postBody)
                    )(t.convertToVal)
                  }.getOrElse(t.BooleanLiteral(true)),
                  optInvChecked.getOrElse(t.BooleanLiteral(true).copiedFrom(wh)),
                ),
                // when the while loop terminates without returning, we check the loop condition
                // is false and that the invariant is true
                _ => t.and(
                  t.Not(getFunctionalResult(condChecked).copiedFrom(cond)).copiedFrom(cond),
                  optInvChecked.getOrElse(t.BooleanLiteral(true).copiedFrom(wh))
                ),
                wh.getPos
              )
            ).copiedFrom(wh)

          val fullBody = t.exprOps.withPostcondition(
            t.exprOps.withPrecondition(
              t.exprOps.withMeasure(newBody, measure).copiedFrom(wh),
              Some(t.andJoin(optInvChecked.toSeq :+ getFunctionalResult(condChecked)))
            ).copiedFrom(wh),
            Some(newPost)
          ).copiedFrom(wh)

          t.LetRec(
            Seq(t.LocalFunDef(id, Seq(), Seq(), loopType.copiedFrom(wh), fullBody, Seq()).copiedFrom(wh)),
            t.IfExpr(
              condChecked,
              t.ApplyLetRec(id, Seq(), tpe, Seq(), Seq()).copiedFrom(wh),
              ControlFlowSort.proceed(retTypeChecked, t.UnitType(), t.UnitLiteral()).copiedFrom(wh)
            ).copiedFrom(wh)
          ).copiedFrom(wh)

        case s.Assert(e, err, rest) =>
          t.Assert(SimpleWhileTransformer.transform(e), err, transform(rest, currentType)).setPos(expr)

        case s.Assume(e, rest) =>
          t.Assume(SimpleWhileTransformer.transform(e), transform(rest, currentType)).setPos(expr)

        case s.Return(e) =>
          ControlFlowSort.ret(
            retTypeChecked,
            SimpleWhileTransformer.transform(currentType),
            SimpleWhileTransformer.transform(e)
          )

        case s.IfExpr(cond, e1, e2)
          if exprHasReturn(e1) || exprHasReturn(e2) =>

          t.IfExpr(SimpleWhileTransformer.transform(cond),
            proceedOrTransform(e1, currentType),
            proceedOrTransform(e2, currentType)
          ).setPos(expr)

        case s.IfExpr(cond, e1, e2) =>
          t.IfExpr(SimpleWhileTransformer.transform(cond),
            transform(e1, currentType),
            transform(e2, currentType)
          ).setPos(expr)

        case s.MatchExpr(scrut, cases)
          if cases.exists {
            case s.MatchCase(_, _, rhs) => exprHasReturn(rhs)
          } =>

          t.MatchExpr(SimpleWhileTransformer.transform(scrut),
            cases.map(proceedOrTransform(_, currentType))
          ).setPos(expr)

        case s.MatchExpr(scrut, cases) =>
          t.MatchExpr(SimpleWhileTransformer.transform(scrut),
            cases.map {
              case mc @ s.MatchCase(pat, optGuard, rhs) =>
                t.MatchCase(
                  SimpleWhileTransformer.transform(pat),
                  optGuard.map(SimpleWhileTransformer.transform),
                  transform(rhs, currentType)
                ).setPos(mc)
            }
          ).setPos(expr)

        case s.Let(vd, e, body) if exprHasReturn(e) =>
          val firstType = vd.tpe
          val firstTypeChecked = SimpleWhileTransformer.transform(firstType)
          val controlFlowVal =
            t.ValDef.fresh("cf",
              ControlFlowSort.controlFlow(retTypeChecked, firstTypeChecked)
            ).setPos(e)
          val vdChecked: t.ValDef = SimpleWhileTransformer.transform(vd)

          t.Let(
            controlFlowVal,
            transform(e, firstType),
            ControlFlowSort.andThen(
              retTypeChecked, firstTypeChecked, SimpleWhileTransformer.transform(currentType),
              controlFlowVal.toVariable,
              (v: t.Variable) =>
                t.exprOps.replaceFromSymbols(
                  Map(vdChecked -> v),
                  proceedOrTransform(body, currentType)
                )(t.convertToVal),
              body.getPos
            )
          ).setPos(expr)

        case s.Let(vd, e, body) =>
          t.Let(
            SimpleWhileTransformer.transform(vd),
            SimpleWhileTransformer.transform(e),
            transform(body, currentType)
          ).setPos(expr)

        case s.LetVar(vd, e, body) if exprHasReturn(e) =>
          val firstType = vd.tpe
          val firstTypeChecked = SimpleWhileTransformer.transform(firstType)
          val controlFlowVal =
            t.ValDef.fresh("cf",
              ControlFlowSort.controlFlow(retTypeChecked, firstTypeChecked)
            ).setPos(e)
          val vdChecked: t.ValDef = SimpleWhileTransformer.transform(vd)

          t.LetVar(
            controlFlowVal,
            transform(e, firstType),
            ControlFlowSort.andThen(
              retTypeChecked, firstTypeChecked, SimpleWhileTransformer.transform(currentType),
              controlFlowVal.toVariable,
              (v: t.Variable) =>
                t.exprOps.replaceFromSymbols(
                  Map(vdChecked -> v),
                  proceedOrTransform(body, currentType)
                )(t.convertToVal),
              body.getPos
            )
          ).setPos(expr)

        case s.LetVar(vd, e, body) =>
          t.LetVar(
            SimpleWhileTransformer.transform(vd),
            SimpleWhileTransformer.transform(e),
            transform(body, currentType)
          ).setPos(expr)

        case s.LetRec(lfds, rest) =>
          t.LetRec(
            lfds.map(lfd => new ReturnTransformer(s.Inner(lfd)).getResult.asInstanceOf[t.Inner].fd),
            transform(rest, currentType)
          ).setPos(expr)

        case s.Block(es, last) =>
          val currentTypeChecked = SimpleWhileTransformer.transform(currentType)

          def processBlockExpressions(es: Seq[s.Expr]): t.Expr = es match {
            case Seq(e) => transform(e, currentType)

            case e +: rest if exprHasReturn(e) =>
              val firstType = e.getType
              val firstTypeChecked = SimpleWhileTransformer.transform(e.getType)
              val controlFlowVal =
                t.ValDef.fresh("cf",
                  ControlFlowSort.controlFlow(retTypeChecked, firstTypeChecked)
                ).setPos(e)
              val transformedRest = processBlockExpressions(rest)

              t.Let(
                controlFlowVal,
                transform(e, firstType),
                ControlFlowSort.andThen(
                  retTypeChecked, firstTypeChecked, currentTypeChecked,
                  controlFlowVal.toVariable,
                  _ => if (rest.exists(exprHasReturn))
                      transformedRest
                    else
                      ControlFlowSort.proceed(retTypeChecked, currentTypeChecked, transformedRest),
                  rest.head.getPos
                )
              ).setPos(e)

            case es =>
              val (nonReturnEs, others) = es.span(e => !exprHasReturn(e))
              val nonReturnsEsChecked = nonReturnEs.map(SimpleWhileTransformer.transform(_))
              if (others.isEmpty)
                t.Block(nonReturnsEsChecked.init, nonReturnsEsChecked.last).copiedFrom(expr)
              else
                t.Block(nonReturnsEsChecked, processBlockExpressions(others)).copiedFrom(expr)
          }
          processBlockExpressions(es :+ last)

        case _ =>
          val (ids, vs, es, tps, flags, recons) = deconstructor.deconstruct(expr)
          val tvs = vs.map(SimpleWhileTransformer.transform).map(_.asInstanceOf[t.Variable])
          val ttps = tps.map(SimpleWhileTransformer.transform)
          val tflags = flags.map(SimpleWhileTransformer.transform)

          val currentTypeChecked = SimpleWhileTransformer.transform(currentType)

          def rec(es: Seq[s.Expr], tes: Seq[t.Expr]): t.Expr = es match {
            case Seq() => recons(ids, tvs, tes, ttps, tflags)
            case e +: rest if !exprHasReturn(e) =>
              // We use a let-binding here to preserve execution order.
              val vd = t.ValDef.fresh("x", SimpleWhileTransformer.transform(e.getType), true).copiedFrom(e)
              t.Let(vd, SimpleWhileTransformer.transform(e), rec(rest, tes :+ vd.toVariable)).copiedFrom(e)
            case e +: rest =>
              val firstType = SimpleWhileTransformer.transform(e.getType)
              ControlFlowSort.andThen(
                retTypeChecked, firstType, currentTypeChecked,
                transform(e, e.getType),
                (v: t.Variable) => {
                  val transformedRest = rec(rest, tes :+ v)
                  if (rest.exists(exprHasReturn))
                    transformedRest
                  else
                    ControlFlowSort.proceed(retTypeChecked, currentTypeChecked, transformedRest)
                },
                e.getPos
              )
          }

          rec(es, Seq.empty)
      }
    }

    new ReturnTransformer(s.Outer(fd)).getResult.asInstanceOf[t.Outer].fd
  }

  override protected def extractSymbols(context: TransformerContext, symbols: s.Symbols): t.Symbols = {
    if (context.exprHasReturn.nonEmpty)
      super.extractSymbols(context, symbols)
        .withSorts(Seq(ControlFlowSort.syntheticControlFlow))
    else
      super.extractSymbols(context, symbols)
  }
}

object ReturnElimination {
  def apply(trees: Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with ReturnElimination
}