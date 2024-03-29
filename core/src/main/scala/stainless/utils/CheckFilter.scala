/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package utils

/** Filter functions for verification purposes. */
trait CheckFilter {
  protected val context: inox.Context
  protected val trees: ast.Trees
  import trees._

  type Path = Seq[String]

  private lazy val pathsOpt: Option[Seq[Path]] = context.options.findOption(optFunctions) map { functions =>
    functions map CheckFilter.fullNameToPath
  }

  private def isInOptions(fid: Identifier): Boolean = pathsOpt match {
    case None => true
    case Some(paths) =>
      // Support wildcard `_` as specified in the documentation.
      // A leading wildcard is always assumes.
      pathsOpt.isEmpty
      val path: Path = CheckFilter.fullNameToPath(CheckFilter.fixedFullName(fid))
      paths exists { p =>
        if (p endsWith Seq("_")) path containsSlice p.init
        else path endsWith p
      }
  }

  private def shouldBeChecked(fid: Identifier, flags: Seq[trees.Flag]): Boolean = {
    val isUnchecked = flags.contains(DropVCs)
    pathsOpt match {
      case None =>
        val isLibrary = flags exists (_.name == "library")
        val isUnchecked = flags contains DropVCs
        !(isLibrary || isUnchecked)

      case Some(paths) => !isUnchecked && isInOptions(fid)
    }
  }

  def filter(ids: Seq[Identifier], symbols: trees.Symbols, component: Component): Seq[Identifier] = {
    def isDerivedFrom(ids: Set[Identifier])(fd: trees.FunDef): Boolean =
      fd.flags.exists {
        case trees.Derived(Some(id)) => ids(id)
        case trees.Derived(None) => true
        case _ => false
      }

    val init = ids.flatMap(id => symbols.lookupFunction(id).toSeq).filter(shouldBeChecked).map(_.id).toSet

    val toCheck = inox.utils.fixpoint { (ids: Set[Identifier]) =>
      ids ++ symbols.functions.values.toSeq
        .filter(isDerivedFrom(ids))
        .filter(shouldBeChecked)
        .map(_.id)
    } (init)

    val toProcess = toCheck.toSeq.sortBy(symbols.getFunction(_).getPos)

    for (id <- toProcess) {
      val fd = symbols.getFunction(id)
      if (fd.flags exists (_.name == "library")) {
        val fullName = fd.id.fullName
        context.reporter.warning(
          s"Component [${component.name}]: Forcing processing of $fullName which was assumed verified")
      }
    }

    toProcess
  }

  /** Checks whether the given function/class should be verified at some point. */
  def shouldBeChecked(fd: FunDef): Boolean = shouldBeChecked(fd.id, fd.flags)

  def isInOptions(fd: FunDef): Boolean = isInOptions(fd.id)

}

object CheckFilter {
  def apply(t: ast.Trees, ctx: inox.Context): CheckFilter {
    val trees: t.type
  } = new CheckFilter {
    override val context = ctx
    override val trees: t.type = t
  }

  type Path = Seq[String]

  def fullNameToPath(fullName: String): Path = (fullName split '.').toSeq

  // TODO this is probably done somewhere else in a cleaner fasion...
  def fixedFullName(id: Identifier): String = id.fullName
    .replace("$bar", "|")
    .replace("$up", "^")
    .replace("$eq", "=")
    .replace("$plus", "+")
    .replace("$minus", "-")
    .replace("$times", "*")
    .replace("$div", "/")
    .replace("$less", "<")
    .replace("$geater", ">")
    .replace("$colon", ":")
    .replace("$amp", "&")
    .replace("$tilde", "~")
}
